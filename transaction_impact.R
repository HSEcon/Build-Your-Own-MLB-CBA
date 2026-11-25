# Transaction analysis uses the existing Models 1–5, never writes to a payroll snapshot.
# Amounts in millions. Salary proxy and CBT adjustment are separate inputs.
tx_rebase <- function(reference, cash, cbt, coef=payroll_effect, attendance_coef=.7638659) {
  n <- nrow(reference)
  stopifnot(n==30, length(cash)==n, length(cbt)==n, all(is.finite(c(cash,cbt))),
    all(cash+reference$Payroll_M>0), all(cbt+reference$CBT_Payroll_M>0),
    is.finite(coef),is.finite(attendance_coef))
  out <- reference
  out$Payroll_M <- reference$Payroll_M+cash
  out$CBT_Payroll_M <- reference$CBT_Payroll_M+cbt
  # Model 1's gross change in spending is an alternative talent proxy, not player hWAR.
  # Model 7 applies that proxy against the matchup schedule. Do not add both estimates.
  raw <- 162*coef*log(out$Payroll_M/reference$Payroll_M)
  f <- data.frame(Team_ID=reference$Team_ID,Expected_Wins_2027=162*reference$Win_Pct,Attendance_2027=reference$Attendance,Revenue_2027=reference$Revenue_Millions)
  if(abs(sum(f$Expected_Wins_2027)-2430)<.01 && setequal(f$Team_ID,names(fo_div))) {
    z <- fo_matchup_change(f,setNames(raw,f$Team_ID))
    dw <- z$Win_Change[match(reference$Team_ID,z$Team)]/162
  } else {
    # Non-162-game / historical reference retains legacy accounting only.
    dw <- (raw-mean(raw))/162
  }
  out$Win_Pct <- reference$Win_Pct+dw
  stopifnot(all(out$Win_Pct>=0 & out$Win_Pct<=1),abs(sum(out$Win_Pct)-sum(reference$Win_Pct))<1e-8)
  out$Attendance <- reference$Attendance*exp(attendance_coef*dw)
  # Model 5A's revenue estimate and baseline growth are already in the saved forecast.
  # Only the incremental Model 5B/C attendance and winning terms are applied.
  out$Revenue_Millions <- reference$Revenue_Millions*exp((.156374*attendance_coef+.141378)*dw)
  out$Model3_Wins_From_25M <- 162*coef*log1p(25/out$Payroll_M)
  out
}

tx_pair <- function(reference, teams, cash, cbt, direction="add",coef=payroll_effect,attendance_coef=.7638659) {
  stopifnot(length(teams)==length(cash),length(teams)==length(cbt),length(teams)>0,
    !anyDuplicated(teams),all(teams %in% reference$Team_ID),all(is.finite(c(cash,cbt))),
    direction %in% c("add","reverse"))
  a <- b <- rep(0,nrow(reference)); i <- match(teams,reference$Team_ID)
  a[i] <- cash; b[i] <- cbt
  # Reverse is a hypothetical without-transaction snapshot; it is not evidence of inclusion.
  if(direction=="reverse") list(before=tx_rebase(reference,-a,-b,coef,attendance_coef),after=reference)
  else list(before=reference,after=tx_rebase(reference,a,b,coef,attendance_coef))
}

tx_team_effects <- function(pair,coef=payroll_effect) {
  b <- pair$before; a <- pair$after
  stopifnot(identical(b$Team_ID,a$Team_ID))
  dp <- a$Payroll_M-b$Payroll_M
  dw <- 162*(a$Win_Pct-b$Win_Pct)
  gross <- 162*coef*log(a$Payroll_M/b$Payroll_M)
  data.frame(Team_ID=b$Team_ID,Club=b$Team_Name,Payroll_Before=b$Payroll_M,Payroll_After=a$Payroll_M,
    Payroll_Change=dp,CBT_Before=b$CBT_Payroll_M,CBT_After=a$CBT_Payroll_M,CBT_Change=a$CBT_Payroll_M-b$CBT_Payroll_M,
    Wins_Before=162*b$Win_Pct,Wins_After=162*a$Win_Pct,Wins_Change=dw,Gross_Wins=gross,
    Revenue_Change=a$Revenue_Millions-b$Revenue_Millions,
    Model2_Revenue_Change=b$Revenue_Millions*expm1(revenue_effect*dw/162),
    Attendance_Change=a$Attendance-b$Attendance,
    Efficiency_Before=(b$Win_Pct/.5)/(b$Payroll_M/mean(b$Payroll_M)),
    Efficiency_After=(a$Win_Pct/.5)/(a$Payroll_M/mean(a$Payroll_M)),
    Cost_Per_Gross_Win=ifelse(dp>0 & gross>1e-10,dp/gross,NA_real_),
    Net_Revenue_Less_Payroll=a$Revenue_Millions-b$Revenue_Millions-dp)
}

tx_league_effects <- function(pair) {
  b <- pair$before; a <- pair$after
  s0 <- sd(162*b$Win_Pct); s1 <- sd(162*a$Win_Pct)
  c(Payroll=sum(a$Payroll_M-b$Payroll_M),CBT=sum(a$CBT_Payroll_M-b$CBT_Payroll_M),
    Gini_Before=gini(b$Payroll_M),Gini_After=gini(a$Payroll_M),Balance=if(s0>0)100*(s0-s1)/s0 else NA_real_,
    Revenue=sum(a$Revenue_Millions-b$Revenue_Millions),Attendance=sum(a$Attendance-b$Attendance))
}

tx_active_rules <- function(p) {
  p <- complete_policy(p)
  if(p$upper_rule=="cap" && p$scenario_year<p$cap_start) {
    p$upper_rule <- "cbt";p$upper_value <- 244;p$upper_tax_rate <- 20
    if(p$lower_rule=="floor")p$lower_rule <- "none"
  }
  p
}

tx_position <- function(p,cbt) {
  p <- tx_active_rules(p)
  room <- if(p$upper_rule=="none")rep(NA_real_,length(cbt))else p$upper_value-cbt
  gap <- if(p$lower_rule=="none")rep(NA_real_,length(cbt))else pmax(p$lower_value-cbt,0)
  tax <- if(p$upper_rule=="cbt")pmax(cbt-p$upper_value,0)*p$upper_tax_rate/100 else rep(0,length(cbt))
  low_tax <- if(p$lower_rule=="tax")gap*p$lower_tax_rate/100 else rep(0,length(cbt))
  list(room=room,gap=gap,tax=tax,low_tax=low_tax,upper=p$upper_rule,lower=p$lower_rule)
}

tx_policy_effects <- function(pair,policies,coef=payroll_effect,response=1) {
  b <- pair$before;a <- pair$after
  rows <- lapply(names(policies),function(nm){
    p <- complete_policy(policies[[nm]])
    m0 <- simulate_policy(p,payroll_coef=coef,behavior_multiplier=response,baseline=b)
    m1 <- simulate_policy(p,payroll_coef=coef,behavior_multiplier=response,baseline=a)
    q0 <- tx_position(p,b$CBT_Payroll_M);q1 <- tx_position(p,a$CBT_Payroll_M)
    t0 <- m0$team;t1 <- m1$team
    # Direct club effects are relative to each simulation's own baseline; add the
    # change between those baselines to compare absolute annual revenue less payroll.
    net <- (a$Revenue_Millions-b$Revenue_Millions)-(a$Payroll_M-b$Payroll_M)+
      t1$Direct_Club_Effect_M-t0$Direct_Club_Effect_M
    list(team=data.frame(Agreement=nm,Team_ID=b$Team_ID,Club=b$Team_Name,
      Upper_Rule=q1$upper,Lower_Rule=q1$lower,Room_Before=q0$room,Room_After=q1$room,
      Floor_Gap_Before=q0$gap,Floor_Gap_After=q1$gap,
      Static_CBT_Change=q1$tax-q0$tax,Static_Low_Tax_Change=q1$low_tax-q0$low_tax,
      Modeled_Payroll_Change=t1$New_Payroll_M-t0$New_Payroll_M,
      Modeled_Wins_Change=t1$Predicted_Wins-t0$Predicted_Wins,
      Modeled_Revenue_Change=a$Revenue_Millions-b$Revenue_Millions+t1$Revenue_Change_M-t0$Revenue_Change_M,
      Modeled_Attendance_Change=a$Attendance-b$Attendance+t1$Predicted_Attendance_Change-t0$Predicted_Attendance_Change,
      Modeled_CBT_Change=t1$CBT_Exposure_M-t0$CBT_Exposure_M,
      Modeled_Low_Tax_Change=t1$Low_Spending_Tax_M-t0$Low_Spending_Tax_M,
      Club_Effect_Change=net),
      league=data.frame(Agreement=nm,Player_Comp_Change=m1$metrics$player_comp-m0$metrics$player_comp,
        Gini_Change=m1$metrics$gini-m0$metrics$gini,
        Balance_Change=if(m0$metrics$win_spread>0)100*(m0$metrics$win_spread-m1$metrics$win_spread)/m0$metrics$win_spread else NA_real_,
        Revenue_Change=sum(a$Revenue_Millions-b$Revenue_Millions)+m1$metrics$revenue_change-m0$metrics$revenue_change,
        Club_Effect_Change=sum(net)))
  })
  list(team=do.call(rbind,lapply(rows,"[[","team")),league=do.call(rbind,lapply(rows,"[[","league")))
}

tx_snapshot <- function(reference,payroll) {
  stopifnot(nrow(payroll)==30,!anyDuplicated(payroll$Team_ID),setequal(payroll$Team_ID,reference$Team_ID))
  i <- match(reference$Team_ID,payroll$Team_ID)
  tx_rebase(reference,payroll$Projected_2027_Payroll_Millions[i]-reference$Payroll_M,
    payroll$Projected_2027_CBT_Payroll_Millions[i]-reference$CBT_Payroll_M)
}

tx_history <- function(reference,path="data/payroll_history") {
  files <- sort(list.files(path,pattern="[.]csv$",full.names=TRUE))
  stopifnot(length(files)>0)
  snapshots <- lapply(files,function(f) {
    p <- read.csv(f,stringsAsFactors=FALSE)
    stopifnot(length(unique(p$Snapshot_Date))==1)
    list(date=as.character(p$Snapshot_Date[1]),file=basename(f),baseline=tx_snapshot(reference,p))
  })
  snapshots <- snapshots[order(vapply(snapshots,"[[",character(1),"date"),vapply(snapshots,"[[",character(1),"file"))]
  snapshots
}

tx_present_value <- function(payments,years,rate) {
  stopifnot(length(payments)==length(years),length(years)>0,all(is.finite(c(payments,years,rate))),
    all(payments>=0),all(years>=0),length(rate)==1,rate> -1)
  sum(payments/(1+rate)^years)
}

tx_contract_catalog <- function(ledger,signings,trades) {
  # The amount column is a suggested experiment, not a verified change to a snapshot.
  d <- ledger[ledger$Obligation_Season==2027, ,drop=FALSE]
  records <- list(hypothetical=list(label="Hypothetical transaction",teams="BAL",cash=25,cbt=25,
    note="Example: add $25M of annual salary and $25M of CBT payroll. Edit either amount.",direction="add"))
  for(i in seq_len(nrow(d))) {
    x <- d[i,];key <- paste("contract",x$Contract_ID,x$Club,x$Obligation_Season,sep=":")
    s <- signings[match(x$Contract_ID,signings$Signing_ID),,drop=FALSE]
    amount <- NA_real_; cb <- NA_real_;direction <- "add"
    note <- "Annual salary or a usable comparison amount has not been reported. Enter your own estimate to test a scenario."
    if(is.finite(x$Incremental_Payroll_M) && is.finite(x$Incremental_CBT_M) && x$Snapshot_Reconciliation!="Unreconciled") {
      amount <- x$Incremental_Payroll_M; cb <- x$Incremental_CBT_M
      note <- "Known difference from the saved contract amount. A zero difference means the obligation is already reflected in that comparison."
    } else if(is.finite(x$Difference_From_Projection_M)) {
      amount <- x$Difference_From_Projection_M;cb <- amount
      note <- "Salary settlement minus published arbitration projection. This assumes the projection was in the reference payroll and the CBT change equals the salary change; neither is confirmed."
    } else if(is.finite(x$Salary_Change_From_Prior_M)) {
      amount <- x$Salary_Change_From_Prior_M;cb <- amount
      note <- "Year-over-year salary difference used as a what-if adjustment. Prior salary is not necessarily in the projected payroll; equal CBT change is assumed."
    } else if(is.finite(x$Salary_M) && !x$Transaction_Type %in% c("Trade","Non-tender","Club option","Mutual option declined")) {
      amount <- x$Salary_M;cb <- if(is.finite(x$CBT_Charge_M))x$CBT_Charge_M else amount
      note <- "Reported annual salary compared with a hypothetical zero-salary alternative. No replacement salary is assumed. The CBT amount is assumed equal to annual salary unless verified."
    } else if(x$Transaction_Type=="Free agency" && nrow(s)==1 && is.finite(s$Nominal_AAV_Millions)) {
      amount <- s$Nominal_AAV_Millions;cb <- amount
      note <- "Reported average annual value used only as an illustrative flat annual salary and CBT change; the actual annual schedule and CBT charge are unknown."
    }
    records[[key]] <- list(label=paste(x$Player,x$Club,"—",x$Transaction_Type),teams=x$Club,cash=amount,cbt=cb,
      note=paste(note,"This experiment never changes the saved payroll."),direction=direction)
  }
  # Include new signings before their annual contract schedule is available.
  for(i in seq_len(nrow(signings))) {
    s <- signings[i,]
    if(is.na(s$New_Team_ID)||!s$New_Team_ID %in% base$Team_ID) next
    if(any(d$Contract_ID==s$Signing_ID & d$Club==s$New_Team_ID)) next
    known <- is.finite(s$Payroll_2027_Impact_Millions) && is.finite(s$CBT_2027_Impact_Millions)
    amount <- if(known)s$Payroll_2027_Impact_Millions else s$Nominal_AAV_Millions
    records[[paste0("signing:",s$Signing_ID)]] <- list(label=paste(s$Player,s$New_Team_ID,"— Free agency"),
      teams=s$New_Team_ID,cash=amount,cbt=if(known)s$CBT_2027_Impact_Millions else amount,direction="add",
      note=if(known)"Saved annual impact inputs; this is a what-if, not an additional payroll update." else "Average annual value used only as a hypothetical annual salary and CBT change. Verify the annual contract schedule.")
  }
  # Full trade: only auto-fill if every movement's annual salary transfer is known.
  for(id in unique(trades$Trade_ID[trades$Payroll_Season=="2027"])) {
    x <- trades[trades$Trade_ID==id & trades$Payroll_Season=="2027",,drop=FALSE]
    clubs <- sort(unique(c(x$From_Team_ID,x$To_Team_ID)))
    if(length(clubs)!=2)next
    dc <- dt <- rep(NA_real_,2)
    note <- "Complete trade selected. Some annual salary or retained salary figures are unknown. Enter each team's estimated net salary and CBT change. No full-deal total is assumed."
    if(all(is.finite(x$From_Club_Salary_Change_Millions)) && all(is.finite(x$To_Club_Salary_Change_Millions))) {
      dc <- vapply(clubs,function(k)sum(x$From_Club_Salary_Change_Millions[x$From_Team_ID==k])+sum(x$To_Club_Salary_Change_Millions[x$To_Team_ID==k]),numeric(1))
      if(all(is.finite(x$From_Club_CBT_Change_Millions))&&all(is.finite(x$To_Club_CBT_Change_Millions))) {
        dt <- vapply(clubs,function(k)sum(x$From_Club_CBT_Change_Millions[x$From_Team_ID==k])+sum(x$To_Club_CBT_Change_Millions[x$To_Team_ID==k]),numeric(1))
      }
      note <- "Reported full-deal salary changes; CBT remains unknown if not reported. This is a before-and-after scenario, not an automatic payroll update."
    }
    records[[paste0("trade:",id)]] <- list(label=paste(x$Transaction_Date[1],paste(clubs,collapse=" ↔ "),paste(x$Player,collapse=", "),sep=" — "),
      teams=clubs,cash=dc,cbt=dt,note=note,direction="add")
    # Offer individual known salary as a transparent partial-trade experiment.
    for(j in seq_len(nrow(x))) {
      q <- x[j,]; linked <- d[d$Player==q$Player & d$Club==q$To_Team_ID & d$Transaction_Type=="Trade",,drop=FALSE]
      if(nrow(linked)!=1 || !is.finite(linked$Salary_M)) next
      sal <- linked$Salary_M
      records[[paste0("movement:",q$Movement_ID)]] <- list(label=paste(q$Player,"— single-player trade scenario"),
        teams=c(q$From_Team_ID,q$To_Team_ID),cash=c(-sal,sal),cbt=c(-sal,sal),direction="add",
        note="Partial trade only: reported annual salary with no retained salary and equal CBT transfer assumed. Other players and cash in the deal are excluded; edit amounts to test retention.")
    }
  }
  records
}

# Isolate the effect of a single recorded signing by removing its applied
# adjustments from the latest live scenario. Never add the same deal again.
# Missing amounts remain unknown; calculated wins are conditional on known
# adjustments only, not a full-player valuation or a schedule forecast.
tx_recorded_signing_effect <- function(reference, signing, events, coef=payroll_effect, attendance_coef=.7638659) {
  stopifnot(nrow(signing)==1L)
  ids <- unique(na.omit(c(as.character(signing$New_Team_ID),as.character(signing$Previous_Team_ID))))
  ids <- ids[nzchar(ids)]
  stopifnot(length(ids)>0,all(ids %in% reference$Team_ID))
  e <- events[events$Effective_Season==2027 & events$Event_ID==signing$Signing_ID,,drop=FALSE]
  # A non-tender may precede a new signing. Show that earlier departure as
  # already reflected, never apply its reduction a second time.
  idx <- match(ids,e$Club)
  cash <- ifelse(!is.na(idx) & e$Payroll_Status[idx]!="Pending",e$New_Payroll_M[idx]-e$Prior_Payroll_M[idx],NA_real_)
  cbt <- ifelse(!is.na(idx) & e$CBT_Status[idx]=="Confirmed",e$New_CBT_M[idx]-e$Prior_CBT_M[idx],NA_real_)
  statuses <- ifelse(is.na(idx),"Not recorded",e$Payroll_Status[idx])
  proxy <- !is.na(idx) & grepl("nominal AAV spending proxy ONLY",e$Notes[idx],fixed=TRUE)
  statuses[proxy] <- "Provisional AAV proxy"
  if(signing$Status=="Withdrawn") {cash[] <- NA_real_;cbt[] <- NA_real_;statuses[] <- "Withdrawn"}
  other_exit <- events[events$Effective_Season==2027 & events$Player==signing$Player &
    grepl("^2026-nt-",events$Event_ID) & events$Payroll_Status!="Pending",,drop=FALSE]
  prior_exit <- ids!=signing$New_Team_ID & ids %in% other_exit$Club & is.na(cash)
  statuses[prior_exit] <- "Prior non-tender; not counted again"
  known <- is.finite(cash) | is.finite(cbt)
  result <- data.frame(Team_ID=ids,Role=ifelse(ids==signing$New_Team_ID,"Signing club","Previous club"),
    Payroll_Change=cash,CBT_Change=cbt,Payroll_Status=statuses,Conditional_Wins=NA_real_)
  league <- NULL
  if(any(is.finite(cash))) {
    dc <- ifelse(is.finite(cash),cash,0)
    dt <- ifelse(is.finite(cbt),cbt,0)
    pair <- tx_pair(reference,ids,dc,dt,"reverse",coef,attendance_coef)
    league <- tx_team_effects(pair,coef)
    result$Conditional_Wins <- league$Wins_Change[match(ids,league$Team_ID)]
  }
  list(clubs=result,league=league,partial=any(!is.finite(cash)),any_applied=any(known))
}
