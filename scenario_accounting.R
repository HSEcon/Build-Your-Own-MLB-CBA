# Scenario accounting in millions of dollars. Missing player-level data are explicit assumptions.
policy_assumptions <- function() {
  list(media_share_pct=100, media_revenue_pct=20, media_start=2027,
       cap_start=2027, scenario_year=2027, min_players_under2=12,
       min_players_2plus=5, arbitration_payroll=22,
       arbitration_bonus_pct=0, arbitration_bonus_start=2028,
       veteran_bonus=0, super_two_pct=22, arb_candidates=4, arb_raise=2,
       prearb_min_bonus=0, prearb_eligible=12,
       min_arb_salary=0, arb_floor_players=3, arb_floor_baseline=2,
       attendance_coef=0.7638659, revenue_growth=0.047417, payroll_coef=0.0273665)
}
complete_policy <- function(p) modifyList(policy_assumptions(), p)
projection_baseline <- function(historical, forecasts, payroll) {
  stopifnot(nrow(forecasts)==30,nrow(payroll)==30,
    !anyDuplicated(forecasts$Team_ID),!anyDuplicated(payroll$Team_ID),
    setequal(historical$Team_ID,forecasts$Team_ID),setequal(historical$Team_ID,payroll$Team_ID),
    length(unique(forecasts$Snapshot_Date))==1,
    identical(as.character(unique(forecasts$Snapshot_Date)),as.character(unique(payroll$Snapshot_Date))))
  i <- match(historical$Team_ID,forecasts$Team_ID)
  j <- match(historical$Team_ID,payroll$Team_ID)
  stopifnot(all(abs(forecasts$Projected_2027_Payroll_Millions[i]-payroll$Latest_Payroll_Millions[j])<1e-6))
  out <- historical
  out$Payroll_M <- forecasts$Projected_2027_Payroll_Millions[i]
  out$CBT_Payroll_M <- payroll$Latest_CBT_Millions[j]
  out$Win_Pct <- forecasts$Expected_Wins_2027[i]/162
  out$Revenue_Millions <- forecasts$Revenue_2027[i]
  out$Attendance <- forecasts$Attendance_2027[i]
  stopifnot(all(is.finite(out$Payroll_M)),all(out$Payroll_M>0),
    all(is.finite(out$CBT_Payroll_M)),all(out$CBT_Payroll_M>0),
    all(is.finite(out$Revenue_Millions)),all(out$Revenue_Millions>0),
    all(is.finite(out$Attendance)),all(out$Attendance>0),
    abs(sum(out$Win_Pct)-15)<1e-6)
  out$Model3_Wins_From_25M <- 162*payroll_effect*log1p(25/out$Payroll_M)
  out
}
policy_transfers <- function(p,baseline) {
  p <- complete_policy(p)
  active <- p$central_media && p$scenario_year>=p$media_start
  media <- baseline$Revenue_Millions*p$media_revenue_pct/100
  pooled <- if(active) media*p$media_share_pct/100 else rep(0,nrow(baseline))
  media_transfer <- mean(pooled)-pooled
  ordinary_contribution <- (baseline$Revenue_Millions-pooled)*p$revenue_sharing/100
  ordinary_transfer <- mean(ordinary_contribution)-ordinary_contribution
  list(media=media_transfer,ordinary=ordinary_transfer,net=media_transfer+ordinary_transfer)
}
player_pay_costs <- function(p,n=30) {
  p <- complete_policy(p)
  minimum <- p$min_players_under2*max(0,p$minimum_salary_under2-.78)+
    p$min_players_2plus*max(0,p$minimum_salary_2plus-.78)
  eligibility <- if(p$arbitration=="2") 100 else p$super_two_pct
  # Newly eligible players are separate from already arbitration-eligible floor recipients.
  earlier_arb <- p$arb_candidates*max(0,eligibility-22)/100*max(p$arb_raise,p$min_arb_salary-.78)
  arb_floor <- p$arb_floor_players*max(0,p$min_arb_salary-p$arb_floor_baseline)
  arb <- if(p$scenario_year>=p$arbitration_bonus_start) p$arbitration_payroll*p$arbitration_bonus_pct/100 else 0
  list(minimum=rep(minimum,n),earlier_arb=rep(earlier_arb,n),arb_floor=rep(arb_floor,n),arbitration_bonus=rep(arb,n),
    veteran_bonus=rep(p$veteran_bonus,n),prearb_delta=rep((p$prearb_pool-50)/n,n))
}
contract_delta <- function(new,already_in_snapshot) {
  if(is.na(new)||is.na(already_in_snapshot)) return(NA_real_)
  new-already_in_snapshot
}
contract_ledger <- function(path="data/annual_contracts.csv") {
  d <- read.csv(path,stringsAsFactors=FALSE,na.strings=c("","NA"))
  required <- c("Contract_ID","Player","Club","Transaction_Type","Signed_Date","Effective_Season",
    "Obligation_Season","Salary_M","Cash_Payment_M","CBT_Charge_M","Baseline_Salary_M",
    "Baseline_CBT_M","Snapshot_Reconciliation","Status","Source_URL","Notes",
    "Prior_Salary_M","Published_Projection_M","Signing_Bonus_M","Deferred_Earnings_M","Deferred_Payment_M","Option_M","Buyout_M","Retained_Salary_M")
  stopifnot(all(required %in% names(d)),!anyDuplicated(d[c("Contract_ID","Club","Obligation_Season")]))
  for(field in c("Salary_M","Cash_Payment_M","CBT_Charge_M","Baseline_Salary_M","Baseline_CBT_M","Prior_Salary_M","Published_Projection_M","Signing_Bonus_M","Deferred_Earnings_M","Deferred_Payment_M","Option_M","Buyout_M","Retained_Salary_M"))
    stopifnot(all(is.na(d[[field]])|(is.finite(d[[field]])&d[[field]]>=0)))
  stopifnot(all(d$Obligation_Season>=d$Effective_Season),
    all(d$Snapshot_Reconciliation %in% c("Included","Not included","Unreconciled")))
  d$Salary_Change_From_Prior_M <- mapply(contract_delta,d$Salary_M,d$Prior_Salary_M)
  d$Difference_From_Projection_M <- mapply(contract_delta,d$Salary_M,d$Published_Projection_M)
  d$Incremental_Payroll_M <- mapply(contract_delta,d$Salary_M,d$Baseline_Salary_M)
  d$Incremental_CBT_M <- mapply(contract_delta,d$CBT_Charge_M,d$Baseline_CBT_M)
  d$Incremental_Payroll_M[d$Snapshot_Reconciliation=="Unreconciled"] <- NA_real_
  d$Incremental_CBT_M[d$Snapshot_Reconciliation=="Unreconciled"] <- NA_real_
  d
}

# Applied scenario adjustments are a separate view, not contract reconciliation proof.
# Match on stable contract/event ID, club and season, never player name.
contract_applied_view <- function(ledger, events) {
  key <- function(id,club,year) paste(id,club,year,sep="|")
  k <- key(events$Event_ID,events$Club,events$Effective_Season)
  stopifnot(!anyDuplicated(k))
  i <- match(key(ledger$Contract_ID,ledger$Club,ledger$Obligation_Season),k)
  cash <- events$Payroll_Status[i]; cbt <- events$CBT_Status[i]
  ledger$Applied_Payroll_M <- ifelse(!is.na(cash)&cash!="Pending",events$New_Payroll_M[i]-events$Prior_Payroll_M[i],NA_real_)
  ledger$Applied_CBT_M <- ifelse(!is.na(cbt)&cbt=="Confirmed",events$New_CBT_M[i]-events$Prior_CBT_M[i],NA_real_)
  ledger$Applied_Status <- ifelse(is.na(cash),"Not reviewed",ifelse(cash=="Pending","Pending reconciliation",paste(cash,"scenario adjustment")))
  ledger$Applied_CBT_Status <- ifelse(is.na(cbt),"Not reviewed",cbt)
  ledger$Adjustment_Notes <- events$Notes[i]
  ledger
}

# Rebuild the reactionary scenario from reviewed facts, not a roster census.
# Zero is a MODEL assumption only for new-club arrivals or known departures;
# unknown annual salary, retained obligations and CBT are never filled with zero.
incremental_event_view <- function(events, signings, ledger) {
  original <- events
  stopifnot(!anyDuplicated(events[c("Event_ID","Club","Effective_Season")]),
            !anyDuplicated(signings$Signing_ID))
  can_apply <- function(i) {
    events$Payroll_Status[i]=="Pending" &&
      !any(seq_len(nrow(events))!=i & events$Player==events$Player[i] &
        events$Club==events$Club[i] & events$Effective_Season==events$Effective_Season[i] &
        events$Payroll_Status!="Pending")
  }
  apply_delta <- function(i, prior, new, note) {
    stopifnot(is.finite(prior),is.finite(new),prior>=0,new>=0)
    events$Prior_Payroll_M[i] <<- prior
    events$New_Payroll_M[i] <<- new
    events$Payroll_Status[i] <<- "Provisional"
    events$Notes[i] <<- paste(original$Notes[i],"Incremental scenario:",note)
  }
  for (k in seq_len(nrow(signings))) {
    s <- signings[k,]
    if(s$Status=="Withdrawn") next
    i <- which(events$Event_ID==s$Signing_ID & events$Club==s$New_Team_ID & events$Effective_Season==2027)
    if(length(i)!=1L || !can_apply(i)) next
    row <- ledger[ledger$Contract_ID==s$Signing_ID & ledger$Club==s$New_Team_ID &
      ledger$Obligation_Season==2027 & ledger$Transaction_Type=="Free agency",,drop=FALSE]
    if(nrow(row)!=1L || row$Status %in% c("Contingent","Conflicting salary reports")) next
    different_club <- !is.na(s$Previous_Team_ID) && nzchar(s$Previous_Team_ID) && s$Previous_Team_ID!=s$New_Team_ID
    overseas <- (is.na(s$Previous_Team_ID)||!nzchar(s$Previous_Team_ID)) &&
      grepl("NPB|Japan",row$Notes,ignore.case=TRUE)
    if((different_club||overseas) && is.na(row$Salary_M)) {
      if(grepl("Reviewed nominal-AAV model proxy",original$Notes[i],fixed=TRUE) &&
        is.finite(s$Total_Value_Millions) && is.finite(s$Years) && s$Years>0) {
        apply_delta(i,0,s$Total_Value_Millions/s$Years,"nominal AAV spending proxy ONLY; not reported annual cash, salary or CBT. Replaced when annual salary becomes known.")
      }
      next
    }
    if(is.na(row$Salary_M)) next
    if(different_club||overseas) {
      apply_delta(i,0,row$Salary_M,"reported annual salary; assumes player was absent from acquiring club's historical anchor. Cash timing and CBT remain unverified.")
    } else if(!is.na(row$Prior_Salary_M)) {
      apply_delta(i,row$Prior_Salary_M,row$Salary_M,"reported old and new annual salary; assumes old amount included in anchor.")
    }
  }
  # A known old salary is usable for a provisional departure, not a claim that
  # all residual obligations are known. A prior non-tender supersedes a second exit.
  for (k in seq_len(nrow(signings))) {
    s <- signings[k,]
    if(s$Status=="Withdrawn" || is.na(s$Previous_Team_ID) ||
      !nzchar(s$Previous_Team_ID) || s$Previous_Team_ID==s$New_Team_ID) next
    i <- which(events$Event_ID==s$Signing_ID & events$Club==s$Previous_Team_ID & events$Effective_Season==2027)
    if(length(i)!=1L || !can_apply(i) || is.na(events$Prior_Payroll_M[i])) next
    nt <- ledger$Player==s$Player & ledger$Club==s$Previous_Team_ID &
      ledger$Transaction_Type=="Non-tender" & ledger$Obligation_Season==2027
    if(any(nt)) next
    apply_delta(i,events$Prior_Payroll_M[i],0,"reported former-club annual salary; assumes inclusion in anchor and no remaining modeled salary for the departing player. Residual payments and CBT unknown.")
  }
  for(k in seq_len(nrow(ledger))) {
    r <- ledger[k,]
    if(r$Obligation_Season!=2027 || !r$Transaction_Type %in% c("Arbitration settlement","Extension","Non-tender")) next
    i <- which(events$Event_ID==r$Contract_ID & events$Club==r$Club & events$Effective_Season==2027)
    if(length(i)!=1L || !can_apply(i) || is.na(r$Prior_Salary_M)) next
    if(r$Transaction_Type=="Non-tender" && r$Status=="Non-tendered") {
      apply_delta(i,r$Prior_Salary_M,0,"reported prior salary; assumes inclusion in anchor and no residual modeled salary after non-tender. CBT unknown.")
    } else if(!is.na(r$Salary_M) && !r$Status %in% c("Contingent","Conflicting salary reports") &&
      !grepl("not (on|a comparable).*opening.day|September waiver|September claim",r$Notes,ignore.case=TRUE)) {
      apply_delta(i,r$Prior_Salary_M,r$Salary_M,"reported annual salary change; assumes prior salary included in historical anchor. Cash timing and CBT unknown.")
    }
  }
  stopifnot(identical(events$Prior_CBT_M,original$Prior_CBT_M),
            identical(events$New_CBT_M,original$New_CBT_M),
            identical(events$CBT_Status,original$CBT_Status))
  events
}
read_incremental_events <- function(path="data/live_payroll_adjustments.csv",
                                     signing_path="data/free_agency_input.csv",
                                     ledger_path="data/annual_contracts.csv") {
  raw <- read.csv(path,stringsAsFactors=FALSE,na.strings=c("","NA"))
  signings <- read.csv(signing_path,stringsAsFactors=FALSE,na.strings=c("","NA"))
  incremental_event_view(raw,signings,contract_ledger(ledger_path))
}
