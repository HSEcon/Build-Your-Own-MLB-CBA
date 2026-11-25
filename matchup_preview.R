# Private preview. Matchup-based incremental scenarios; no deployment hooks.
fo_divisions <- list("AL East"=c("BAL","BOS","NYY","TBR","TOR"),
 "AL Central"=c("CHW","CLE","DET","KCR","MIN"),
 "AL West"=c("ATH","HOU","LAA","SEA","TEX"),
 "NL East"=c("ATL","MIA","NYM","PHI","WSH"),
 "NL Central"=c("CHC","CIN","MIL","PIT","STL"),
 "NL West"=c("ARI","COL","LAD","SDP","SFG"))
fo_div <- setNames(rep(names(fo_divisions),each=5),unlist(fo_divisions,use.names=FALSE))
fo_synthetic_schedule <- function(ids) {
  teams <- names(fo_div); stopifnot(setequal(teams,ids))
  rows <- list(); k <- 1L
  for(i in seq_len(29)) for(j in (i+1):30) {
    n <- if((i-1)%/%5==(j-1)%/%5) 13L else if((i-1)%/%15==(j-1)%/%15) 6L+as.integer((i-j)%%5 %in% c(0,1)) else 3L+as.integer((i-j)%%15==0)
    h <- n%/%2+as.integer(n%%2==1 && (i+j)%%2==0)
    rows[[k]]<-data.frame(Home=teams[i],Away=teams[j],Games=h);k<-k+1L
    rows[[k]]<-data.frame(Home=teams[j],Away=teams[i],Games=n-h);k<-k+1L
  }
  fo_validate_schedule(do.call(rbind,rows),ids)
}
fo_matchup_change <- function(f, talent, payroll=setNames(numeric(),character()), ratings=NULL) {
  ids<-as.character(f$Team_ID); stopifnot(length(ids)==30,setequal(ids,names(fo_div)),all(names(talent)%in%ids),all(is.finite(talent)),!anyDuplicated(names(talent)),all(names(payroll)%in%ids),all(is.finite(payroll)),!anyDuplicated(names(payroll)))
  s<-fo_synthetic_schedule(ids)
  r<-if(is.null(ratings)) fo_calibrate(ids,f$Expected_Wins_2027) else ratings[ids]
  before<-fo_schedule_wins(s,r)
  rnew<-r
  for(t in names(talent)) {
    if(talent[[t]]==0) next
    target<-before[[t]]+talent[[t]]
    if(target<=0||target>=162) stop("Requested talent change implies an impossible record.")
    fun<-function(a) {rr<-r;rr[t]<-rr[t]+a;fo_schedule_wins(s,rr)[[t]]-target}
    rnew[t]<-r[t]+uniroot(fun,c(-20,20),tol=1e-10)$root
  }
  after<-fo_schedule_wins(s,rnew)
  change<-as.numeric(after[ids]-before[ids]);base<-as.numeric(before[ids]);final<-as.numeric(after[ids])
  stopifnot(abs(sum(final)-2430)<1e-6,abs(sum(change))<1e-6,all(final>=0&final<=162))
  dw<-change/162;att<-f$Attendance_2027*expm1(.7638659*dw)
  rev<-f$Revenue_2027*expm1((.156374*.7638659+.141378)*dw)
  pay<-setNames(rep(0,30),ids);pay[names(payroll)]<-payroll
  data.frame(Team=ids,Division=unname(fo_div[ids]),Before_Wins=base,After_Wins=final,Win_Change=change,Before_Losses=162-base,After_Losses=162-final,Loss_Change=-change,Attendance_Change=att,Revenue_Change_M=rev,Payroll_Change_M=as.numeric(pay),Direct_Effect_M=rev-as.numeric(pay))
}

# Rebuild the private Model 5 financial forecast from the saved payroll inputs.
# Actual 2026 win percentages are normalized only for the 162-game matchup baseline.
# Preserve the original data file so Phase 1 live data jobs cannot be affected.
fo_financial_forecast <- function(f, coef=0.0273665) {
  required <- c("Team_ID","Payroll_2026_Millions","Projected_2027_Payroll_Millions",
    "Win_Pct_2026","Attendance_2026","Revenue_2026","Expected_Wins_2027")
  stopifnot(all(required %in% names(f)),nrow(f)==30,setequal(f$Team_ID,names(fo_div)),
    all(is.finite(f$Payroll_2026_Millions)),all(f$Payroll_2026_Millions>0),
    all(is.finite(f$Projected_2027_Payroll_Millions)),all(f$Projected_2027_Payroll_Millions>0),
    all(is.finite(f$Win_Pct_2026)),all(is.finite(f$Attendance_2026)),all(is.finite(f$Revenue_2026)))
  ids <- as.character(f$Team_ID)
  base_wins <- 162*f$Win_Pct_2026
  base_wins <- base_wins + (2430-sum(base_wins))/30
  base <- data.frame(Team_ID=ids,Expected_Wins_2027=base_wins,
    Attendance_2027=f$Attendance_2026,Revenue_2027=f$Revenue_2026)
  raw <- 162*coef*log(f$Projected_2027_Payroll_Millions/f$Payroll_2026_Millions)
  # Remove the median log change as a common spending trend; no equal win-loss allocation.
  raw <- raw-median(raw)
  z <- fo_matchup_change(base,setNames(raw,ids))
  f$Expected_Wins_2027 <- z$After_Wins[match(ids,z$Team)]
  f$Win_Pct_2027 <- f$Expected_Wins_2027/162
  f$Win_Pct_Change <- f$Win_Pct_2027-f$Win_Pct_2026
  f$Attendance_2027 <- f$Attendance_2026*exp(.7638659*f$Win_Pct_Change)
  f$Attendance_Change_Pct <- 100*(f$Attendance_2027/f$Attendance_2026-1)
  f$Revenue_2027 <- f$Revenue_2026*exp(.047417+.156374*log(f$Attendance_2027/f$Attendance_2026)+.141378*f$Win_Pct_Change)
  f$Revenue_Change_Pct <- 100*(f$Revenue_2027/f$Revenue_2026-1)
  f$Forecast_Status <- paste(f$Forecast_Status,"| Private preview: schedule-adjusted payroll proxy; synthetic matchups")
  stopifnot(abs(sum(f$Expected_Wins_2027)-2430)<1e-6)
  f
}


# Multi-year trade value is separate from the single-season economic comparison.
# These are transparent, editable scenario assumptions, not fitted aging curves.
# Illustrative, not fitted: age affects both conditional production and probability of remaining MLB-active.
# Age tiers are scenario defaults, NOT estimates of actual contractual control.
fo_control_example <- function(age) if(age<=25) 6L else if(age<=29) 4L else if(age<=34) 3L else 1L
fo_asset_path <- function(war,age,years,peak=27,growth=.25,decline=.35,upside=.25) {
  stopifnot(length(war)==1L,length(age)==1L,is.finite(war),is.finite(age),
    length(years)==1L,is.finite(years),years>=1,years<=10,years==floor(years),
    all(is.finite(c(peak,growth,decline,upside))),growth>=0,decline>=0,upside>=0)
  ages<-age+seq_len(years)-1
  next_age<-ages[-1]
  # Accelerating aging penalty, not a constant subtraction. Older players lose more.
  step<-ifelse(next_age<=peak,growth*(1+.12*pmax(0,peak-next_age)),
      -decline*(1+.15*pmax(0,next_age-peak)+.015*pmax(0,next_age-peak)^2))
  # Additional upside accumulates before peak and persists into later seasons.
  extra<-upside*exp(-.18*pmax(0,next_age-peak)^2)
  # Conditional probability of remaining MLB-active; conditional on the first projected year.
  # 4% annual minimum exit risk; rises steeply at advanced ages. These are editable-model
  # assumptions in the sense that peak/decline are user inputs, not empirical probabilities.
  hazard<-pmin(.90,.04+.0035*pmax(0,next_age-29)^2)
  survival<-c(1,cumprod(1-hazard))
  raw<-war+c(0,cumsum(step));hi<-war+c(0,cumsum(step+extra))
  # Beyond the first year assume teams can replace below-replacement production.
  expected<-c(war,pmax(0,raw[-1]))*survival
  high<-c(war,pmax(0,hi[-1]))*survival
  data.frame(Year=2026+seq_len(years),Age=ages,Expected=expected,Upside=high,Active_Probability=survival)
}
fo_asset_salary <- function(s,years) {
  if(is.null(s)||!nzchar(trimws(s)))return(rep(NA_real_,years))
  x<-suppressWarnings(as.numeric(trimws(strsplit(s,",",fixed=TRUE)[[1]])))
  if(length(x)!=years||any(!is.finite(x)|x<0))return(rep(NA_real_,years))
  x
}
fo_asset_surplus <- function(path,salary,market,discount) {
  stopifnot(nrow(path)==length(salary),is.finite(market),market>=0,is.finite(discount),discount>=0)
  if(any(!is.finite(salary)|salary<0))return(c(Salary=NA_real_,Expected=NA_real_,Upside=NA_real_))
  df<-(1+discount)^seq_len(nrow(path))
  c(Salary=sum(salary/df),Expected=sum((market*path$Expected-salary)/df),Upside=sum((market*path$Upside-salary)/df))
}
fo_asset_ui <- function(prefix) {
  k<-function(x)paste0(prefix,x)
  fo_panel(h3("Long-term trade value"),p("A younger, inexpensive player with several years of team control can be more valuable than a veteran with greater immediate production. Compare future wins and discounted contract surplus separately from the 2027 impact above."),
    fo_note("Control defaults are illustrative age-tier scenarios (6 years through age 25, 4 through 29, 3 through 34, 1 thereafter), NOT actual service time or contract facts. Edit each player. Salary schedules must be entered or verified; missing salary or prospect value is not zero."),
    div(class="fo-controls",numericInput(k("market"),"Market value per hWAR ($M)",8,min=0,max=50,step=.5),
      numericInput(k("discount"),"Discount rate (%)",5,min=0,max=30,step=.5),numericInput(k("peak"),"Assumed peak age",27,min=20,max=35,step=1),numericInput(k("horizon"),"Display horizon (seasons)",10,min=1,max=10,step=1)),
    div(class="fo-controls",numericInput(k("growth"),"Annual pre-peak growth (hWAR)",.25,min=0,max=2,step=.05),
      numericInput(k("decline"),"Base post-peak decline (hWAR)",.35,min=0,max=2,step=.05),
      numericInput(k("upside"),"Additional development upside (hWAR)",.25,min=0,max=2,step=.05)),
    uiOutput(k("inputs")),uiOutput(k("coverage")),
    h4("Annual future wins by club"),plotlyOutput(k("future_wins"),height="400px"),
    h4("Cumulative future wins by club"),plotlyOutput(k("cumulative_wins"),height="365px"),
    h4("Player career scenarios (irrespective of control)"),plotlyOutput(k("career_paths"),height="365px"),
    p(class="fo-small","Annual and cumulative incremental wins versus retaining traded players, holding 2027 club strengths fixed. Solid = base scenario; dashed = development upside. Club impact ends at assumed control expiry; individual career scenarios continue independently. Aging accelerates after peak and MLB-exit risk increases with age. After the initial year, below-replacement talent can be replaced. These are illustrative assumptions, not fitted probabilities or standings forecasts. Unknown prospects are excluded."),
    h4("Player contract surplus"),DTOutput(k("players")),h4("Net trade surplus by club"),DTOutput(k("clubs")),
    p(class="fo-small","Discounted surplus = expected hWAR × assumed market dollars per win minus the receiving club's annual salary, discounted to the present. Enter one salary in millions for each controlled year, separated by commas. The aging and market assumptions are not statistically fitted; the upside curve is not a probability bound. Net surplus is withheld if any player or salary schedule is missing."))
}
fo_asset_server <- function(input,output,session,prefix,moves,economic_forecast,ratings) {
  k<-function(x)paste0(prefix,x)
  eligible<-reactive({
    x<-moves();x$Known<-is.finite(x$WAR)&is.finite(x$Age)&!is.na(x$ID)&!is.na(x$From)&!is.na(x$To)&x$From%in%names(fo_div)&x$To%in%names(fo_div)&x$From!=x$To
    x$Key<-paste0(gsub("[^a-zA-Z0-9]","",ifelse(is.na(x$ID),"missing",x$ID)),"_",x$From,"_",x$To,"_",seq_len(nrow(x)))
    x
  })
  output[[k("inputs")]]<-renderUI({
    x<-eligible();x<-x[which(x$Known),,drop=FALSE]
    if(!nrow(x))return(p("Select a trade with at least one player with a usable MLB projection."))
    tagList(lapply(seq_len(nrow(x)),function(i) {
      z<-x[i,];div(class="fo-panel",h4(paste0(z$Player," · ",z$From," → ",z$To)),
        div(class="fo-controls",numericInput(k(paste0("years_",z$Key)),"Assumed remaining control (years)",fo_control_example(z$Age),min=1,max=10,step=1),
          textInput(k(paste0("salary_",z$Key)),"Receiving club salary by year ($M)","",placeholder="e.g. 1, 4, 7"),
          p(paste("Age",round(z$Age),"· Starting hWAR",fo_num(z$WAR,1)))))
    }))
  })
  calc<-reactive({
    x<-eligible();validate(need(nrow(x)>0,"Select a trade first."))
    known<-which(x$Known);validate(need(length(known)>0,"No players have sufficient MLB projection data."))
    pars<-lapply(c("market","discount","peak","growth","decline","upside"),function(n)input[[k(n)]]);req(all(lengths(pars)==1L));v<-as.numeric(unlist(pars))
    validate(need(all(is.finite(v))&&all(v>=0),"Use nonnegative, finite scenario assumptions."))
    ages<-list();career<-list();summ<-list()
    horizon<-input[[k("horizon")]];req(length(horizon)==1L);validate(need(is.finite(horizon)&&horizon>=1&&horizon<=10&&horizon==floor(horizon),"Choose 1–10 display seasons."))
    for(i in known) {
      z<-x[i,];n<-input[[k(paste0("years_",z$Key))]];req(!is.null(n))
      validate(need(is.finite(n)&&n>=1&&n<=10&&n==floor(n),"Control must be a whole number from 1 to 10 years."))
      y<-fo_asset_path(z$WAR,z$Age,n,v[3],v[4],v[5],v[6])
      salary<-fo_asset_salary(input[[k(paste0("salary_",z$Key))]],n)
      pv<-fo_asset_surplus(y,salary,v[1],v[2]/100)
      ages[[length(ages)+1L]]<-cbind(data.frame(From=z$From,To=z$To),y)
      career[[length(career)+1L]]<-cbind(data.frame(Player=z$Player,Control_Years=n),fo_asset_path(z$WAR,z$Age,horizon,v[3],v[4],v[5],v[6]))
      summ[[length(summ)+1L]]<-data.frame(Player=z$Player,From=z$From,To=z$To,Age=z$Age,
        "Control years"=n,"Expected total hWAR"=sum(y$Expected),"Upside total hWAR"=sum(y$Upside),
        "PV salary ($M)"=unname(pv["Salary"]),"Expected surplus ($M)"=unname(pv["Expected"]),"Upside surplus ($M)"=unname(pv["Upside"]),
        Coverage=if(all(is.finite(salary)))"User-entered salary schedule"else"Salary schedule missing or invalid",check.names=FALSE)
    }
    if(any(!x$Known))for(i in which(!x$Known)) {
      z<-x[i,];summ[[length(summ)+1L]]<-data.frame(Player=z$Player,From=z$From,To=z$To,Age=z$Age,
        "Control years"=NA_real_,"Expected total hWAR"=NA_real_,"Upside total hWAR"=NA_real_,"PV salary ($M)"=NA_real_,
        "Expected surplus ($M)"=NA_real_,"Upside surplus ($M)"=NA_real_,Coverage="No usable projection / age / club",check.names=FALSE)
    }
    list(paths=do.call(rbind,ages),career=do.call(rbind,career),horizon=horizon,summary=do.call(rbind,summ),n=nrow(x),covered=length(known))
  })
  output[[k("coverage")]]<-renderUI({
    q<-calc();fo_note(paste(q$covered,"of",q$n,"players have usable age and MLB forecasts. Salary scenarios do not alter the saved payroll or the 2027 transaction graph."))
  })
  output[[k("players")]]<-renderDT({fo_table(calc()$summary,10)},server=FALSE)
  output[[k("clubs")]]<-renderDT({
    q<-calc();s<-q$summary;clubs<-sort(unique(na.omit(c(s$From,s$To))))
    ok<-q$covered==q$n&&all(is.finite(s[["Expected surplus ($M)"]]))
    net<-function(col,t)if(ok)sum(s[[col]][which(s$To==t)])-sum(s[[col]][which(s$From==t)])else NA_real_
    z<-data.frame(Club=clubs,"Expected net surplus ($M)"=vapply(clubs,function(t)net("Expected surplus ($M)",t),numeric(1)),
      "Upside net surplus ($M)"=vapply(clubs,function(t)net("Upside surplus ($M)",t),numeric(1)),
      Coverage=if(ok)"Complete entered scenario"else"Incomplete; no net valuation",check.names=FALSE)
    fo_table(z,10)
  },server=FALSE)
  future<-reactive({
    q<-calc();p<-q$paths;f<-economic_forecast();r<-ratings()
    clubs<-sort(unique(c(p$From,p$To)));out<-list()
    for(y in 2026+seq_len(q$horizon))for(mode in c("Expected","Upside")) {
      a<-p[p$Year==y,,drop=FALSE];delta<-setNames(rep(0,length(clubs)),clubs)
      for(i in seq_len(nrow(a))) {
        delta[a$From[i]]<-delta[a$From[i]]-a[[mode]][i]
        delta[a$To[i]]<-delta[a$To[i]]+a[[mode]][i]
      }
      z<-tryCatch(fo_matchup_change(f,delta,ratings=r),error=function(e)e)
      validate(need(!inherits(z,"error"),if(inherits(z,"error"))conditionMessage(z)else ""))
      z<-z[z$Team%in%clubs,c("Team","Win_Change"),drop=FALSE];z$Year<-y;z$Scenario<-mode;out[[length(out)+1L]]<-z
    }
    do.call(rbind,out)
  })
  win_plot<-function(cumulative=FALSE) {
    z<-future();clubs<-sort(unique(z$Team));colors<-setNames(rep(c("#286d5a","#376f93","#b6794b","#a85c59"),length.out=length(clubs)),clubs)
    p<-plot_ly()
    for(t in clubs)for(mode in c("Expected","Upside")){
      a<-z[z$Team==t&z$Scenario==mode,];a<-a[order(a$Year),]
      values<-if(cumulative)cumsum(a$Win_Change)else a$Win_Change
      p<-add_trace(p,x=a$Year,y=values,type="scatter",mode="lines+markers",name=paste(t,mode),line=list(color=colors[[t]],dash=if(mode=="Expected")"solid"else"dash",width=if(mode=="Expected")3 else 2),marker=list(color=colors[[t]],size=5),hovertemplate=paste0(t," · ",mode,"<br>%{x}: %{y:+.2f} wins<extra></extra>"))
    }
    layout(p,xaxis=list(title="Season",dtick=1,tickformat="d"),yaxis=list(title=if(cumulative)"Cumulative incremental wins"else"Annual incremental wins",zeroline=TRUE),legend=list(orientation="h"),margin=list(t=10,b=65))
  }
  output[[k("future_wins")]]<-renderPlotly(win_plot())
  output[[k("cumulative_wins")]]<-renderPlotly(win_plot(TRUE))
  output[[k("career_paths")]]<-renderPlotly({
    z<-calc()$career;p<-plot_ly()
    for(t in unique(z$Player)){
      a<-z[z$Player==t,];a<-a[order(a$Year),]
      p<-add_trace(p,x=a$Year,y=a$Expected,type="scatter",mode="lines+markers",name=t,customdata=a$Active_Probability,hovertemplate=paste0(t,"<br>%{x}: %{y:.2f} hWAR<br>Scenario active probability: %{customdata:.0%}<extra></extra>"))
    }
    layout(p,xaxis=list(title="Season",dtick=1,tickformat="d"),yaxis=list(title="Illustrative expected player hWAR",zeroline=TRUE),legend=list(orientation="h"),margin=list(t=10,b=65))
  })
}

fo_move_ui <- function(teams,signing=FALSE) {
  id<-if(signing) "sign" else "trade"
  key<-function(x) paste0("fo_",id,"_",x)
  tagList(fo_title("FRONT OFFICE / MATCHUP IMPACT",if(signing) "Signing Simulator" else "Trade Simulator",
    if(signing) "See what signing a player does to the new club, the old club and everyone they play." else "Build a deal between two clubs. See how the players, wins and money move."),
    fo_note("Hypothetical comparison against the saved Model 5 baseline. Players are listed by prior-season affiliation, not verified contract status. Replacement-level depth fills outgoing playing time unless you specify displaced value. All dollar inputs are your assumptions."),
    fluidRow(column(6,fo_panel(h3(if(signing) "Signing club" else "Team A"),
      selectInput(key("a"),"Club",teams,selected="BAL"),
      if(signing) numericInput(key("displaced"),"Displaced hWAR (above replacement)",0,min=0,max=20,step=.25) else selectizeInput(key("send_a"),"Players sent to Team B",choices=NULL,multiple=TRUE),
      numericInput(key("pay_a"),"Net annual payroll change ($M)",0,min=-500,max=500,step=1))),
      column(6,fo_panel(h3(if(signing) "Player and former club" else "Team B"),
      selectInput(key("b"),if(signing) "Former club" else "Club",teams,selected=if(signing) "TBR" else "NYY"),
      selectizeInput(key("send_b"),if(signing) "Player signed" else "Players sent to Team A",choices=NULL,multiple=!signing),
      if(signing) radioButtons(key("baseline"),"Compare against",c("Former club retains player"="retains","Player already available (no former-club deduction)"="available")),
      numericInput(key("pay_b"),"Former / other club net payroll change ($M)",0,min=-500,max=500,step=1)))),
    radioButtons(key("workload"),"Player valuation",c("Expected hWAR"="hWAR","Healthy workload scenario (not a probability bound)"="Healthy_hWAR"),inline=TRUE),
    { immediate<-tagList(uiOutput(key("metrics")),
    fluidRow(column(6,fo_panel(h3("Players and net production"),DTOutput(key("players")))),column(6,fo_panel(h3("Economic effects on both clubs"),DTOutput(key("econ"))))),
    fo_panel(h3("League-wide win and loss changes"),plotlyOutput(key("chart"),height="340px"),DTOutput(key("table")),p(class="fo-small","Division opponents play each other 13 times in the synthetic schedule. Other intraleague opponents meet 6–7 times; interleague opponents 3–4 times. Changes may offset if opponents play both trade partners. League wins always equal league losses (2,430 each).")))
      if(signing) immediate else tabsetPanel(id="fo_trade_view",tabPanel("Immediate Impact",immediate),tabPanel("Future Value",fo_asset_ui("fo_trade_asset_"))) })
}
fo_move_server <- function(input,output,session,economic_forecast,signing=FALSE) {
  id<-if(signing) "sign" else "trade";key<-function(x) paste0("fo_",id,"_",x)
  get<-function(x) input[[key(x)]]
  p<-fo_players[fo_players$Season==2027&is.finite(fo_players$hWAR),]
  if(!"Healthy_hWAR"%in%names(p)) p$Healthy_hWAR<-NA_real_
  club_players<-function(t) p[grepl(paste0("(^|;)",t,"(;|$)"),ifelse(is.na(p$Teams),"",p$Teams)),]
  if(!signing) observeEvent(get("a"),{req(get("a"));updateSelectizeInput(session,key("send_a"),choices=fo_choices(club_players(get("a"))),selected=character(),server=TRUE)})
  observeEvent(get("b"),{req(get("b"));updateSelectizeInput(session,key("send_b"),choices=fo_choices(club_players(get("b"))),selected=character(),server=TRUE)})
  ratings<-reactive({f<-economic_forecast();req(nrow(f)==30);fo_calibrate(f$Team_ID,f$Expected_Wins_2027)})
  result<-reactive({
    a<-get("a");b<-get("b");mode<-get("workload");req(a,b,mode,get("pay_a"),get("pay_b"))
    validate(need(a!=b,"Choose two different clubs."),need(nrow(p)>0,"Add the private player data first."))
    pa<-if(signing) p[FALSE,] else club_players(a)[club_players(a)$ID%in%get("send_a"),]
    pb<-club_players(b)[club_players(b)$ID%in%get("send_b"),]
    validate(need(!length(intersect(pa$ID,pb$ID)),"Choose each player only once."),need(all(is.finite(c(pa[[mode]],pb[[mode]]))),"A selected player has no value for this workload scenario. Choose expected hWAR."))
    wa<-sum(pa[[mode]]);wb<-sum(pb[[mode]])
    delta_a<-wb-wa;delta_b<-wa-wb
    if(signing) {req(get("baseline"),get("displaced"));delta_a<-if(nrow(pb)) wb-get("displaced") else 0;delta_b<-if(nrow(pb)&&get("baseline")=="retains") -wb else 0}
    has<-nrow(pa)+nrow(pb)>0
    pay<-setNames(if(has)c(get("pay_a"),get("pay_b"))else c(0,0),c(a,b))
    z<-tryCatch(fo_matchup_change(economic_forecast(),setNames(c(delta_a,delta_b),c(a,b)),pay,ratings()),error=function(e) e)
    validate(need(!inherits(z,"error"),if(inherits(z,"error"))conditionMessage(z)else ""))
    list(z=z,a=a,b=b,pa=pa,pb=pb,wa=wa,wb=wb,da=delta_a,db=delta_b,mode=mode)
  })
  if(!signing) fo_asset_server(input,output,session,"fo_trade_asset_",reactive({
    q<-result();rows<-function(x,from,to)data.frame(ID=x$ID,Player=x$Player,From=rep(from,nrow(x)),To=rep(to,nrow(x)),Age=x$Age,WAR=x[[q$mode]],stringsAsFactors=FALSE)
    rbind(rows(q$pa,q$a,q$b),rows(q$pb,q$b,q$a))
  }),economic_forecast,ratings)
  output[[key("metrics")]]<-renderUI({q<-result();z<-q$z;row<-function(t) z[z$Team==t,];a<-row(q$a);b<-row(q$b)
    fo_kpis(fo_kpi(paste(q$a,"wins"),sprintf("%+.2f",a$Win_Change),paste(fo_num(a$Before_Wins),"\u2192",fo_num(a$After_Wins))),fo_kpi(paste(q$b,"wins"),sprintf("%+.2f",b$Win_Change),paste(fo_num(b$Before_Wins),"\u2192",fo_num(b$After_Wins))),fo_kpi("Net talent change",sprintf("%+.2f",q$da),paste(q$a,"hWAR")),fo_kpi("League wins",fo_num(sum(z$After_Wins),0),"Every added win is an added loss elsewhere"))})
  output[[key("players")]]<-renderDT({q<-result();rows<-function(x,from,to) data.frame(Player=x$Player,From=rep(from,nrow(x)),To=rep(to,nrow(x)),hWAR=round(x[[q$mode]],2));fo_table(rbind(rows(q$pa,q$a,q$b),rows(q$pb,q$b,q$a)),10)},server=FALSE)
  output[[key("econ")]]<-renderDT({q<-result();z<-q$z[q$z$Team%in%c(q$a,q$b),c("Team","Attendance_Change","Revenue_Change_M","Payroll_Change_M","Direct_Effect_M")];z[-1]<-lapply(z[-1],round,2);fo_table(z,10)},server=FALSE)
  output[[key("chart")]]<-renderPlotly({q<-result();z<-q$z;z<-z[order(-z$Win_Change),];plot_ly(z,x=~factor(Team,levels=Team),y=~Win_Change,type="bar",marker=list(color=ifelse(z$Win_Change>=0,"#2c8666","#b65c56"))) %>% layout(xaxis=list(title="Club"),yaxis=list(title="Expected win change"),margin=list(t=12))})
  output[[key("table")]]<-renderDT({q<-result();z<-q$z;z<-z[order(-abs(z$Win_Change)),];z[-c(1,2)]<-lapply(z[-c(1,2)],round,3);fo_table(z,30)},server=FALSE)
}
