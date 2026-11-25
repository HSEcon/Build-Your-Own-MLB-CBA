# Private research preview. Independent of existing CBA calculations.
fo_path <- "data/hwar_preview/players.csv"
fo_read_players <- function() {
  if (!file.exists(fo_path)) return(data.frame(Season=integer(),ID=character(),Player=character(),Teams=character(),PA=numeric(),IP=numeric(),Hitting_hWAR=numeric(),Pitching_hWAR=numeric(),hWAR=numeric(),OPS=numeric(),ERA=numeric(),Status=character(),Rank=numeric(),Core_WAR=numeric()))
  d <- read.csv(fo_path, stringsAsFactors=FALSE, fileEncoding="UTF-8", na.strings=c("","NA"))
  required <- c("Season","ID","Player","Teams","PA","IP","Hitting_hWAR","Pitching_hWAR","hWAR","OPS","ERA","Status","Rank")
  stopifnot(all(required %in% names(d)), !anyDuplicated(d[c("Season","ID")]))
  if (!"Core_WAR" %in% names(d)) d$Core_WAR <- NA_real_
  d$ID <- as.character(d$ID)
  # A failed projection is unknown, not a measured zero-WAR player.
  if("Defense_Coverage" %in% names(d)) {
    unknown <- which(d$Season==2027 & grepl("^Fallback:",d$Defense_Coverage))
    for(nm in intersect(c("hWAR","Hitting_hWAR","Pitching_hWAR","PA","IP","Healthy_hWAR","Healthy_PA","Healthy_IP"),names(d))) d[[nm]][unknown] <- NA_real_
  }
  d$Teams <- gsub("(^|;)KC(?=;|$)", "\\1KCR", d$Teams, perl=TRUE)
  d$Teams <- gsub("(^|;)SD(?=;|$)", "\\1SDP", d$Teams, perl=TRUE)
  d$Teams <- gsub("(^|;)SF(?=;|$)", "\\1SFG", d$Teams, perl=TRUE)
  d$Teams <- gsub("(^|;)TB(?=;|$)", "\\1TBR", d$Teams, perl=TRUE)
  d$Teams <- gsub("(^|;)OAK(?=;|$)", "\\1ATH", d$Teams, perl=TRUE)
  d
}
fo_players <- fo_read_players()
for (nm in c("Healthy_hWAR","Healthy_PA","Healthy_IP","Age","Batting_Runs","Running_Runs","Defense_Runs","Position_Runs","Pitch_Runs","FIP_Runs","RA9_Runs","Bat_Replacement","Pitch_Replacement","Runs_Per_Win")) if(!nm %in% names(fo_players)) fo_players[[nm]] <- rep(NA_real_,nrow(fo_players))
for (nm in c("Position","Defense_Coverage","Park_Coverage")) if(!nm %in% names(fo_players)) fo_players[[nm]] <- rep(NA_character_,nrow(fo_players))
fo_money <- function(x) paste0("$",formatC(x,format="f",digits=2,big.mark=","),"M")
fo_num <- function(x,digits=1) formatC(x,format="f",digits=digits,big.mark=",")
fo_title <- function(kicker,title,copy) div(class="page-head",div(class="eyebrow",kicker),h2(title),p(copy))
fo_note <- function(...) div(class="fo-note",...)
fo_panel <- function(...) div(class="dashboard-panel fo-panel",...)
fo_kpi <- function(label,value,note="") div(class="fo-kpi",div(class="fo-label",label),div(class="fo-value",value),div(class="fo-caption",note))
fo_kpis <- function(...) div(class="fo-kpis",...)
fo_css <- function() tags$style(HTML("
.fo-note{background:#e7eff5;border-left:3px solid #7195ad;padding:12px 15px;margin:12px 0 18px;color:#35556b;line-height:1.5}
.fo-kpis{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:12px;margin:14px 0 20px}
.fo-kpi{background:white;border:1px solid #d7e2e9;border-radius:8px;padding:16px;min-width:0}
.fo-label{font-size:12px;color:#61798d;font-weight:600}.fo-value{overflow-wrap:anywhere;font-size:25px;font-weight:700;color:#203c53;margin:5px 0;letter-spacing:-.6px}.fo-caption{font-size:11px;color:#607589;line-height:1.4}
.fo-profile-search{max-width:520px}.fo-profile-head{background:#fff;border:1px solid #d7e2e9;border-top:4px solid #286d5a;border-radius:10px;padding:18px 22px;margin:14px 0;display:flex;flex-wrap:wrap;gap:18px;align-items:center;justify-content:space-between}.fo-profile-head h2{font-size:32px;margin:4px 0}.fo-profile-head p{margin:4px 0;color:#61798d}.fo-profile-head>.fo-small{flex-basis:100%}.fo-profile-facts{display:flex;gap:24px;flex-wrap:wrap}.fo-profile-facts div{display:flex;flex-direction:column;gap:5px}.fo-profile-facts span{font-size:12px;color:#61798d}.fo-profile-facts strong{font-size:19px;color:#203c53}.fo-player-link,.fo-transaction-link{font-weight:650;color:#20684e;text-decoration:underline;text-underline-offset:2px}.fo-shortcuts{display:flex;align-items:center;flex-wrap:wrap;gap:16px;margin:8px 0 15px;padding:10px 14px;background:#f4f8f6;border:1px solid #dbe8e2;border-radius:7px;font-size:13px}.fo-shortcuts span{font-weight:650;color:#385e50}.fo-shortcuts a{color:#20684e;font-weight:650;text-decoration:underline}.fo-panel .dataTables_wrapper thead th{background:#233e50;color:white;border-bottom:2px solid #306c56;font-size:12px;white-space:nowrap}.fo-panel .dataTables_wrapper tbody td{font-variant-numeric:tabular-nums;vertical-align:middle}.fo-panel .dataTables_wrapper tbody tr:hover td{background-color:#eaf2ee}.fo-panel .nav-pills>li.active>a{background:#276b54}.fo-panel{min-width:0}.fo-controls{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px}.fo-panel .form-group{width:100%}.fo-small{font-size:12px;color:#62798c}
.fo-math{background:#f5f8fa;border:1px solid #d7e2e9;padding:12px;border-radius:5px;font-family:monospace;white-space:normal;overflow-wrap:anywhere}
@media(max-width:850px){.fo-kpis,.fo-controls{grid-template-columns:repeat(2,minmax(0,1fr))}}@media(max-width:520px){.fo-kpis,.fo-controls{grid-template-columns:1fr}}
"))
# Presentation only: preserve numeric values and raw field names for calculations and exports.
fo_pretty_label <- function(x) {
  known <- c(Teams="Team(s)",Healthy_hWAR="Healthy hWAR",Hitting_hWAR="Hitting hWAR",Pitching_hWAR="Pitching hWAR",
    Healthy_PA="Healthy PA",Healthy_IP="Healthy IP",Batting_Runs="Batting runs",Running_Runs="Baserunning runs",
    Defense_Runs="Fielding runs",Position_Runs="Positional runs",Pitch_Runs="Pitching runs",
    FIP_Runs="FIP runs",RA9_Runs="RA9 runs",Bat_Replacement="Hitting replacement runs",
    Pitch_Replacement="Pitching replacement runs",Runs_Per_Win="Runs per win",Defense_Coverage="Fielding coverage",
    Park_Coverage="Park coverage",Obligation_Season="Contract season",Transaction_Type="Transaction type",
    Salary_M="Salary ($M)",CBT_Charge_M="CBT charge ($M)",Snapshot_Reconciliation="Payroll reconciliation",
    Team_ID="Team",Team_Name="Team name",Before_Wins="Wins before",After_Wins="Wins after",
    Win_Change="Win change",Wins_Change="Win change",Loss_Change="Loss change",Attendance_Change="Attendance change",
    Revenue_Change_M="Revenue change ($M)",Payroll_Change_M="Payroll change ($M)",Direct_Effect_M="Direct financial effect ($M)",
    Expected_Wins_2027="Expected 2027 wins",Projected_2027_Payroll_Millions="Projected 2027 payroll ($M)",
    Statcast_Weight="Statcast weight",Statcast_Runs="Statcast runs",DRS_Runs="DRS runs",
    Projected_Defense_Wins="Projected fielding wins",Projected_Nondefense_Hitting_Wins="Projected other hitting wins")
  out<-x; hit<-x%in%names(known);out[hit]<-unname(known[x[hit]])
  raw<-!hit & grepl("_",x)
  out[raw]<-sub(" (M|Millions)$"," ($M)",gsub("_+"," ",gsub("([a-z])([A-Z])","\\1 \\2",sub("^mlbdb_(hitting|pitching)_","",x[raw]))))
  out
}
fo_display_digits <- function(x) {
  k<-tolower(gsub("[ _-]","",x))
  if(grepl("^(ip|healthyip|expectedip|projectedip)$",k))return(2L)
  if(grepl("^(season|year|rank|age|pa|healthypa|expectedpa|starts|battinggames|pitchinggames)$",k) ||
     grepl("attendance|defensiveouts|^statcastouts$|^drsouts$|controlyears|^b(ab|h|d|t|hr|r|rbi|w|k|hbp|sb|cs)$|^p(bfp|h|hr|r|er|w|k|hbp)$",k))return(0L)
  if(grepl("gini|coefficient|rsquared|rmse|^p$|^se$|^t$|^estimate$|weight$|rate$|success$|efficiency",k))return(3L)
  if(grepl("^(avg|obp|slg|ops|woba|babip|iso|xwoba|xslg|xavg)$|woba|babip",k))return(3L)
  if(grepl("^(era|ra9|whip|fip)$|winchange|winschange|losschange|grossmarginalwins|runsperwin",k))return(2L)
  if(grepl("hwar|payroll|salary|revenue|\\(\\$m\\)|millions|_m$|runs$|wins$|replacement",tolower(x)))return(1L)
  2L
}
fo_format_table <- function(t,d) {
  nums<-which(vapply(d,is.numeric,logical(1)))
  if(length(nums)) {
    digits<-vapply(names(d)[nums],fo_display_digits,integer(1))
    for(k in unique(digits))t<-DT::formatRound(t,nums[digits==k],digits=k)
  }
  t
}
fo_display_value <- function(v,field) {
  if(length(v)!=1L||is.na(v))return("—")
  if(is.numeric(v)) {
    if(!is.finite(v))return("—")
    return(formatC(v,format="f",digits=fo_display_digits(field),big.mark=","))
  }
  as.character(v)
}
fo_table <- function(d,page=15,select=FALSE) {
  t<-DT::datatable(d,colnames=fo_pretty_label(names(d)),rownames=FALSE,selection=if(select) "single" else "none",escape=TRUE,options=list(pageLength=page,scrollX=TRUE,autoWidth=FALSE,order=list(),lengthMenu=c(10,15,25,50,100)))
  fo_format_table(t,d)
}
fo_choices <- function(d) { if(!nrow(d)) return(character()); d <- d[order(-d$Season),]; d <- d[!duplicated(d$ID),]; d <- d[order(d$Player),]; setNames(d$ID,paste0(d$Player," (",d$Teams,")")) }
fo_default_player <- function() {
  x<-fo_players[fo_players$Season==2027 & is.finite(fo_players$hWAR),,drop=FALSE]
  if(nrow(x)) x$ID[which.max(x$hWAR)] else if(nrow(fo_players)) fo_players$ID[1] else character()
}
fo_name_key <- function(x) gsub("[^a-z0-9]","",tolower(iconv(x,to="ASCII//TRANSLIT",sub="")))
fo_player_match <- function(name,team=NULL,season=2027L) {
  d<-fo_players[fo_players$Season==season & !is.na(fo_players$Player),,drop=FALSE]
  key<-fo_name_key(name); if(length(key)!=1 || is.na(key) || !nzchar(key)) return(d[FALSE,,drop=FALSE])
  x<-d[which(fo_name_key(d$Player)==key),,drop=FALSE]
  if(nrow(x)>1 && !is.null(team) && !is.na(team)) x<-x[grepl(paste0("(^|;)",team,"(;|$)"),ifelse(is.na(x$Teams),"",x$Teams)),,drop=FALSE]
  if(nrow(x)==1) x else d[FALSE,,drop=FALSE]
}
fo_name_link <- function(name,team=NULL) {
  x<-fo_player_match(name,team)
  if(nrow(x)!=1) x<-fo_players[which(fo_name_key(fo_players$Player)==fo_name_key(name)),,drop=FALSE]
  if(length(unique(x$ID))==1) return(tags$a(href="#",class="fo-player-link",`data-fo-player`=x$ID[1],name))
  span(name,title="No unique player profile in the current dataset")
}
fo_year_values <- sort(unique(fo_players$Season),decreasing=TRUE)
fo_seasons <- setNames(as.character(fo_year_values),paste(fo_year_values,ifelse(fo_year_values>=2027,"scenario","historical")))
fo_role_class <- function(d) {
  pos<-ifelse(is.na(d$Position),"",toupper(d$Position));pa<-ifelse(is.finite(d$PA),d$PA,0);ip<-ifelse(is.finite(d$IP),d$IP,0)
  ifelse(pos=="TWP","Two-way players",ifelse(pos=="P","Pitchers",ifelse(pos%in%c("C","1B","2B","3B","SS","LF","CF","RF","DH","OF","IF"),"Position players",ifelse(pa>=100&ip>=20,"Two-way players",ifelse(ip>0&pa<100,"Pitchers",ifelse(pa>0,"Position players","Unclassified"))))))
}
fo_sort_choices <- function(x) {
  fields<-c("Value"="hWAR","Healthy hWAR"="Healthy_hWAR","Plate appearances"="PA","Innings pitched"="IP","OPS"="OPS","ERA"="ERA")
  c("Value",names(fields)[names(fields)!="Value" & vapply(fields,function(k)k%in%names(x)&&any(is.finite(x[[k]])),logical(1))])
}
fo_leaders_ui <- function(teams) tagList(fo_css(),tags$script(HTML("document.addEventListener('click',function(e){var a=e.target.closest('a.fo-player-link');if(!a)return;e.preventDefault();e.stopPropagation();var id=a.getAttribute('data-fo-player');if(window.Shiny && id)Shiny.setInputValue('fo_open_player',String(id),{priority:'event'});},true);")),
  fo_title("MODEL 6 / PLAYER ANALYTICS","hWAR Leaders","Find the best players in baseball. Compare seasons, filter by team and see how much value comes from hitting and pitching."),
  fo_note("Open-data hWAR. Historical data end in 2025; 2027 is a two-year scenario using 2025 affiliations, not verified rosters. Individual fielding uses a recorded-error proxy only; no range or park adjustment. Missing forecasts are not zero player value."),
  uiOutput("fo_leader_metrics"),
  div(class="fo-shortcuts",span("Explore the numbers"),tags$a(href="#",onclick="Shiny.setInputValue('fo_jump','profiles',{priority:'event'});return false;","Find a player"),tags$a(href="#",onclick="Shiny.setInputValue('fo_jump','research',{priority:'event'});return false;","How hWAR works")),
  fo_panel(div(class="fo-controls",selectInput("fo_year","Season",fo_seasons,selected=if("2027"%in%fo_seasons)"2027" else unname(fo_seasons)[1],selectize=FALSE),selectInput("fo_team","Team",c("All teams"="ALL",teams)),selectInput("fo_role","Player type",c("All players","Position players","Pitchers","Two-way players","Unclassified"))),
    div(class="fo-controls",selectInput("fo_sort","Sort by",fo_sort_choices(fo_players[fo_players$Season==(if(2027%in%fo_players$Season)2027 else if(length(fo_year_values))max(fo_year_values) else 2027),,drop=FALSE]),selected="Value",selectize=FALSE),textInput("fo_search","Search by player",""),downloadButton("fo_download","Download filtered CSV")),
    DTOutput("fo_leader_table"),p(class="fo-small","Click a player in the table to open their profile. Historical multi-team players appear under each team they played for; their full season totals are not split by club. 2027 OPS and ERA are hidden because they are not projections.")))
fo_profile_ui <- function() tagList(
  fo_title("MODEL 6 / PLAYER ANALYTICS","Player Profiles","Explore player value, career statistics and the economics of player movement."),
  div(class="fo-profile-search",selectizeInput("fo_player","Find a player",choices=fo_choices(fo_players),selected=fo_default_player())),
  div(class="fo-shortcuts",span("Quick navigation"),tags$a(href="#",onclick="Shiny.setInputValue('fo_jump','leaders',{priority:'event'});return false;","Back to player rankings"),tags$a(href="#",onclick="Shiny.setInputValue('fo_jump','research',{priority:'event'});return false;","How hWAR works")),
  fo_profile_context_ui(),uiOutput("fo_profile_metrics"),
  tabsetPanel(id="fo_profile_tab",type="pills",
    tabPanel("Overview",fo_standard_profile_ui(),fo_panel(h3("Career hWAR"),plotlyOutput("fo_career_plot",height="320px"),p(class="fo-small","2027 is a projection. Historical hWAR has incomplete defense coverage; healthy workload is a scenario, not an upper confidence bound.")),
      fluidRow(column(6,fo_panel(h3("2027 projected value breakdown"),uiOutput("fo_projection_status"),conditionalPanel("output.fo_has_projection === true",plotlyOutput("fo_components",height="275px"),DTOutput("fo_component_table")),p(class="fo-small","The active forecast displays only measured or projected components available in the dataset; combined hitting value includes defense when separate projections are unavailable."))),column(6,fo_panel(h3("Season-by-season numbers"),DTOutput("fo_history")))),
    fo_panel(h3("Historical run components"),DTOutput("fo_historical_components"),p(class="fo-small","Historical run components are displayed in runs, not wins. Missing fielding or park coverage is not a measured zero."))),
    tabPanel("Statistics",fo_profile_detail_ui()),
    tabPanel("Projection research",fo_projection_research_ui()),
    tabPanel("Transactions & contracts",fo_related_ui())))
fo_roster_ui <- function(teams) tagList(fo_title("FRONT OFFICE / WHAT-IF","Build Your Own Roster","Pick a team, make hypothetical moves and see what they do to wins, payroll and revenue."),
  fo_note("This is a sandbox, not a live free-agent list. Players are selectable regardless of contract status. Enter the production and salary you would replace. Existing payroll-based forecasts remain unchanged."),
  fluidRow(column(5,fo_panel(selectInput("fo_club","Your team",teams,selected="BAL"),selectizeInput("fo_add","Players to acquire",choices=NULL,multiple=TRUE),
    numericInput("fo_salary","Total new annual payroll ($M)",30,min=0,max=1000,step=1),numericInput("fo_removed_war","Displaced hWAR",2,min=-20,max=100,step=.25),numericInput("fo_removed_salary","Removed payroll ($M)",5,min=0,max=1000,step=1),
    numericInput("fo_budget","Extra payroll budget ($M)",40,min=0,max=1000,step=1),actionButton("fo_clear","Reset moves"),p(class="fo-small","All salary assumptions are yours; no unverified contract estimates are inserted."))),
    column(7,uiOutput("fo_roster_metrics"),fo_panel(h3("Your selected players"),DTOutput("fo_selected")),fo_panel(h3("Economic impact"),DTOutput("fo_roster_econ")))),
  fo_panel(h3("Effect on all 30 clubs"),plotlyOutput("fo_roster_chart",height="310px"),p(class="fo-small","Incremental gains are centered across the league. The total expected wins stay at 2,430. This is a single-team replacement experiment, not a completed trade or schedule model.")))
fo_schedule_ui <- function() tagList(fo_title("MODEL 7 / RESEARCH PREVIEW","Projected Standings","A team can have the same talent and a different expected record depending on who it plays."),
  fo_note("The default schedule is synthetic. Upload a verified 162-game-per-club schedule to test actual matchups. Team strengths currently start from the Model 5 payroll forecast, not a verified full roster hWAR forecast."),
  fo_panel(div(class="fo-controls",fileInput("fo_schedule_file","Optional schedule CSV",accept=".csv"),sliderInput("fo_home_adv","Home field advantage (log odds)",0,.4,0,.02),downloadButton("fo_schedule_template","Download schedule template")),
    p(class="fo-small","CSV columns: Home, Away, Games. One row per ordered matchup; all club IDs must match the existing financial data and each club must play exactly 162 games.")),
  uiOutput("fo_schedule_metrics"),fluidRow(column(7,fo_panel(h3("Schedule-adjusted wins"),plotlyOutput("fo_schedule_plot",height="430px"))),column(5,fo_panel(h3("How much does the schedule matter?"),DTOutput("fo_schedule_table")))),
  fo_panel(h3("How we made Model 7"),p("We convert each baseline winning percentage into a strength rating, use a logistic matchup model to estimate the probability of beating each opponent, and sum expected wins across the schedule."),div(class="fo-math","P(home wins) = logistic(strength_home - strength_away + home advantage)"),p("Every game contributes exactly one expected win across both teams. The default synthetic schedule has 2,430 games. Ratings are calibrated against that schedule without home advantage before applying an alternative schedule. Actual schedule, home advantage and hWAR strength calibration require out-of-sample validation.")))
fo_contract_ui <- function() tagList(fo_title("MODEL 8 / CONTRACT ECONOMICS","Contract Valuation Lab","A contract is worth more than the number on the front page. See how age, future production and the time value of money change the value of a deal."),
  fo_note("User-controlled valuation scenario. The dollar value of hWAR and decline rate are assumptions, not fitted market estimates. Market surplus is not a forecast of a club's cash profit."),
  fluidRow(column(4,fo_panel(selectizeInput("fo_contract_player","Player",choices=NULL),numericInput("fo_years","Years",5,min=1,max=15,step=1),numericInput("fo_aav","Annual salary ($M)",20,min=0,max=1000),numericInput("fo_market","Market value per hWAR ($M)",8,min=0,max=50,step=.5),sliderInput("fo_decline","Annual hWAR change",-1,.5,-.3,.05),sliderInput("fo_discount","Annual discount rate (%)",0,20,5,.5))),
    column(8,uiOutput("fo_contract_metrics"),fo_panel(plotlyOutput("fo_contract_plot",height="330px")))),fo_panel(h3("Year-by-year contract value"),DTOutput("fo_contract_table")),
  fo_panel(h3("How we made Model 8"),div(class="fo-math","Surplus PV = sum[(projected hWAR × market $/hWAR − annual salary) / (1 + discount rate)^year]"),p("All payments and player production are treated as end-of-year flows. The model uses a linear, user-chosen change in hWAR and floors hWAR at zero. Future work: estimate salary per win, age-specific performance, injuries, options and deferred payments.")))
fo_fa_ui <- function() tagList(fo_title("FRONT OFFICE / PLAYER MARKET","Free-Agent Lab","Connect our player projections to the existing transaction tracker. Compare reported deals with player value without assuming unverified salaries."),
  fo_note("Only unique exact name matches are joined automatically. AAV is not necessarily 2027 payroll or CBT charge. All reported deals retain their original status; the existing tracker is still the source of truth."),
  fo_panel(DTOutput("fo_fa_table"),p(class="fo-small","Unmatched players stay blank. This view covers tracked reported agreements, not every unsigned free agent.")))
# Model 6 defensive research: compare like-for-like run estimates, not outs and runs.
# Starting weights are explicit research priors, not empirically optimal estimates.
fo_defense_weights <- data.frame(Position=c('C','1B','2B','3B','SS','LF','CF','RF'),
  Statcast=c(.35,.30,.65,.45,.75,.55,.75,.55),
  stringsAsFactors=FALSE)
fo_defense_weights$DRS <- 1-fo_defense_weights$Statcast
fo_blend_defense <- function(position,statcast_runs,drs_runs,statcast_outs,drs_outs) {
  n<-length(position);stopifnot(all(lengths(list(statcast_runs,drs_runs,statcast_outs,drs_outs))==n))
  w<-fo_defense_weights$Statcast[match(position,fo_defense_weights$Position)]
  valid_s<-is.finite(statcast_runs)&is.finite(statcast_outs)&statcast_outs>0
  valid_d<-is.finite(drs_runs)&is.finite(drs_outs)&drs_outs>0
  eligible<-!is.na(w)
  # Exact positional defensive outs required for two-source comparisons.
  both<-eligible&valid_s&valid_d&statcast_outs==drs_outs
  s_only<-eligible&valid_s&!both
  d_only<-eligible&!valid_s&valid_d
  value<-rep(NA_real_,n);used<-rep(NA_real_,n);basis<-rep('Unmeasured / excluded',n)
  value[both]<-w[both]*statcast_runs[both]+(1-w[both])*drs_runs[both];used[both]<-w[both];basis[both]<-'Two-source blend'
  value[s_only]<-statcast_runs[s_only];used[s_only]<-1;basis[s_only]<-ifelse(valid_d[s_only],'Statcast only: exposure mismatch','Statcast only')
  value[d_only]<-drs_runs[d_only];used[d_only]<-0;basis[d_only]<-'DRS only'
  data.frame(Position=position,Statcast_Runs=statcast_runs,DRS_Runs=drs_runs,Statcast_Weight=used,Defense_Runs=value,Coverage=basis,stringsAsFactors=FALSE)
}
fo_defense_lab_ui <- function() fo_panel(h3('Position-specific defense weighting'),
  p('Compare total Statcast fielding run value (including OAA-derived range where applicable) with total Defensive Runs Saved. Both are estimates in runs; never add raw OAA to DRS. The weights below are research priors, not fitted coefficients. Catcher framing, blocking and throwing are already included where available; do not count them twice.'),
  tags$table(class='table table-striped',tags$thead(tags$tr(tags$th('Position'),tags$th('Statcast'),tags$th('DRS'))),
    tags$tbody(lapply(seq_len(nrow(fo_defense_weights)),function(i) tags$tr(tags$td(fo_defense_weights$Position[i]),tags$td(paste0(100*fo_defense_weights$Statcast[i],'%')),tags$td(paste0(round(100*fo_defense_weights$DRS[i]),'%')))))),
  fo_note('The open-data model uses a recorded-error proxy from Retrosheet. No proprietary Statcast or DRS data are installed. This hypothetical research calculator does not change the leaderboard, projections or trade estimates.'),
  div(class='fo-controls',selectInput('fo_def_pos','Position',fo_defense_weights$Position,selected='CF'),numericInput('fo_def_s','Statcast fielding runs (example)',10),numericInput('fo_def_d','DRS (example)',4)),
  div(class='fo-controls',numericInput('fo_def_s_out','Statcast defensive outs',2400,min=0,step=1),numericInput('fo_def_d_out','DRS defensive outs',2400,min=0,step=1),numericInput('fo_def_rpw','Runs per win (example)',10,min=1,step=.1)),
  uiOutput('fo_def_result'),hr(),h4('Test a verified positional dataset'),fileInput('fo_def_file','Authorized positional fielding CSV (local session only)',accept='.csv'),p(class='fo-small','Required headers: ID, Season, Position, Statcast_Runs, DRS_Runs, Statcast_Outs, DRS_Outs. One row per player, season and position; both run metrics must cover the same defensive outs. No data are sent to external providers.'),DTOutput('fo_def_import'),p(class='fo-small','Example inputs are illustrative, not actual player statistics. For production, import authorized player-season-position data with matching exposures, sum the positional estimates once, replace the existing defensive component, and refit and validate the forecast. Pitchers and DH are excluded from this fielding blend.'))
fo_defense_lab_server <- function(input,output) {
  output$fo_def_import<-renderDT({
    req(input$fo_def_file)
    d<-tryCatch(read.csv(input$fo_def_file$datapath,colClasses='character',stringsAsFactors=FALSE),error=function(e)NULL)
    fields<-c('ID','Season','Position','Statcast_Runs','DRS_Runs','Statcast_Outs','DRS_Outs')
    validate(need(!is.null(d)&&nrow(d)>0&&all(fields%in%names(d)),'Invalid or empty positional CSV.'),need(!anyDuplicated(d[c('ID','Season','Position')]),'Duplicate player-season-position records.'))
    validate(need(all(d$Position%in%fo_defense_weights$Position),'Unsupported position.'),need(all(paste(d$ID,d$Season)%in%paste(fo_players$ID[fo_players$Season<2027],fo_players$Season[fo_players$Season<2027])),'Player ID or historical season absent from the installed dataset.'))
    for(nm in fields[4:7]) {raw<-d[[nm]];num<-suppressWarnings(as.numeric(raw));validate(need(all(is.na(raw)|trimws(raw)==''|is.finite(num)),paste('Invalid numeric field:',nm)));d[[nm]]<-num}
    validate(need(all(is.na(d$Statcast_Outs)|d$Statcast_Outs>=0&d$Statcast_Outs==floor(d$Statcast_Outs))&&all(is.na(d$DRS_Outs)|d$DRS_Outs>=0&d$DRS_Outs==floor(d$DRS_Outs)),'Defensive outs must be nonnegative integers.'))
    z<-fo_blend_defense(d$Position,d$Statcast_Runs,d$DRS_Runs,d$Statcast_Outs,d$DRS_Outs)
    fo_table(cbind(d[c('ID','Season')],z),15)
  },server=FALSE)
  output$fo_def_result<-renderUI({
    req(input$fo_def_pos,input$fo_def_s,input$fo_def_d,input$fo_def_s_out,input$fo_def_d_out,input$fo_def_rpw)
    validate(need(is.finite(input$fo_def_rpw)&&input$fo_def_rpw>0,'Runs per win must be positive.'))
    z<-fo_blend_defense(input$fo_def_pos,input$fo_def_s,input$fo_def_d,input$fo_def_s_out,input$fo_def_d_out)
    f<-function(x) if(is.finite(x)) fo_num(x,2) else '—'
    fo_kpis(fo_kpi('Defense runs',f(z$Defense_Runs),z$Coverage),fo_kpi('Defense wins',f(z$Defense_Runs/input$fo_def_rpw),'Not total hWAR'),fo_kpi('Effective Statcast weight',if(is.finite(z$Statcast_Weight))paste0(round(100*z$Statcast_Weight),'%')else '—'),fo_kpi('Effective DRS weight',if(is.finite(z$Statcast_Weight))paste0(round(100*(1-z$Statcast_Weight)),'%')else '—'))
  })
}

fo_methods_ui <- function() tagList(
  research_section(11,"Model 6 — Player value (open hWAR)","Independent estimates from reusable Retrosheet regular-season records, not published fWAR, bWAR, OAA or DRS.",
    div(class="research-finding",strong("Data coverage: "),"2015–2025 observed statistics. 2027 is a two-year scenario based on the last complete season, 2025. No 2026 observations or verified 2027 rosters are implied."),
    h4("Step 1 — Batting and baserunning"),
    formula_box("R_{bat}=\\sum_j w_j E_j-PA\\times\\overline{r}_{bat}"),
    p("Use fixed illustrative event weights: unintentional walk 0.69, hit by pitch 0.72, single 0.88, double 1.25, triple 1.58 and home run 2.03. Subtract the season average per PA. Baserunning adds 0.2 runs per steal and subtracts 0.4 per caught stealing. These are approximations, not fitted run values."),
    h4("Step 2 — Recorded-error proxy, position and replacement"),
    formula_box("R_{def}=0.7\\sum_p(Chances_p\\times ErrorRate_{lg,p}-Errors_p)"),
    formula_box("hWAR_{hit}=\\frac{R_{bat}+R_{run}+R_{def}+R_{pos}+20PA/600}{RPW}"),
    p("Compare recorded errors to the league error rate at each position, using putouts + assists + errors as chances. This is not range, framing, OAA or DRS. Positional workload adjustments use explicit illustrative runs per 1,350 defensive innings; no park adjustment. For a hitter without recorded fielding workload, use the DH approximation. Missing individual fielding is not measured zero. The separate research calculator below illustrates how a future authorized two-source defensive blend could work; it is not used in the leaderboard."),
    fo_defense_lab_ui(),
    h4("Step 3 — Pitching"),
    formula_box("FIP=\\frac{13HR+3(BB+HBP)-2K}{IP}+C"),
    formula_box("hWAR_{pitch}=\\frac{(ERA_{lg}-FIP)IP/9+IP/9}{RPW}"),
    p("Season FIP constant aligns league FIP with ERA. Use FIP rather than adding team-dependent runs allowed a second time. Runs per win is the explicit illustrative scale 1.5 times league RA9 plus 3. Two-way player value is the sum of hitting and pitching. This is not a validated replica of existing WAR systems."),
    h4("Step 4 — Two-year player scenarios"),
    p("For players observed in the origin season, weight the last three observed seasons 3, 2 and 1, latest first. Estimate per-opportunity hWAR with workload shrinkage (150 weighted PA or 40 weighted IP toward zero replacement value); multiply by the weighted mean workload. The healthy scenario uses maximum observed workload over those seasons. No age adjustment is fitted in the player forecast; the separate trade lab applies user-selected age scenarios. Players absent from the 2025 records are not projected."),
    h4("Chronological validation"),
    p("The build records two-year holdout RMSE for 2023, 2024 and 2025 against independently calculated historical hWAR, alongside the origin-season baseline, for matched players only. It does not measure true player value or forecast accuracy for new entrants. See the dataset audit."),
    p("The information used here was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at \"www.retrosheet.org\".")),
  research_section(12,"Model 7 — Schedule, trades and signings","Turn Model 6 player moves or Model 1 payroll changes into team-specific wins and losses, then connect them to Model 5 economics.",
    div(class="research-finding",strong("Research question: "),"If a club changes its roster or payroll, which opponents gain or lose expected wins, and what does that mean for attendance and revenue? A schedule-based model answers this without assigning identical losses to every other club."),
    h4("Step 1 — Build the matchup baseline"),
    formula_box("p_{ij}=\\frac{1}{1+e^{-(s_i-s_j+h)}}"),
    formula_box("W_i=\\sum_{j\\ne i}\\left[g_{ij}^{home}p_{ij}+g_{ij}^{away}(1-p_{ji})\\right]"),
    p("Calibrate strength ratings s to the saved Model 5 baseline at zero home advantage. The synthetic schedule has 13 games against each division opponent, six or seven against other intraleague opponents, and three or four against interleague opponents. A verified schedule can replace it in the standings lab."),
    h4("Step 2 — Move the players"),
    formula_box("\\Delta v_i=\\sum hWAR_{in}-\\sum hWAR_{out}-hWAR_{displaced}"),
    p("The outgoing club loses the same player's production relative to a retention baseline. Replacement-level depth fills open playing time by default. Signing a player already excluded from a club's baseline should not create a second loss. For real trades, matched players enter the covered subtotal while unknown prospects remain explicitly unvalued."),
    formula_box("W_i(s_i+\\delta_i,s_{-i})-W_i(s_i,s_{-i})=\\Delta v_i"),
    p("Solve each changed club's strength adjustment holding the other ratings fixed, then apply all adjustments simultaneously and recalculate every matchup. Teams playing an improved opponent more often generally lose more expected wins, but gains and losses can offset when they play both trade partners."),
    div(class="research-finding",strong("Accounting identity: "),"Every game creates one expected win and one expected loss. The league has 2,430 games; total changes in wins and losses both sum to zero."),
    h4("Step 3 — Translate winning to financial outcomes"),
    formula_box("\\Delta A_i=A_i\\left[e^{0.7638659\\Delta W_i/162}-1\\right]"),
    formula_box("\\Delta R_i=R_i\\left[e^{(0.156374\\times0.7638659+0.141378)\\Delta W_i/162}-1\\right]"),
    p("These are incremental Model 5 effects. The baseline growth intercept is not repeated. Model 1 payroll-based wins and Model 6 player-based wins are alternative strength estimates, not additive. Salary changes affect the direct club financial effect; unknown salary/CBT terms do not block the baseball estimate."),
    h4("Worked illustration — a five-win improvement"),
    p("A hypothetical Baltimore improvement changes the win probability of each scheduled game involving Baltimore. The Yankees, Red Sox, Rays and Blue Jays each play Baltimore 13 times in the synthetic schedule, whereas most other clubs meet it less often. The actual expected loss also depends on each opponent's baseline strength. The sum of all 30 changes is zero; individual losses need not be equal."),
    p("Validation: 162 games per club; one expected winner per game; zero-sum trades and signings; division weighting. Actual schedules, roster replacement assumptions and prediction accuracy need further validation.")),
  research_section(13,"Model 8 — Front office and contract economics","Connect player value, roster moves, team financial outcomes and discounted contract costs to practical front-office decisions.",
    h4("Step 1 — Establish the baseball and financial alternatives"),
    p("Select a reported or hypothetical move, compare the affected players' expected or healthy-workload projections, account for displaced playing time and evaluate the opponent-specific changes from Model 7. Inspect the salary and CBT inputs separately. Missing player or contract data are shown as incomplete rather than automatically assigned zero value."),
    h4("Step 2 — Value contract production and salary"),
    formula_box("v_t=\\max(0,\\widehat{hWAR}_{2027}+(t-1)d)"),
    formula_box("PV_{salary}=\\sum_{t=1}^{T}\\frac{C_t}{(1+r)^t},\\quad PV_{value}=\\sum_{t=1}^{T}\\frac{v_t m}{(1+r)^t}"),
    formula_box("PV_{surplus}=PV_{value}-PV_{salary}"),
    p("T is contract length, C is annual cash compensation, d is the assumed annual decline, m is the assumed market price per hWAR, and r is the discount rate. The contract lab uses flat end-of-year salary payments, while the existing finance tools support user-entered cash schedules including year-zero bonuses."),
    h4("Step 3 — Interpret surplus and constraints"),
    p("Positive surplus suggests that projected production is worth more than discounted salary under the selected market-price assumption; it does not establish that a trade is fair. The long-term trade valuation uses per-player assumed remaining control, annual salary schedules and editable age-specific growth and decline to compare discounted production surplus. A separate upside scenario adds pre-peak growth, while future matchup wins use the same baseline each year. Unverified salary and prospect value remain unknown. No statistically validated trade grade or prospect valuation is claimed."),
    p("AAV, annual cash salary and CBT payroll are different measures. Use the annual contract ledger where terms are known. The market price of hWAR and decline assumptions are user-controlled, not estimated from a fitted salary model. Surplus measures player market value minus salary; it is not team profit."),
    div(class="research-finding",strong("Connection to the earlier models: "),"Model 6 provides projected production; Model 7 provides team wins; Models 2 and 5 estimate incremental revenue; Model 4 measures payroll and CBA exposure. Keep each measure distinct to avoid double counting.")),
  fo_panel(h3("Sources and release limitations"),
    p("Independent open-data player research. No third-party player photos, club logos, proprietary WAR or Statcast/DRS data are included in the player dataset."),
    tags$ul(tags$li(tags$a(href="https://www.retrosheet.org/downloads/csvdownloads.html","Retrosheet — historical player counts and biographical records")),tags$li(tags$a(href="https://www.retrosheet.org/notice.txt","Retrosheet reuse terms"))),
    p("The information used here was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at \"www.retrosheet.org\".")))
fo_synthetic_schedule <- function(ids) {
  stopifnot(length(ids)==30L)
  rows <- list(); k <- 1L
  for (i in seq_len(29)) for(j in (i+1):30) {
    dist <- min(j-i,30-(j-i))
    n <- if(dist<=8 || dist==15) 6L else 5L
    home <- if(n==6) 3L else if((j-i)%%2==0) 3L else 2L
    rows[[k]] <- data.frame(Home=ids[i],Away=ids[j],Games=home);k<-k+1L
    rows[[k]] <- data.frame(Home=ids[j],Away=ids[i],Games=n-home);k<-k+1L
  }
  do.call(rbind,rows)
}
fo_validate_schedule <- function(s,ids) {
  stopifnot(all(c("Home","Away","Games") %in% names(s)))
  stopifnot(nrow(s)>0,all(s$Home %in% ids),all(s$Away %in% ids),all(s$Home!=s$Away),is.numeric(s$Games),all(is.finite(s$Games)),all(s$Games>0),all(s$Games==round(s$Games)),!anyDuplicated(s[c("Home","Away")]))
  total <- vapply(ids,function(x) sum(s$Games[s$Home==x | s$Away==x]),numeric(1))
  stopifnot(all(total==162),sum(s$Games)==2430)
  s
}
fo_schedule_wins <- function(s,r,home=0) {
  p <- plogis(r[s$Home]-r[s$Away]+home)
  setNames(vapply(names(r),function(x) sum(s$Games[s$Home==x]*p[s$Home==x])+sum(s$Games[s$Away==x]*(1-p[s$Away==x])),numeric(1)),names(r))
}
fo_calibrate <- function(ids,target) {
  s <- fo_synthetic_schedule(ids)
  objective <- function(x) {r <- setNames(c(x,0),ids);sum((fo_schedule_wins(s,r)-target)^2)}
  fit <- optim(rep(0,length(ids)-1),objective,method="BFGS",control=list(maxit=1000,reltol=1e-12))
  r <- setNames(c(fit$par,0),ids);r <- r-mean(r)
  stopifnot(max(abs(fo_schedule_wins(s,r)-target))<.05)
  r
}
fo_server <- function(input,output,session,economic_forecast) {
  fo_defense_lab_server(input,output)
  d <- fo_players
  require_players <- function() validate(need(nrow(d)>0,"Add the private player-data ZIP to this app folder first."))
  p27 <- d[d$Season==2027 & is.finite(d$hWAR),]
  observe({
    if(nrow(d)>0) {
      updateSelectizeInput(session,"fo_add",choices=fo_choices(p27),server=TRUE)
      updateSelectizeInput(session,"fo_contract_player",choices=fo_choices(p27),selected=if("691718" %in% p27$ID) "691718" else p27$ID[1],server=TRUE)
    }
  })
  observeEvent(input$fo_year,{
    x<-d[d$Season==as.integer(input$fo_year),,drop=FALSE];choices<-fo_sort_choices(x)
    selected<-isolate(input$fo_sort);if(is.null(selected)||!selected%in%choices)selected<-"Value"
    updateSelectInput(session,"fo_sort",choices=choices,selected=selected)
  })
  leaders <- reactive({
    require_players();req(input$fo_year,input$fo_team,input$fo_role,input$fo_sort)
    x <- d[d$Season==as.integer(input$fo_year),]
    hist <- as.integer(input$fo_year)<2027
    x$Value <- x$hWAR
    x$League_Rank <- rank(-x$Value,ties.method="min",na.last="keep")
    if(input$fo_team!="ALL") x <- x[grepl(paste0("(^|;)",input$fo_team,"(;|$)"),ifelse(is.na(x$Teams),"",x$Teams)),]
    if(input$fo_role!="All players") x <- x[which(fo_role_class(x)==input$fo_role),,drop=FALSE]
    if(nzchar(trimws(input$fo_search))) x <- x[grepl(tolower(input$fo_search),tolower(x$Player),fixed=TRUE),]
    if(as.integer(input$fo_year)==2027) {x$OPS<-NA_real_;x$ERA<-NA_real_}
    v <- switch(input$fo_sort,"Value"=x$Value,"Healthy hWAR"=x$Healthy_hWAR,"Plate appearances"=x$PA,"Innings pitched"=x$IP,"OPS"=x$OPS,"ERA"=-x$ERA)
    if(is.null(v)||!input$fo_sort%in%fo_sort_choices(x))v<-x$Value
    x[order(v,decreasing=TRUE,na.last=TRUE),,drop=FALSE]
  })
  output$fo_leader_metrics <- renderUI({
    x<-leaders();z<-x[is.finite(x$Value),];lead<-if(nrow(z)) z[which.max(z$Value),] else NULL
    fo_kpis(fo_kpi("Players",nrow(x),"Filtered results"),fo_kpi("Valued players",nrow(z),"Non-missing estimates"),fo_kpi("hWAR leader",if(is.null(lead)) "—" else fo_num(lead$Value,2),if(is.null(lead)) "No calculated values" else lead$Player),fo_kpi("Season",input$fo_year,if(input$fo_year=="2027") "Preliminary projection" else "Historical results"))
  })
  output$fo_leader_table <- renderDT({
    x<-leaders(); hist<-as.integer(input$fo_year)<2027
    z<-data.frame(Rank=x$League_Rank,Player=x$Player,Team=x$Teams,PA=x$PA,IP=x$IP,Value=x$Value,check.names=FALSE)
    names(z)[6]<-"hWAR"
    z[["Age"]]<-x$Age
    z[["Position"]]<-x$Position;z[["Hitting hWAR"]]<-x$Hitting_hWAR;z[["Pitching hWAR"]]<-x$Pitching_hWAR
    if(input$fo_year=="2027") z[["Healthy hWAR"]]<-x$Healthy_hWAR
    if("Projected_Defense_Wins"%in%names(x) && input$fo_year=="2027") z[["Defense wins"]]<-round(x$Projected_Defense_Wins,2)
    if(input$fo_year!="2027") z[["Fielding runs"]]<-round(x$Defense_Runs,2)
    z[["Coverage"]]<-x$Defense_Coverage
    if(input$fo_year!="2027"){z$OPS<-round(x$OPS,3);z$ERA<-round(x$ERA,2)}
    z$Player<-vapply(seq_len(nrow(x)),function(i)as.character(tags$a(href="#",class="fo-player-link",`data-fo-player`=x$ID[i],x$Player[i])),character(1))
    t<-DT::datatable(z,rownames=FALSE,escape=-2,selection="none",class="compact stripe hover",options=list(pageLength=15,scrollX=TRUE,order=list(),lengthMenu=c(15,25,50,100),fixedHeader=TRUE))
    DT::formatStyle(fo_format_table(t,z),"hWAR",backgroundColor=DT::styleInterval(c(0,2,5),c("#f9e9e5","#f5f7f8","#e9f3ee","#cfe5d8")),fontWeight="bold")
  },server=TRUE)
  output$fo_download <- downloadHandler(filename=function() paste0("player-leaders-",input$fo_year,".csv"),content=function(file) write.csv(leaders(),file,row.names=FALSE,na=""))
  observeEvent(input$fo_jump,{if(identical(input$fo_jump,"leaders")){updateRadioButtons(session,"nav_group",selected="Player Analytics");updateRadioButtons(session,"player_tab",selected="hWAR Leaders");updateTabsetPanel(session,"dashboard_tab",selected="hWAR Leaders")}else if(identical(input$fo_jump,"profiles")){updateRadioButtons(session,"player_tab",selected="Player Profiles");updateTabsetPanel(session,"dashboard_tab",selected="Player Profiles")}else if(identical(input$fo_jump,"research")){updateRadioButtons(session,"nav_group",selected="Research & Background");updateRadioButtons(session,"research_tab",selected="How the Model Works");updateTabsetPanel(session,"dashboard_tab",selected="How the Model Works")}},ignoreInit=TRUE)
  observeEvent(input$fo_open_player,{
    id<-as.character(input$fo_open_player);req(id %in% d$ID)
    removeModal();updateRadioButtons(session,"nav_group",selected="Player Analytics")
    updateRadioButtons(session,"player_tab",selected="Player Profiles")
    updateTabsetPanel(session,"dashboard_tab",selected="Player Profiles")
    updateTabsetPanel(session,"fo_profile_tab",selected="Overview")
    updateSelectizeInput(session,"fo_player",selected=id)
    session$onFlushed(function() {
      updateRadioButtons(session,"nav_group",selected="Player Analytics")
      updateRadioButtons(session,"player_tab",selected="Player Profiles")
      updateTabsetPanel(session,"dashboard_tab",selected="Player Profiles")
      updateSelectizeInput(session,"fo_player",selected=id)
    },once=TRUE)
  },ignoreInit=TRUE)
  profile <- reactive({require_players();req(input$fo_player);x<-d[d$ID==input$fo_player,];x[order(x$Season),]})
  output$fo_profile_metrics <- renderUI({
    x<-profile();a<-x$hWAR[x$Season==2025];b<-x$hWAR[x$Season==2027];f<-function(v) if(length(v)&&is.finite(v[1])) fo_num(v[1],2) else "—"
    fo_kpis(fo_kpi("2025 hWAR",f(a),"See defense and park coverage"),fo_kpi("2027 hWAR",f(b),"Expected workload"),fo_kpi("Projected PA / IP",paste(f(x$PA[x$Season==2027]),"/",f(x$IP[x$Season==2027])),"Plate appearances / innings"),fo_kpi("Healthy hWAR",f(x$Healthy_hWAR[x$Season==2027]),"Workload scenario"))
  })
  output$fo_career_plot <- renderPlotly({
    x<-profile();p<-plot_ly()
    if(any(is.finite(x$hWAR[x$Season<2027]))) p<-add_trace(p,x=x$Season[x$Season<2027],y=x$hWAR[x$Season<2027],name="Historical hWAR (coverage varies)",type="scatter",mode="lines+markers",line=list(color="#2b5a78"))
    if(any(is.finite(x$hWAR[x$Season==2027]))) p<-add_trace(p,x=x$Season[x$Season==2027],y=x$hWAR[x$Season==2027],name="Expected 2027",type="scatter",mode="markers",marker=list(color="#b6794b",size=12))
    if(any(is.finite(x$Healthy_hWAR))) p<-add_trace(p,x=x$Season,y=x$Healthy_hWAR,name="Healthy workload",type="scatter",mode="markers",marker=list(color="#38866c",size=10))
    layout(p,xaxis=list(title="Season",dtick=1),yaxis=list(title="Wins above replacement"),legend=list(orientation="h"),margin=list(t=20))
  })
  has_projection <- reactive({
    q<-profile();q<-q[q$Season==2027,,drop=FALSE]
    nrow(q)==1L && is.finite(q$hWAR)
  })
  output$fo_has_projection <- reactive(has_projection())
  shiny::outputOptions(output,"fo_has_projection",suspendWhenHidden=FALSE)
  output$fo_projection_status <- renderUI({
    if(!has_projection()) fo_note("No validated 2027 projection available. Historical statistics remain accessible; missing value is not zero WAR.") else NULL
  })
  projected_components <- reactive({
    x<-profile();q<-x[x$Season==2027,,drop=FALSE];validate(need(nrow(q)==1 && is.finite(q$hWAR),"No validated 2027 projection available."))
    if(all(c("Projected_Defense_Wins","Projected_Nondefense_Hitting_Wins")%in%names(q)) &&
       all(is.finite(c(q$Projected_Defense_Wins,q$Projected_Nondefense_Hitting_Wins,q$Pitching_hWAR))) &&
       abs(sum(c(q$Projected_Defense_Wins,q$Projected_Nondefense_Hitting_Wins,q$Pitching_hWAR))-q$hWAR)<.001) {
      data.frame(Component=c("Other hitting value (includes replacement)","Defense","Pitching"),Wins=c(q$Projected_Nondefense_Hitting_Wins,q$Projected_Defense_Wins,q$Pitching_hWAR))
    } else data.frame(Component=c("Hitting (includes available defense)","Pitching"),Wins=c(q$Hitting_hWAR,q$Pitching_hWAR))
  })
  output$fo_components <- renderPlotly({
    z<-projected_components();plot_ly(z,x=~Wins,y=~Component,type="bar",orientation="h",marker=list(color=ifelse(z$Wins<0,"#ba6057","#297451")),hovertemplate="%{y}: %{x:.2f} wins<extra></extra>") %>%
      layout(xaxis=list(title="Wins above replacement"),yaxis=list(title="",automargin=TRUE),margin=list(t=10,l=15),showlegend=FALSE)
  })
  output$fo_component_table<-renderDT({
    z<-projected_components();z<-rbind(z,data.frame(Component="Total projected hWAR",Wins=sum(z$Wins)));z$Wins<-round(z$Wins,2);fo_table(z,5)
  },server=FALSE)
  output$fo_historical_components<-renderDT({
    x<-profile();x<-x[x$Season<2027,,drop=FALSE];validate(need(nrow(x)>0,"No historical seasons in this dataset."))
    fields<-c("Season","Batting_Runs","Running_Runs","Defense_Runs","Position_Runs","Pitch_Runs","Bat_Replacement","Pitch_Replacement","Runs_Per_Win","Defense_Coverage","Park_Coverage")
    z<-x[,fields,drop=FALSE]
    unmeasured<-is.na(z$Defense_Coverage)|grepl("unmeasured|missing|unavailable|unmatched|incomplete|not measured",z$Defense_Coverage,ignore.case=TRUE)
    z$Defense_Runs[unmeasured]<-NA_real_
    z[2:9]<-lapply(z[2:9],round,2);fo_table(z,10)
  },server=FALSE)
  output$fo_history <- renderDT({x<-profile();x$OPS[x$Season==2027]<-NA;x$ERA[x$Season==2027]<-NA;fo_table(x[,c("Season","Teams","Position","Age","PA","IP","hWAR","Healthy_hWAR","OPS","ERA")],10)},server=FALSE)
  observeEvent(input$fo_clear,{updateSelectizeInput(session,"fo_add",selected=character());updateNumericInput(session,"fo_salary",value=30);updateNumericInput(session,"fo_removed_war",value=2);updateNumericInput(session,"fo_removed_salary",value=5);updateNumericInput(session,"fo_budget",value=40)})
  roster_selected<-reactive({require_players();p27[p27$ID %in% input$fo_add,]})
  output$fo_selected<-renderDT({x<-roster_selected();fo_table(x[,c("Player","Teams","PA","IP","hWAR")],10)},server=FALSE)
  roster<-reactive({
    x<-roster_selected();f<-economic_forecast();req(input$fo_club)
    validate(need(nrow(f)==30,"Existing Model 5 forecast unavailable."))
    k<-match(input$fo_club,f$Team_ID);req(!is.na(k))
    validate(need(!any(grepl(paste0("(^|;)",input$fo_club,"(;|$)"),ifelse(is.na(x$Teams),"",x$Teams))),"A selected player already has this club as a 2025 affiliation. Choose another player."))
    raw<-if(nrow(x)) sum(x$hWAR)-input$fo_removed_war else 0
    payroll<-if(nrow(x)) input$fo_salary-input$fo_removed_salary else 0
    delta<-rep(-raw/30,30);delta[k]<-raw*29/30
    w<-f$Expected_Wins_2027+delta
    validate(need(all(w>=0&w<=162),"This scenario would create an impossible team record."))
    dw<-delta/162;att<-f$Attendance_2027*exp(.7638659*dw)
    rev<-f$Revenue_2027*exp(.156374*log(att/f$Attendance_2027)+.141378*dw)
    stopifnot(abs(sum(w)-2430)<1e-5)
    list(x=x,f=f,k=k,raw=raw,payroll=payroll,delta=delta,w=w,att=att,rev=rev)
  })
  output$fo_roster_metrics<-renderUI({z<-roster();k<-z$k;fo_kpis(fo_kpi("Acquired hWAR",fo_num(sum(z$x$hWAR),2)),fo_kpi("Net wins added",fo_num(z$delta[k],2),"After displaced value"),fo_kpi("Projected wins",fo_num(z$w[k],1)),fo_kpi("Budget remaining",fo_money(input$fo_budget-z$payroll),"Negative means over budget"))})
  output$fo_roster_econ<-renderDT({z<-roster();k<-z$k;revenue<-z$rev[k]-z$f$Revenue_2027[k];fo_table(data.frame(Measure=c("Additional payroll","Additional attendance","Additional revenue","Revenue less additional payroll","Payroll per added win"),Estimate=c(fo_money(z$payroll),fo_num(z$att[k]-z$f$Attendance_2027[k],0),fo_money(revenue),fo_money(revenue-z$payroll),if(z$delta[k]>0) fo_money(z$payroll/z$delta[k]) else "—")),10)},server=FALSE)
  output$fo_roster_chart<-renderPlotly({z<-roster();plot_ly(x=z$f$Team_ID,y=z$delta,type="bar",marker=list(color=ifelse(z$delta>=0,"#376f93","#a7b4bf"))) %>% layout(yaxis=list(title="Expected win change"),xaxis=list(title="Club"),margin=list(t=15))})
  schedule_ratings<-reactive({f<-economic_forecast();validate(need(nrow(f)==30,"Model 5 forecast unavailable."));fo_calibrate(f$Team_ID,f$Expected_Wins_2027)})
  output$fo_schedule_template<-downloadHandler(filename=function() "synthetic_schedule_template.csv",content=function(file) write.csv(fo_synthetic_schedule(economic_forecast()$Team_ID),file,row.names=FALSE))
  schedule<-reactive({
    f<-economic_forecast();r<-schedule_ratings();s<-fo_synthetic_schedule(f$Team_ID)
    if(!is.null(input$fo_schedule_file)){s<-tryCatch(fo_validate_schedule(read.csv(input$fo_schedule_file$datapath,stringsAsFactors=FALSE),f$Team_ID),error=function(e) NULL);validate(need(!is.null(s),"Invalid schedule: check required columns, club IDs, positive integer games, uniqueness and 162 games per team."))}
    req(input$fo_home_adv);w<-fo_schedule_wins(s,r,input$fo_home_adv);stopifnot(abs(sum(w)-2430)<1e-6)
    data.frame(Team=f$Team_ID,Baseline=f$Expected_Wins_2027,Adjusted=as.numeric(w[f$Team_ID]),Change=as.numeric(w[f$Team_ID])-f$Expected_Wins_2027)
  })
  output$fo_schedule_metrics<-renderUI({x<-schedule();fo_kpis(fo_kpi("League wins",fo_num(sum(x$Adjusted),0)),fo_kpi("Games per club",162),fo_kpi("Largest change",fo_num(max(abs(x$Change)),2),"Expected wins"),fo_kpi("Schedule",if(is.null(input$fo_schedule_file)) "Synthetic" else "Uploaded","Not independently verified"))})
  output$fo_schedule_plot<-renderPlotly({x<-schedule();x<-x[order(x$Adjusted),];plot_ly(x,y=~factor(Team,levels=Team),x=~Adjusted,type="bar",orientation="h",marker=list(color="#376f93"),name="Adjusted") %>% add_markers(x=~Baseline,name="Baseline",marker=list(color="#b6794b",size=5)) %>% layout(xaxis=list(title="Expected wins"),yaxis=list(title=""),margin=list(t=15))})
  output$fo_schedule_table<-renderDT({x<-schedule();x<-x[order(-abs(x$Change)),];x[-1]<-lapply(x[-1],round,2);fo_table(x,15)},server=FALSE)
  contract<-reactive({
    require_players();req(input$fo_contract_player,input$fo_years,input$fo_aav,input$fo_market,input$fo_decline,input$fo_discount)
    x<-p27[p27$ID==input$fo_contract_player,];req(nrow(x)==1)
    n<-as.integer(input$fo_years);validate(need(n>=1&&n<=15,"Choose 1 to 15 years."))
    t<-seq_len(n);war<-pmax(0,x$hWAR+(t-1)*input$fo_decline);value<-war*input$fo_market;salary<-rep(input$fo_aav,n);discount<-(1+input$fo_discount/100)^t
    data.frame(Year=2026+t,hWAR=war,Market_Value=value,Salary=salary,Surplus_PV=(value-salary)/discount,Value_PV=value/discount,Salary_PV=salary/discount)
  })
  output$fo_contract_metrics<-renderUI({x<-contract();fo_kpis(fo_kpi("Contract value",fo_money(sum(x$Salary))),fo_kpi("PV of salary",fo_money(sum(x$Salary_PV))),fo_kpi("PV of player value",fo_money(sum(x$Value_PV))),fo_kpi("Estimated surplus",fo_money(sum(x$Surplus_PV)),"Under your assumptions"))})
  output$fo_contract_plot<-renderPlotly({x<-contract();plot_ly(x,x=~Year,y=~Market_Value,type="scatter",mode="lines+markers",name="Market value",line=list(color="#376f93")) %>% add_trace(y=~Salary,name="Salary",mode="lines+markers",line=list(color="#b6794b")) %>% layout(yaxis=list(title="$ millions"),xaxis=list(dtick=1),margin=list(t=15))})
  output$fo_contract_table<-renderDT({x<-contract();x[-1]<-lapply(x[-1],round,2);fo_table(x,15)},server=FALSE)
  output$fo_fa_table<-renderDT({
    f<-read.csv("data/free_agency_tracker.csv",stringsAsFactors=FALSE)
    key<-function(x) gsub("[^a-z0-9]","",tolower(iconv(x,to="ASCII//TRANSLIT")))
    q<-key(p27$Player);matches<-vapply(key(f$Player),function(k){i<-which(q==k);if(length(i)==1) p27$hWAR[i] else NA_real_},numeric(1))
    z<-data.frame(Player=f$Player,Team=f$New_Team_ID,Status=f$Status,Years=f$Years,AAV_M=round(f$Nominal_AAV_Millions,2),Projected_hWAR=round(matches,2),AAV_per_hWAR=ifelse(matches>0,round(f$Nominal_AAV_Millions/matches,2),NA_real_))
    fo_table(z,15)
  },server=FALSE)
}
