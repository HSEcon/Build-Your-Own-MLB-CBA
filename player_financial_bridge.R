# Read-only research bridge. No saved payroll or CBA scenario is mutated.
fo_profile_detail_ui <- function() fo_panel(h3("Player statistics"),
  div(class="fo-controls",selectInput("fo_detail_year","Season",choices=NULL),selectInput("fo_detail_group","Stats",c("hWAR breakdown","Standard batting","Advanced batting","Batted ball and discipline","Standard pitching","Advanced pitching","Pitch arsenal","Fielding and baserunning","All available statistics")),downloadButton("fo_profile_export","Download player data")),
  uiOutput("fo_detail_coverage"),DTOutput("fo_detail_table"),p(class="fo-small","Historical counting and rate statistics are calculated from Retrosheet; proprietary advanced metrics are not included. Blank means unavailable or not applicable. 2027 is a two-year scenario, not observed statistics."))
# Observed standard statistics, never projected totals.
fo_standard_profile_ui <- function() fo_panel(
  h3("Traditional MLB statistics"),
  uiOutput("fo_standard_coverage"),uiOutput("fo_standard_summary"),
  uiOutput("fo_standard_bat_panel"),uiOutput("fo_standard_pitch_panel"),uiOutput("fo_standard_field_panel"),
  p(class="fo-small","Regular-season observations only. Multi-team season totals are combined. Career-window totals include only seasons in this dataset, not necessarily the entire MLB career. Rates use combined counts. IP uses baseball notation (.1 = one out, .2 = two outs); other model tables may use decimal innings. A dash means unavailable or not applicable."))
fo_ip_notation <- function(outs) ifelse(is.na(outs),NA_character_,paste0(outs%/%3,".",outs%%3))
fo_standard_table <- function(z,role) {
  stopifnot(!anyDuplicated(z[c("ID","Season")]))
  get<-function(x,n) if(n %in% names(x)) as.numeric(x[[n]]) else rep(NA_real_,nrow(x))
  sum_known<-function(x) if(all(is.finite(x))) sum(x) else NA_real_
  rate<-function(n,d) ifelse(is.finite(n)&is.finite(d)&d>0,n/d,NA_real_)
  fields<-switch(role,
    batting=c(G="batting_games",PA="b_pa",AB="b_ab",R="b_r",H="b_h","2B"="b_d","3B"="b_t",HR="b_hr",RBI="b_rbi",BB="b_w",SO="b_k",SB="b_sb",CS="b_cs",HBP="b_hbp",SF="b_sf"),
    pitching=c(G="pitching_games",GS="starts",W="p_wins",L="p_losses",SV="p_saves",Outs="p_ipouts",BF="p_bfp",H="p_h",R="p_r",ER="p_er",HR="p_hr",BB="p_w",SO="p_k",HBP="p_hbp"),
    fielding=c(Chances="Recorded_Fielding_Chances",E="Recorded_Fielding_Errors"))
  active<-switch(role,batting=get(z,"b_pa")>0,pitching=get(z,"p_ipouts")>0|get(z,"pitching_games")>0,fielding=get(z,"Recorded_Fielding_Chances")>0)
  z<-z[which(active),,drop=FALSE];if(!nrow(z))return(NULL)
  z<-z[order(z$Season),,drop=FALSE]
  fields<-fields[unname(fields)%in%names(z)]
  if(!length(fields))return(NULL)
  d<-data.frame(Season=as.character(z$Season),Team=z$Teams,check.names=FALSE)
  for(n in names(fields))d[[n]]<-get(z,fields[[n]])
  total<-data.frame(Season="Dataset total",Team="",check.names=FALSE)
  for(n in names(fields))total[[n]]<-sum_known(d[[n]])
  d<-rbind(d,total)
  val<-function(n) if(n%in%names(d))d[[n]] else rep(NA_real_,nrow(d))
  if(role=="batting"){
    d$AVG<-rate(val("H"),val("AB"))
    d$OBP<-rate(val("H")+val("BB")+val("HBP"),val("AB")+val("BB")+val("HBP")+val("SF"))
    d$SLG<-rate(val("H")+val("2B")+2*val("3B")+3*val("HR"),val("AB"))
    d$OPS<-d$OBP+d$SLG
  }
  if(role=="pitching"){
    d$ERA<-rate(27*val("ER"),val("Outs"))
    d$WHIP<-rate(3*(val("BB")+val("H")),val("Outs"))
    d[["K/9"]]<-rate(27*val("SO"),val("Outs"))
    d[["BB/9"]]<-rate(27*val("BB"),val("Outs"))
    d$IP<-fo_ip_notation(val("Outs"));d$Outs<-NULL
    preferred<-c("Season","Team","G","GS","W","L","SV","IP","BF","H","R","ER","HR","BB","SO","HBP","ERA","WHIP","K/9","BB/9")
    d<-d[,intersect(preferred,names(d)),drop=FALSE]
  }
  if(role=="fielding")d$FPCT<-rate(val("Chances")-val("E"),val("Chances"))
  d
}
fo_standard_dt <- function(d) {
  t<-DT::datatable(d,rownames=FALSE,selection="none",escape=TRUE,
    class="compact stripe hover",options=list(dom="t",paging=FALSE,scrollX=TRUE,ordering=FALSE,
      rowCallback=DT::JS("function(row,data){if(data[0]==='Dataset total'){$(row).css({'font-weight':'bold','background-color':'#eaf2ee','border-top':'2px solid #276b54'});}}")))
  ints<-intersect(c("G","PA","AB","R","H","2B","3B","HR","RBI","BB","SO","SB","CS","HBP","SF","GS","W","L","SV","BF","ER","Chances","E"),names(d))
  rates3<-intersect(c("AVG","OBP","SLG","OPS","FPCT"),names(d))
  rates2<-intersect(c("ERA","WHIP","K/9","BB/9"),names(d))
  if(length(ints))t<-DT::formatRound(t,ints,0)
  if(length(rates3))t<-DT::formatRound(t,rates3,3)
  if(length(rates2))t<-DT::formatRound(t,rates2,2)
  t
}
fo_standard_profile_server <- function(input,output,session) {
  hist<-reactive({req(input$fo_player);fo_players[fo_players$ID==input$fo_player & fo_players$Season<2027,,drop=FALSE]})
  bat<-reactive(fo_standard_table(hist(),"batting"));pitch<-reactive(fo_standard_table(hist(),"pitching"));field<-reactive(fo_standard_table(hist(),"fielding"))
  output$fo_standard_coverage<-renderUI({z<-hist();if(!nrow(z))return(fo_note("No observed regular-season statistics for this player."));fo_note(paste("Observed seasons:",min(z$Season),"–",max(z$Season),"·",nrow(z),"seasons in dataset"))})
  output$fo_standard_summary<-renderUI({
    z<-hist();if(!nrow(z))return(NULL)
    b<-bat();p<-pitch();f<-function(d,n,k=0) if(is.null(d)||!n%in%names(d))"—" else {x<-tail(d[[n]],1);if(is.numeric(x)&&is.finite(x))fo_num(x,k) else if(is.na(x))"—" else as.character(x)}
    items<-list()
    if(!is.null(b))items<-c(items,list(fo_kpi("Hits",f(b,"H")),fo_kpi("Home runs",f(b,"HR")),fo_kpi("Batting average",f(b,"AVG",3)),fo_kpi("OPS",f(b,"OPS",3))))
    if(!is.null(p))items<-c(items,list(fo_kpi("Innings pitched",f(p,"IP")),fo_kpi("Strikeouts",f(p,"SO")),fo_kpi("ERA",f(p,"ERA",2)),fo_kpi("WHIP",f(p,"WHIP",2))))
    if(length(items))do.call(fo_kpis,items)
  })
  output$fo_standard_bat_panel<-renderUI({if(!is.null(bat()))tagList(h4("Batting"),DTOutput("fo_standard_bat"))})
  output$fo_standard_pitch_panel<-renderUI({if(!is.null(pitch()))tagList(h4("Pitching"),DTOutput("fo_standard_pitch"))})
  output$fo_standard_field_panel<-renderUI({if(!is.null(field()))tagList(h4("Fielding"),DTOutput("fo_standard_field"))})
  output$fo_standard_bat<-renderDT({req(bat());fo_standard_dt(bat())},server=FALSE)
  output$fo_standard_pitch<-renderDT({req(pitch());fo_standard_dt(pitch())},server=FALSE)
  output$fo_standard_field<-renderDT({req(field());fo_standard_dt(field())},server=FALSE)
}

fo_derived_stats <- function(x) {
  if(nrow(x)!=1||!is.finite(x$Season)||x$Season>=2027)return(x)
  get<-function(k)if(k%in%names(x))suppressWarnings(as.numeric(x[[k]][1]))else NA_real_
  ratio<-function(n,d)if(is.finite(n)&&is.finite(d)&&d>0)n/d else NA_real_
  vals<-list(ISO=get("SLG")-get("AVG"),BB_rate=ratio(get("b_w"),get("b_pa")),K_rate=ratio(get("b_k"),get("b_pa")),SB_success=ratio(get("b_sb"),get("b_sb")+get("b_cs")),K_minus_BB_rate=ratio(get("p_k")-get("p_w"),get("p_bfp")))
  for(k in names(vals))if(!k%in%names(x)||!is.finite(get(k)))x[[k]]<-vals[[k]]
  x
}

fo_profile_detail_server <- function(input,output,session) {
  fo_standard_profile_server(input,output,session)
  cache<-new.env(parent=emptyenv())
  raw<-function(){if(!exists("d",cache,inherits=FALSE)){path<-"data/hwar_preview/profile_stats.csv";cache$d<-if(file.exists(path)) read.csv(path,stringsAsFactors=FALSE,check.names=FALSE,na.strings=c("","NA")) else NULL};cache$d}
  player<-reactive({req(input$fo_player);x<-fo_players[fo_players$ID==input$fo_player,];validate(need(nrow(x)>0,"Player data unavailable."));x})
  observeEvent(input$fo_player,{x<-player();years<-sort(unique(x$Season),decreasing=TRUE);updateSelectInput(session,"fo_detail_year",choices=years,selected=if(any(years<2027))max(years[years<2027]) else years[1])})
  details<-reactive({x<-player();req(input$fo_detail_year);x<-x[x$Season==as.integer(input$fo_detail_year),];req(nrow(x)==1)
    a<-raw();if(!is.null(a)&&all(c("ID","Season")%in%names(a))){b<-a[as.character(a$ID)==x$ID & a$Season==x$Season,,drop=FALSE];if(nrow(b)==1)x<-cbind(x,b[,setdiff(names(b),names(x)),drop=FALSE])};fo_derived_stats(x)})
  output$fo_detail_coverage<-renderUI({x<-details();fo_note(x$Status,br(),"Defense: ",ifelse(is.na(x$Defense_Coverage),"Unavailable",x$Defense_Coverage)," | Park: ",ifelse(is.na(x$Park_Coverage),"Unavailable",x$Park_Coverage))})
  output$fo_detail_table<-renderDT({
    x<-details();req(input$fo_detail_group)
    n<-names(x);core<-c("PA","IP","hWAR","Hitting_hWAR","Pitching_hWAR","Healthy_hWAR","Healthy_PA","Healthy_IP","Batting_Runs","Running_Runs","Defense_Runs","Position_Runs","Pitch_Runs","FIP_Runs","RA9_Runs","Bat_Replacement","Pitch_Replacement","Runs_Per_Win")
    standard_b<-c("batting_games","PA","b_ab","b_h","b_d","b_t","b_hr","b_r","b_rbi","b_w","b_k","b_hbp","b_sb","b_cs","AVG","OBP","SLG","OPS")
    standard_p<-c("pitching_games","starts","IP","p_bfp","p_h","p_hr","p_r","p_er","p_w","p_k","p_hbp","ERA","RA9","WHIP","FIP")
    fields<-switch(input$fo_detail_group,"hWAR breakdown"=core,"Standard batting"=standard_b,
      "Advanced batting"=c("ISO","BB_rate","K_rate","SB_success",grep("^mlbdb_hitting_(woba|wraa|wrc|babip|expected|iso|.*Rate|.*Pct)",n,value=TRUE)),
      "Batted ball and discipline"=grep("^mlbdb_hitting_.*(ExitVelocity|LaunchAngle|barrel|hardHit|sweetSpot|[Ss]wing|[Cc]ontact|[Cc]hase|[Zz]one|[Ww]hiff|[Pp]ull|[Oo]pposite|[Cc]enter|[Cc]alledStrike)",n,value=TRUE),
      "Standard pitching"=standard_p,"Advanced pitching"=c("FIP","K_minus_BB_rate",grep("^mlbdb_pitching_(?!arsenal).*(Pct|[Rr]ate|[Ee]xpected|[Ee]xtension|[Ss]peed|[Ss]pin|[Ww]hiff|[Bb]arrel|[Cc]ontact|[Cc]hase|[Zz]one|[Bb]abip)",n,value=TRUE,perl=TRUE)),
      "Pitch arsenal"=grep("^mlbdb_pitching_arsenal",n,value=TRUE),"Fielding and baserunning"=c("Position","Running_Runs","Defense_Runs","Position_Runs","b_sb","b_cs","SB_success",grep("(_d_|_ifouts|_po$|_a$|_e$|_dp$|_pb$|_sb$|_cs$)",n,value=TRUE)),"All available statistics"=setdiff(n,c("ID","Player","Season","Core_WAR","Legacy_2026_hWAR")))
    fields<-unique(intersect(fields,n));vals<-vapply(fields,function(k)fo_display_value(x[[k]][1],k),character(1))
    label<-fo_pretty_label(fields)
    known<-c(b_ab="At bats",b_h="Hits",b_d="Doubles",b_t="Triples",b_hr="Home runs",b_r="Runs",b_rbi="RBI",b_w="Walks",b_k="Strikeouts",b_hbp="Hit by pitch",b_sb="Stolen bases",b_cs="Caught stealing",p_bfp="Batters faced",p_h="Hits allowed",p_hr="Home runs allowed",p_r="Runs allowed",p_er="Earned runs",p_w="Walks allowed",p_k="Strikeouts",p_hbp="Hit batters")
    label[fields%in%names(known)]<-unname(known[fields[fields%in%names(known)]])
    z<-data.frame(Statistic=label,Value=unname(vals),check.names=FALSE);if(input$fo_detail_group!="hWAR breakdown"&&input$fo_detail_group!="Standard batting"&&input$fo_detail_group!="Standard pitching")z<-z[z$Value!="—",];validate(need(nrow(z)>0,"No measured statistics available for this player and season. Choose another category or historical season; unavailable metrics are not estimated."));fo_table(z,20)
  },server=FALSE)
  output$fo_profile_export<-downloadHandler(filename=function() paste0("player-",input$fo_player,"-",input$fo_detail_year,".csv"),content=function(file) write.csv(details(),file,row.names=FALSE,na=""))
}
fo_report_ui <- function() fo_panel(h3("Player-based signing impact"),
  fo_note("Compare one reported signing against a hypothetical retention baseline. This is not an additional change to saved payroll or wins. Salary remains unknown unless the 2027 cash change is reported; AAV is never substituted automatically."),
  div(class="fo-controls",selectInput("fo_report_signing","Reported signing",choices=NULL),radioButtons("fo_report_reference","Comparison",c("Former club retains the player"="retains","Player already available"="available")),numericInput("fo_report_displaced","Displaced hWAR at signing club",0,min=0,max=20,step=.25)),
  uiOutput("fo_report_status"),DTOutput("fo_report_impacts"),plotlyOutput("fo_report_chart",height="260px"))
fo_roster_coverage_ui <- function() fo_panel(h3("2027 player projection coverage"),p("The hWAR totals below group players by their sole 2025 affiliation. They are not verified 2027 rosters or additive win forecasts. Multi-team players are excluded to avoid double counting. Compare with the payroll-based baseline above."),uiOutput("fo_coverage_note"),DTOutput("fo_coverage_table"))
fo_bridge_server <- function(input,output,session,economic_forecast) {
  p<-fo_players[fo_players$Season==2027 & is.finite(fo_players$hWAR),,drop=FALSE]
  norm<-function(x)gsub("[^a-z0-9]","",tolower(iconv(x,to="ASCII//TRANSLIT",sub="")))
  reports<-reactive({f<-read.csv("data/free_agency_tracker.csv",stringsAsFactors=FALSE,na.strings=c("","NA"));req(nrow(f)>0);f})
  observe({f<-reports();updateSelectInput(session,"fo_report_signing",choices=setNames(f$Signing_ID,paste(f$Player,"→",f$New_Team_ID,"|",f$Status)))})
  rate<-reactive({f<-economic_forecast();req(nrow(f)==30);fo_calibrate(f$Team_ID,f$Expected_Wins_2027)})
  report<-reactive({
    req(input$fo_report_signing,input$fo_report_reference,input$fo_report_displaced)
    f<-reports();x<-f[f$Signing_ID==input$fo_report_signing,];req(nrow(x)==1)
    a<-x$New_Team_ID;b<-x$Previous_Team_ID;ids<-economic_forecast()$Team_ID
    validate(need(!is.na(a)&&a%in%ids,"No recognized signing club in this report."))
    i<-which(norm(p$Player)==norm(x$Player))
    validate(need(length(i)==1,"No unique matching player projection; the signing cannot be valued yet."))
    q<-p[i,];v<-q$hWAR
    if(input$fo_report_reference=="retains"){
      validate(need(!is.na(b)&&b%in%ids&&b!=a,"No recognized distinct former club; choose the already-available comparison."),need(grepl(paste0("(^|;)",b,"(;|$)"),ifelse(is.na(q$Teams),"",q$Teams)),"The player's historical affiliation does not match the reported former club. Choose the already-available comparison."))
    }
    d<-setNames(v-input$fo_report_displaced,a);if(input$fo_report_reference=="retains")d[b]<- -v
    cash<-x$Payroll_2027_Impact_Millions;pay<-if(length(cash)==1&&is.finite(cash))setNames(cash,a) else setNames(numeric(),character())
    z<-fo_matchup_change(economic_forecast(),d,pay,rate())
    z$Payroll_Change_M<-NA_real_;z$Direct_Effect_M<-NA_real_;if(length(pay)){k<-z$Team==a;z$Payroll_Change_M[k]<-cash;z$Direct_Effect_M[k]<-z$Revenue_Change_M[k]-cash}
    list(row=x,player=q,z=z)
  })
  output$fo_report_status<-renderUI({q<-report();fo_kpis(fo_kpi("Projected hWAR",fo_num(q$player$hWAR,2),q$player$Player),fo_kpi("Signing club wins",sprintf("%+.2f",q$z$Win_Change[q$z$Team==q$row$New_Team_ID]),"Model 7 scenario"),fo_kpi("Reported status",q$row$Status),fo_kpi("League wins",fo_num(sum(q$z$After_Wins),0)))})
  output$fo_report_impacts<-renderDT({q<-report();z<-q$z;z<-z[order(-abs(z$Win_Change)),c("Team","Division","Before_Wins","After_Wins","Win_Change","Loss_Change","Attendance_Change","Revenue_Change_M","Payroll_Change_M","Direct_Effect_M")];z[-c(1,2)]<-lapply(z[-c(1,2)],round,2);fo_table(z,10)},server=FALSE)
  output$fo_report_chart<-renderPlotly({q<-report();z<-q$z;z<-z[order(-z$Win_Change),];plot_ly(z,x=~factor(Team,levels=Team),y=~Win_Change,type="bar",marker=list(color=ifelse(z$Win_Change>=0,"#2c8666","#b65c56"))) %>% layout(xaxis=list(title="Club"),yaxis=list(title="Expected win change"),margin=list(t=10))})
  output$fo_coverage_note<-renderUI({validate(need(nrow(p)>0,"Private player projections are not installed."));x<-p[!is.na(p$Teams)&p$Teams%in%names(fo_div),];fo_kpis(fo_kpi("Projected players",nrow(p)),fo_kpi("Single affiliation",nrow(x)),fo_kpi("Projected PA",fo_num(sum(p$PA,na.rm=TRUE),0)),fo_kpi("Projected IP",fo_num(sum(p$IP,na.rm=TRUE),0)))})
  output$fo_coverage_table<-renderDT({validate(need(nrow(p)>0,"Private player projections are not installed."));f<-economic_forecast();x<-p[!is.na(p$Teams)&p$Teams%in%f$Team_ID,];z<-aggregate(cbind(hWAR,PA,IP)~Teams,x,sum,na.rm=TRUE);names(z)[1]<-"Team_ID";z<-merge(f[,c("Team_ID","Team_Name","Expected_Wins_2027","Projected_2027_Payroll_Millions")],z,by="Team_ID",all.x=TRUE);z<-z[order(-z$hWAR),];z[c("Expected_Wins_2027","Projected_2027_Payroll_Millions","hWAR","PA","IP")]<-lapply(z[c("Expected_Wins_2027","Projected_2027_Payroll_Millions","hWAR","PA","IP")],round,2);fo_table(z,15)},server=FALSE)
}


# Reported player movements are independent of whether salary accounting is complete.
fo_actual_moves <- function(x,f,ratings=NULL) {
  stopifnot(all(c("Player","From","To")%in%names(x)))
  ids<-as.character(f$Team_ID); delta<-setNames(rep(0,length(ids)),ids)
  x$Projected_hWAR<-NA_real_;x$Profile_ID<-NA_character_
  for(i in seq_len(nrow(x))) {
    p<-fo_player_match(x$Player[i],x$From[i])
    if(nrow(p)==1 && is.finite(p$hWAR)) {
      x$Projected_hWAR[i]<-p$hWAR;x$Profile_ID[i]<-p$ID
      if(!is.na(x$From[i]) && x$From[i]%in%ids) delta[x$From[i]]<-delta[x$From[i]]-p$hWAR
      if(!is.na(x$To[i]) && x$To[i]%in%ids) delta[x$To[i]]<-delta[x$To[i]]+p$hWAR
    }
  }
  x$Coverage<-ifelse(is.finite(x$Projected_hWAR),"Projected MLB contribution","No MLB projection; excluded from subtotal")
  z<-if(any(is.finite(x$Projected_hWAR))) fo_matchup_change(f,delta,ratings=ratings) else NULL
  list(players=x,impacts=z,complete=all(is.finite(x$Projected_hWAR)))
}
fo_actual_ui <- function() fo_panel(h3("Player value and matchup impact"),p("Model 6 measures the players; Model 7 distributes expected wins and losses by opponent; Model 5 translates those changes into attendance and revenue. Financial terms are evaluated separately below."),
  uiOutput("fo_actual_status"),DTOutput("fo_actual_players"),plotlyOutput("fo_actual_chart",height="630px"),DTOutput("fo_actual_clubs"),
  p(class="fo-small","One-year MLB contribution, not prospect value, contract surplus or a definitive trade winner. No unknown player or salary is treated as measured zero. Retention comparison assumes outgoing players would otherwise stay."),uiOutput("fo_reported_asset_panel"))
fo_actual_server <- function(input,output,session,economic_forecast,signings,trades) {
  ratings<-reactive({f<-economic_forecast();fo_calibrate(f$Team_ID,f$Expected_Wins_2027)})
  result<-reactive({
    key<-input$tx_record;req(key);f<-economic_forecast()
    if(startsWith(key,"trade:")||startsWith(key,"movement:")){
      d<-trades()
      x<-if(startsWith(key,"trade:"))d[d$Trade_ID==substring(key,7),,drop=FALSE] else d[d$Movement_ID==substring(key,10),,drop=FALSE]
      validate(need(nrow(x)>0,"No recorded player movements for this selection."))
      m<-data.frame(Player=x$Player,From=x$From_Team_ID,To=x$To_Team_ID)
    } else if(startsWith(key,"signing:")||startsWith(key,"contract:")){
      d<-signings();k<-strsplit(key,":",fixed=TRUE)[[1]][2];x<-d[d$Signing_ID==k,,drop=FALSE]
      validate(need(nrow(x)==1,"No player transfer for this financial-only transaction."))
      validate(need(!identical(as.character(x$Status),"Withdrawn"),"This signing was withdrawn."))
      m<-data.frame(Player=x$Player,From=x$Previous_Team_ID,To=x$New_Team_ID)
      if(!is.na(m$From)&&m$From==m$To)m$From<-m$To
    } else validate(need(FALSE,"Select a reported signing or trade, or use the Front Office simulators for a hypothetical deal."))
    fo_actual_moves(m,f,ratings())
  })
  output$fo_reported_asset_panel<-renderUI({
    key<-input$tx_record;req(key)
    if(startsWith(key,"trade:"))fo_asset_ui("fo_actual_asset_")else NULL
  })
  fo_asset_server(input,output,session,"fo_actual_asset_",reactive({
    key<-input$tx_record;req(key);validate(need(startsWith(key,"trade:"),"Choose a reported trade."))
    x<-result()$players;p<-fo_players[fo_players$Season==2027,,drop=FALSE];idx<-match(x$Profile_ID,p$ID)
    data.frame(ID=x$Profile_ID,Player=x$Player,From=x$From,To=x$To,Age=p$Age[idx],WAR=x$Projected_hWAR,stringsAsFactors=FALSE)
  }),economic_forecast,ratings)
  output$fo_actual_status<-renderUI({
    q<-result();n<-sum(is.finite(q$players$Projected_hWAR))
    if(!n) return(fo_note("Player movements are recorded, but none have a matched MLB projection. Prospect-only deals require a separate minor-league valuation."))
    z<-q$impacts;clubs<-unique(c(q$players$From,q$players$To));a<-z[z$Team%in%clubs,]
    tagList(fo_note(if(q$complete)"All listed players have MLB projections. Contract and prospect value are not included." else paste(n,"of",nrow(q$players),"players have MLB projections. Results represent only the covered contribution; missing prospects remain unvalued.")),do.call(fo_kpis,lapply(seq_len(nrow(a)),function(i)fo_kpi(paste(a$Team[i],"expected wins"),sprintf("%+.2f",a$Win_Change[i]),paste("Revenue",sprintf("%+.2fM",a$Revenue_Change_M[i]))))))
  })
  output$fo_actual_players<-renderDT({q<-result();x<-q$players;z<-data.frame(Player=vapply(seq_len(nrow(x)),function(i)as.character(fo_name_link(x$Player[i],x$From[i])),character(1)),From=x$From,To=x$To,hWAR=round(x$Projected_hWAR,2),Coverage=x$Coverage);fo_format_table(DT::datatable(z,rownames=FALSE,escape=-1,options=list(dom="t",scrollX=TRUE)),z)},server=FALSE)
  output$fo_actual_chart<-renderPlotly({q<-result();validate(need(!is.null(q$impacts),"No covered MLB contribution to plot."));z<-q$impacts;z<-z[order(z$Win_Change),];plot_ly(z,x=~Win_Change,y=~factor(Team,levels=Team),type="bar",orientation="h",marker=list(color=ifelse(z$Win_Change>=0,"#2c8666","#b65c56")),hovertext=~Division) %>% layout(xaxis=list(title="Expected win change"),yaxis=list(title="",type="category",tickmode="array",tickvals=z$Team,ticktext=z$Team,categoryorder="array",categoryarray=z$Team),margin=list(l=65,t=10))})
  output$fo_actual_clubs<-renderDT({q<-result();validate(need(!is.null(q$impacts),"No covered MLB contribution to value."));z<-q$impacts;z<-z[order(-abs(z$Win_Change)),c("Team","Division","Before_Wins","After_Wins","Win_Change","Loss_Change","Attendance_Change","Revenue_Change_M")];z[-c(1,2)]<-lapply(z[-c(1,2)],round,3);fo_table(z,10)},server=FALSE)
}


# One player profile, shared across leaders, reported transactions and financial pages.
fo_profile_context_ui <- function() tagList(
  tags$script(HTML("$(document).on('click','.fo-transaction-link',function(e){e.preventDefault();Shiny.setInputValue('fo_open_transaction',String($(this).attr('data-fo-transaction')),{priority:'event'});});")),
  uiOutput("fo_profile_identity"))
fo_projection_research_ui <- function() fo_panel(h3("Projection research"),
  p("Performance and playing time are separate estimates. The expected and healthy workload scenarios use the same open-data historical inputs. The expected scenario flows into trade and signing calculations by default."),
  DTOutput("fo_projection_versions"),
  p(class="fo-small","Two-year holdouts compare matched players against their origin-season value. Healthy workload is a scenario, not a prediction interval. Recorded-error fielding is not a range metric; 2026 is not observed."),
  uiOutput("fo_projection_audit"))
fo_related_ui <- function() fo_panel(h3("Related transactions and contracts"),
  p("Open the original transaction analysis without losing the player connection. Amounts come from the reviewed transaction and annual contract ledgers; blanks mean unknown."),
  DTOutput("fo_related_transactions"),DTOutput("fo_related_contracts"))
fo_profile_context_server <- function(input,output,session,signings,trades,open_transaction) {
  profile<-reactive({req(input$fo_player);x<-fo_players[fo_players$ID==input$fo_player,,drop=FALSE];validate(need(nrow(x)>0,"No player data available."));x[order(x$Season),,drop=FALSE]})
  val<-function(x,digits=2){if(length(x)!=1||!is.finite(x)) "—" else fo_num(x,digits)}
  output$fo_profile_identity<-renderUI({
    x<-profile();q<-x[nrow(x),,drop=FALSE];h<-x[x$Season<2027 & is.finite(x$hWAR),,drop=FALSE]
    div(class="fo-profile-head",div(div(class="eyebrow","PLAYER CARD"),h2(q$Player),p(paste(na.omit(c(q$Teams,q$Position,if(is.finite(q$Age))paste("Age",q$Age))),collapse=" · "))),
      div(class="fo-profile-facts",div(span("Historical seasons"),strong(nrow(h))),div(span("Career hWAR*"),strong(val(sum(h$hWAR)))),div(span("Best season*"),strong(if(nrow(h))paste(h$Season[which.max(h$hWAR)],"·",val(max(h$hWAR)))else "—"))),
      p(class="fo-small","*Coverage-limited historical series. Team label is last known season affiliation, not confirmed 2027 roster."))
  })
  read_candidate<-function() {
    path<-"data/hwar_preview/projection_candidates.csv"
    if(!file.exists(path))return(NULL)
    d<-read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
    if(!all(c("ID","Expected_hWAR","Healthy_hWAR","Expected_PA","Expected_IP","Healthy_PA","Healthy_IP","Legacy_2027_hWAR")%in%names(d)))return(NULL)
    d$ID<-as.character(d$ID);if(anyDuplicated(d$ID))return(NULL);d
  }
  candidate<-read_candidate()
  output$fo_projection_versions<-renderDT({
    x<-profile();q<-x[x$Season==2027,,drop=FALSE];validate(need(nrow(q)==1,"No 2027 projection for this player."))
    z<-data.frame(Version=c("Active expected","Active healthy scenario"),hWAR=c(q$hWAR,q$Healthy_hWAR),PA=c(q$PA,q$Healthy_PA),IP=c(q$IP,q$Healthy_IP),Basis=c("Current partial-coverage model","Current talent at healthier workload"))
    if(!is.null(candidate)){a<-candidate[candidate$ID==q$ID,,drop=FALSE];if(nrow(a)==1)z<-rbind(z,data.frame(Version=c("Multi-year candidate expected","Multi-year candidate healthy","Earlier full-component prototype"),hWAR=c(a$Expected_hWAR,a$Healthy_hWAR,a$Legacy_2027_hWAR),PA=c(a$Expected_PA,a$Healthy_PA,a$Legacy_2027_PA),IP=c(a$Expected_IP,a$Healthy_IP,a$Legacy_2027_IP),Basis=c("Separate experimental model; see audit","Same candidate rate, healthy workload","Previous local R output; not directly comparable")))}
    z[c("hWAR","PA","IP")]<-lapply(z[c("hWAR","PA","IP")],round,2);fo_table(z,10)
  },server=FALSE)
  output$fo_projection_audit<-renderUI({
    path<-"data/hwar_preview/open_data_audit.json"
    if(!file.exists(path))return(fo_note("Open-data audit not installed."))
    a<-jsonlite::fromJSON(path)
    z<-a$two_year_holdouts
    fo_note(paste("Chronological two-year holdouts, RMSE in wins (model vs origin-season baseline):",paste(sprintf("%d: %.2f vs %.2f (%d matched players)",z$target,z$model_rmse,z$baseline_rmse,z$n),collapse="; "),". Historical estimates, not independent true player value."))
  })
  tx_link<-function(key,label) as.character(tags$a(href="#",class="fo-transaction-link",`data-fo-transaction`=key,label))
  src<-function(u){if(length(u)==1&&!is.na(u)&&grepl("^https?://",u))as.character(tags$a(href=u,target="_blank",rel="noopener noreferrer","Report"))else "—"}
  output$fo_related_transactions<-renderDT({
    x<-profile();name<-x$Player[nrow(x)];key<-fo_name_key(name);validate(need(length(unique(fo_players$ID[fo_name_key(fo_players$Player)==key]))==1,"Name is ambiguous in player data; transaction matching is disabled."))
    a<-signings();a<-a[which(fo_name_key(a$Player)==key),,drop=FALSE];b<-trades();b<-b[which(fo_name_key(b$Player)==key),,drop=FALSE]
    s<-data.frame(Date=character(),Type=character(),From=character(),To=character(),Status=character(),Analysis=character(),Source=character())
    if(nrow(a)){k<-vapply(seq_len(nrow(a)),function(i){r<-a[i,];ledger<-contract_ledger();if(any(ledger$Contract_ID==r$Signing_ID & ledger$Club==r$New_Team_ID & ledger$Obligation_Season==2027,na.rm=TRUE))paste("contract",r$Signing_ID,r$New_Team_ID,2027,sep=":")else paste0("signing:",r$Signing_ID)},character(1));s<-rbind(s,data.frame(Date=as.character(a$Reported_Date),Type="Signing",From=a$Previous_Team_ID,To=a$New_Team_ID,Status=a$Status,Analysis=vapply(k,tx_link,character(1),label="Analyze signing"),Source=vapply(a$Source_URL,src,character(1))))}
    if(nrow(b))s<-rbind(s,data.frame(Date=as.character(b$Transaction_Date),Type="Trade",From=b$From_Team_ID,To=b$To_Team_ID,Status=b$Status,Analysis=vapply(paste0("trade:",b$Trade_ID),tx_link,character(1),label="Analyze trade"),Source=vapply(b$Source_URL,src,character(1))))
    validate(need(nrow(s)>0,"No matching records in the current offseason transaction tracker."));DT::datatable(s[order(s$Date,decreasing=TRUE),],rownames=FALSE,escape=c(1,2,3,4,5),options=list(pageLength=10,scrollX=TRUE))
  },server=FALSE)
  output$fo_related_contracts<-renderDT({
    x<-profile();name<-x$Player[nrow(x)];key<-fo_name_key(name);validate(need(length(unique(fo_players$ID[fo_name_key(fo_players$Player)==key]))==1,"Name is ambiguous in player data."));d<-contract_ledger();d<-d[which(fo_name_key(d$Player)==key),,drop=FALSE];validate(need(nrow(d)>0,"No matching annual contract rows in the reviewed ledger."))
    cols<-intersect(c("Club","Obligation_Season","Transaction_Type","Salary_M","CBT_Charge_M","Snapshot_Reconciliation"),names(d));fo_table(d[,cols,drop=FALSE],10)
  },server=FALSE)
  observeEvent(input$fo_open_transaction,{key<-as.character(input$fo_open_transaction);req(length(key)==1,grepl("^(trade|signing|contract):",key));open_transaction(key)},ignoreInit=TRUE)
}
