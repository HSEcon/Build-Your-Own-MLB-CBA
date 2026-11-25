# Transaction impact UI and server helpers. All calculations use saved snapshots.
transaction_impact_ui <- function(teams) {
  div(class="tx-center",div(class="page-head",div(class="eyebrow","MLB FINANCIAL DATA"),h2("Transaction Economics"),
    p("Explore how a signing, trade or arbitration agreement changes team finances and the next CBA.")),
  div(class="tx-workspace",div(class="dashboard-panel tx-controls",h3("Analyze a transaction"),
    radioButtons("tx_filter","Transaction type",c("All","Free agency","Trades","Arbitration","Other"),inline=TRUE),
    selectInput("tx_record","Select a reported transaction or build your own",choices=c("Hypothetical transaction"="hypothetical")),
    textOutput("tx_evidence"),
    uiOutput("tx_missing_note"),
    conditionalPanel("input.tx_two && !output.tx_ready", actionButton("tx_trade_example","Run illustrative $5M transfer",class="btn-default"),p(class="small-note","Example only: add $5M to the first club, subtract $5M from the second, with equal CBT changes. Not a reported trade amount.")),
    radioButtons("tx_direction","Comparison",c("Add to saved payroll (what-if)"="add","Remove from saved payroll (what-if)"="reverse"),inline=TRUE),
    fluidRow(column(4,selectInput("tx_team1","Team",teams,selected="BAL")),
      column(4,numericInput("tx_cash1","Annual payroll change ($M)",25,step=.1)),
      column(4,numericInput("tx_cbt1","Luxury tax payroll change ($M)",25,step=.1))),
    checkboxInput("tx_two","Include another team (trade)",FALSE),
    conditionalPanel("input.tx_two",fluidRow(column(4,selectInput("tx_team2","Second team",teams,selected="NYY")),
      column(4,numericInput("tx_cash2","Annual payroll change ($M)",-25,step=.1)),
      column(4,numericInput("tx_cbt2","Luxury tax payroll change ($M)",-25,step=.1)))),
    selectInput("tx_year","CBA rules effective in",c("2027","2028")),
    checkboxInput("tx_confirm","Use my estimates for unknown contract terms",FALSE),
    p(class="small-note","All comparisons use the partial 2027 Live Payroll scenario (2026 carry-forward and reviewed adjustments). 2028 tests later CBA rules, not a separate payroll forecast. Enter net salary changes after retention and replacement costs; CBT is separate. No edits are saved.")),
  div(class="tx-results",conditionalPanel("input.tx_record != 'hypothetical'",fo_actual_ui()),uiOutput("tx_status"),conditionalPanel("output.tx_ready",uiOutput("tx_summary"),
  div(class="dashboard-panel",h3("Team economic impact"),p("Model 1 predicts changes from payroll, not individual talent. Model 7 distributes the payroll-based win proxy across the schedule. This is an alternative to the player-based estimate above, not an additional win effect."),
    DTOutput("tx_team_effects")),
  div(class="dashboard-panel",h3("How the deal changes under each CBA"),
    p("The threshold comparison holds payroll fixed. The modeled results include clubs' spending responses, salary rules and revenue sharing. Tax amounts use the simulator's simplified rates, not exact legal tax bills."),
    h4("Payroll thresholds before and after"),DTOutput("tx_rules"),h4("Team impact after modeled spending responses"),DTOutput("tx_policy_results"),h4("League-wide impact"),DTOutput("tx_league_results"))))),
  div(class="dashboard-panel",h3("Team financial dashboard"),selectInput("tx_focus_team","Club",teams,selected="BAL"),
    uiOutput("tx_team_finances"),DTOutput("tx_team_contracts")),
  div(class="dashboard-panel",h3("Contract value and present value"),
    p("Optional finance analysis: enter actual or hypothetical annual cash payments in millions. Deferred payments belong in the years they are paid; a signing bonus paid immediately goes in year 0. This does not calculate official CBT valuation."),
    textInput("tx_payments","Cash payments by year, starting at year 0","0,25,25,25,25,25"),
    sliderInput("tx_discount","Annual discount rate (%)",0,15,5,step=.5),uiOutput("tx_pv")))
}

transaction_offseason_ui <- function() {
  tagList(div(class="page-head",div(class="eyebrow","MLB FINANCIAL DATA"),h2("Offseason Economics"),
    p("See how reviewed payroll adjustments change modeled league spending, inequality and predicted competitive balance. This is not a complete offseason accounting.")),
    uiOutput("offseason_metrics"),
    div(class="dashboard-panel",h3("Payroll and payroll inequality over time"),plotlyOutput("offseason_plot",height="340px"),
      p(class="small-note","Each point is a saved snapshot, not a complete daily transaction history.")),
    div(class="dashboard-panel",h3("Largest team payroll changes"),DTOutput("offseason_teams")),
    div(class="dashboard-panel",h3("Arbitration settlements"),p("Actual salary versus prior salary and published projection. Missing terms are not zero."),DTOutput("offseason_arb")),
    div(class="dashboard-panel",h3("Best Compromise tracker"),p("Run the existing optimizer on the earliest and latest saved payroll snapshots with identical CBA settings. This compares two economic scenarios; it does not automatically save a recommendation or run costly searches on every page load."),
      actionButton("offseason_compare_best","Compare best compromises",class="btn-primary"),uiOutput("offseason_best")))
}

register_transaction_economics <- function(input,output,session,reference,current_policy,comparison_context,signings,trades) {
  catalog <- reactive(tx_contract_catalog(contract_ledger(),signings(),trades()))
  chosen <- reactive(catalog()[[input$tx_record %||% "hypothetical"]] %||% catalog()[["hypothetical"]])
  observe({
    c <- catalog()
    keys <- names(c);labels <- vapply(c,"[[",character(1),"label")
    f <- input$tx_filter %||% "All"
    keep <- switch(f,"Free agency"=grepl("Free agency|Qualifying offer",labels),"Trades"=grepl("^(trade|movement):",keys),
      "Arbitration"=grepl("Arbitration",labels),"Other"=!grepl("Free agency|Qualifying offer|Arbitration",labels)&!grepl("^(trade|movement):",keys),rep(TRUE,length(keys)))
    keep[keys=="hypothetical"] <- TRUE
    choices <- setNames(keys[keep],labels[keep]);current <- isolate(input$tx_record %||% "hypothetical")
    updateSelectInput(session,"tx_record",choices=choices,selected=if(current %in% choices)current else "hypothetical")
  })
  observeEvent(input$tx_record,{
    x <- chosen()
    updateSelectInput(session,"tx_team1",selected=x$teams[1])
    updateNumericInput(session,"tx_cash1",value=if(is.finite(x$cash[1]))x$cash[1]else 0)
    updateNumericInput(session,"tx_cbt1",value=if(is.finite(x$cbt[1]))x$cbt[1]else 0)
    updateCheckboxInput(session,"tx_two",value=length(x$teams)==2)
    if(length(x$teams)==2){
      updateSelectInput(session,"tx_team2",selected=x$teams[2])
      updateNumericInput(session,"tx_cash2",value=if(is.finite(x$cash[2]))x$cash[2]else 0)
      updateNumericInput(session,"tx_cbt2",value=if(is.finite(x$cbt[2]))x$cbt[2]else 0)
    }
    updateRadioButtons(session,"tx_direction",selected=x$direction)
    updateCheckboxInput(session,"tx_confirm",value=FALSE)
  })
  output$tx_evidence <- renderText(chosen()$note)
  output$tx_missing_note <- renderUI({
    x <- chosen()
    if(any(!is.finite(c(x$cash,x$cbt)))) div(class="plain-box","Unknown amounts appear as editable zero placeholders, not reported values. Enter and confirm estimates to calculate.")
  })
  observeEvent(input$tx_trade_example,{
    req(isTRUE(input$tx_two),input$tx_team1,input$tx_team2,input$tx_team1!=input$tx_team2)
    updateNumericInput(session,"tx_cash1",value=5)
    updateNumericInput(session,"tx_cbt1",value=5)
    updateNumericInput(session,"tx_cash2",value=-5)
    updateNumericInput(session,"tx_cbt2",value=-5)
    updateRadioButtons(session,"tx_direction",selected="add")
    updateCheckboxInput(session,"tx_confirm",value=TRUE)
  },ignoreInit=TRUE)
  readiness <- reactive({
    x <- chosen()
    if(!all(is.finite(c(x$cash,x$cbt))) && !isTRUE(input$tx_confirm))
      return(list(message="Full financial terms are not available. Enter your own net payroll and CBT estimates for each club, then confirm them, or choose the illustrative transfer example for a trade. Only the financial comparison is blocked. Available player-based estimates are shown independently above."))
    ids <- input$tx_team1;cash <- input$tx_cash1;cbt <- input$tx_cbt1
    if(isTRUE(input$tx_two)){ids <- c(ids,input$tx_team2);cash <- c(cash,input$tx_cash2);cbt <- c(cbt,input$tx_cbt2)}
    if(length(ids)==0||length(ids)!=length(cash)||length(ids)!=length(cbt)||anyDuplicated(ids)||!all(is.finite(c(cash,cbt)))||is.null(input$tx_direction))
      return(list(message="Enter finite payroll changes for distinct teams."))
    p <- current_policy()
    z <- tryCatch(tx_pair(reference(),ids,cash,cbt,input$tx_direction,p$payroll_coef,p$attendance_coef),error=function(e)NULL)
    if(is.null(z))return(list(message="Changes must leave positive payrolls and valid projected winning percentages."))
    list(pair=z,ids=ids,message=NULL)
  })
  output$tx_ready <- reactive(is.null(readiness()$message))
  outputOptions(output,"tx_ready",suspendWhenHidden=FALSE)
  output$tx_status <- renderUI({
    state <- readiness()
    if(!is.null(state$message)) div(class="dashboard-panel",h3(chosen()$label),p(state$message),p(class="small-note","The saved payroll baseline is unchanged."))
  })
  pair <- reactive({
    state <- readiness()
    req(is.null(state$message))
    list(pair=state$pair,ids=state$ids)
  })
  rows <- reactive(tx_team_effects(pair()$pair,current_policy()$payroll_coef))
  active <- reactive({r <- rows();r[r$Team_ID %in% pair()$ids,,drop=FALSE]})
  comparisons <- reactive({
    p <- current_policy(); context <- comparison_context(); context$scenario_year <- as.integer(input$tx_year %||% "2027")
    policies <- lapply(list("Current CBA"=current_cba_policy(),"MLB proposal"=mlb_proposal_policy(),"MLBPA proposal"=mlbpa_proposal_policy(),"Your CBA"=p),function(q)modifyList(complete_policy(q),context))
    tx_policy_effects(pair()$pair,policies,p$payroll_coef,input$behavior_response %||% 1)
  })
  stat <- function(label,value,help="")div(class="tx-stat",div(class="metric-label",label),strong(value),p(help))
  dollars <- function(x)paste0("$",format(round(x,2),nsmall=2,trim=TRUE),"M")
  tab <- function(x,cols=NULL) {
    q <- app_table(x,rownames=FALSE,options=list(dom="t",scrollX=TRUE,paging=FALSE,ordering=FALSE))
    q
  }
  output$tx_summary <- renderUI({
    r <- active();v <- tx_league_effects(pair()$pair)
    div(class="dashboard-panel",h3(chosen()$label),p(if(isTRUE(input$tx_confirm))"Illustrative scenario using your estimates, not confirmed contract terms or an actual payroll update."else"What-if comparison using the saved 2027 projection; not a confirmed payroll update."),
      div(class="tx-stat-grid",stat("League payroll change",money_text(v[["Payroll"]],2)),
        stat("Payroll Gini change",sprintf("%+.4f",v[["Gini_After"]]-v[["Gini_Before"]])),
        stat("Competitive balance change",percent_text(v[["Balance"]],2)),
        stat("Projected league revenue change",money_text(v[["Revenue"]],2))),
      lapply(seq_len(nrow(r)),function(i)div(class="tx-team-line",strong(r$Club[i]),
        span(paste(dollars(r$Payroll_Before[i]),"→",dollars(r$Payroll_After[i]))),
        span(paste(sprintf("%+.2f",r$Wins_Change[i]),"predicted wins")),
        span(paste(money_text(r$Revenue_Change[i],2),"revenue")),
        span(paste(format(round(r$Attendance_Change[i]),big.mark=","),"attendance change")))))
  })
  output$tx_team_effects <- renderDT({
    r <- active()
    d <- data.frame(Team=r$Team_ID,`Payroll before ($M)`=r$Payroll_Before,`Payroll after ($M)`=r$Payroll_After,
      `Predicted wins change`=r$Wins_Change,`Gross marginal wins`=r$Gross_Wins,
      `Revenue change ($M)`=r$Revenue_Change,`Model 2 revenue ($M)`=r$Model2_Revenue_Change,
      `Attendance change`=round(r$Attendance_Change),`Efficiency before`=r$Efficiency_Before,`Efficiency after`=r$Efficiency_After,
      `Cost per gross win ($M)`=r$Cost_Per_Gross_Win,`Revenue less payroll ($M)`=r$Net_Revenue_Less_Payroll,check.names=FALSE)
    tab(d,names(d)[-1])
  })
  output$tx_rules <- renderDT({
    d <- comparisons()$team;d <- d[d$Team_ID %in% pair()$ids,]
    x <- data.frame(CBA=d$Agreement,Team=d$Team_ID,`Upper rule`=d$Upper_Rule,
      `Room before ($M)`=d$Room_Before,`Room after ($M)`=d$Room_After,`Lower gap before ($M)`=d$Floor_Gap_Before,
      `Lower gap after ($M)`=d$Floor_Gap_After,`CBT change ($M)`=d$Static_CBT_Change,`Low tax change ($M)`=d$Static_Low_Tax_Change,check.names=FALSE)
    tab(x,names(x)[-(1:3)])
  })
  output$tx_policy_results <- renderDT({
    d <- comparisons()$team;d <- d[d$Team_ID %in% pair()$ids,]
    x <- data.frame(CBA=d$Agreement,Team=d$Team_ID,`Modeled payroll change ($M)`=d$Modeled_Payroll_Change,
      `Wins change`=d$Modeled_Wins_Change,`Revenue change ($M)`=d$Modeled_Revenue_Change,
      `Attendance change`=round(d$Modeled_Attendance_Change),`CBT change ($M)`=d$Modeled_CBT_Change,
      `Low tax change ($M)`=d$Modeled_Low_Tax_Change,`Direct club effect ($M)`=d$Club_Effect_Change,check.names=FALSE)
    tab(x,names(x)[-(1:2)])
  })
  output$tx_league_results <- renderDT({
    d <- comparisons()$league;names(d) <- c("CBA","Player pay change ($M)","Gini change","Balance change (%)","Revenue change ($M)","Direct club effect ($M)")
    tab(d,names(d)[-1])
  })
  output$tx_team_finances <- renderUI({
    b <- reference();i <- match(input$tx_focus_team,b$Team_ID);req(length(i)==1,!is.na(i))
    div(class="tx-stat-grid",stat("Projected payroll",dollars(b$Payroll_M[i])),
      stat("Projected CBT payroll",dollars(b$CBT_Payroll_M[i])),
      stat("Projected wins",sprintf("%.2f",162*b$Win_Pct[i])),
      stat("Projected revenue",dollars(b$Revenue_Millions[i])))
  })
  output$tx_team_contracts <- renderDT({
    req(input$tx_focus_team);d <- contract_ledger();d <- d[d$Club==input$tx_focus_team,]
    tab(data.frame(Player=d$Player,Type=d$Transaction_Type,Season=d$Obligation_Season,Salary=d$Salary_M,Status=d$Status),"Salary")
  })
  output$tx_pv <- renderUI({
    x <- suppressWarnings(as.numeric(trimws(strsplit(input$tx_payments %||% "",",",fixed=TRUE)[[1]])))
    validate(need(length(x)>0&&all(is.finite(x)&x>=0),"Enter nonnegative cash payments separated by commas."))
    div(class="tx-stat-grid",stat("Nominal cash total",dollars(sum(x))),stat("Present value",dollars(tx_present_value(x,seq_along(x)-1,(input$tx_discount %||% 5)/100))))
  })
  history <- reactive(tx_history(reference()))
  history_pair <- reactive({h <- history();list(before=h[[1]]$baseline,after=h[[length(h)]]$baseline)})
  output$offseason_metrics <- renderUI({
    h <- history();v <- tx_league_effects(history_pair());s <- signings();known <- s$Total_Value_Millions[is.finite(s$Total_Value_Millions)]
    tagList(p(paste("Saved snapshots:",h[[1]]$date,"to",h[[length(h)]]$date,"·",length(h),"snapshots")),
      div(class="tx-stat-grid",stat("Reported contract value",dollars(sum(known)),paste(length(known),"reported values; multi-year total")),
        stat("Projected 2027 payroll change",money_text(v[["Payroll"]],2)),
        stat("Payroll Gini",sprintf("%.4f → %.4f",v[["Gini_Before"]],v[["Gini_After"]])),
        stat("Competitive balance change",percent_text(v[["Balance"]],2))))
  })
  output$offseason_plot <- renderPlotly({
    h <- history();x <- seq_along(h);dates <- vapply(h,"[[",character(1),"date")
    pay <- vapply(h,function(z)sum(z$baseline$Payroll_M),numeric(1));g <- vapply(h,function(z)gini(z$baseline$Payroll_M),numeric(1))
    p1 <- plot_ly(x=x,y=pay,type="scatter",mode="lines+markers",name="Payroll ($M)")%>%layout(yaxis=list(title="Payroll ($M)"))
    p2 <- plot_ly(x=x,y=g,type="scatter",mode="lines+markers",name="Gini coefficient")%>%layout(yaxis=list(title="Gini coefficient"))
    subplot(p1,p2,nrows=2,shareX=TRUE,titleY=TRUE)%>%layout(xaxis=list(tickvals=x,ticktext=dates,title="Saved snapshot"),showlegend=FALSE)%>%config(displaylogo=FALSE)
  })
  output$offseason_teams <- renderDT({
    d <- tx_team_effects(history_pair());d <- d[order(abs(d$Payroll_Change),decreasing=TRUE),]
    cols <- c("Club","Payroll_Before","Payroll_After","Payroll_Change","Wins_Change","Revenue_Change","Attendance_Change")
    tab(d[,cols],cols[-1])
  })
  output$offseason_arb <- renderDT({
    d <- contract_ledger();d <- d[d$Transaction_Type=="Arbitration settlement" & d$Obligation_Season==2027,]
    z <- data.frame(Player=d$Player,Team=d$Club,`Salary ($M)`=d$Salary_M,`Prior salary ($M)`=d$Prior_Salary_M,
      `Projection ($M)`=d$Published_Projection_M,`Raise ($M)`=d$Salary_Change_From_Prior_M,`Difference from projection ($M)`=d$Difference_From_Projection_M,check.names=FALSE)
    tab(z,names(z)[-(1:2)])
  })
  best_cache <- reactiveVal(NULL)
  observeEvent(input$offseason_compare_best,{
    h <- history();p <- current_policy();response <- input$behavior_response %||% 1
    result <- tryCatch(withProgress(message="Comparing saved payroll snapshots",value=0,{
      lapply(c(1,length(h)),function(i) {
        opt <- optimize_cba(p,h[[i]]$baseline,behavior=response)
        incProgress(.5);list(date=h[[i]]$date,opt=opt)
      })
    }),error=function(e)list(error=conditionMessage(e)))
    best_cache(result)
  })
  output$offseason_best <- renderUI({
    q <- best_cache();if(is.null(q))return(p("Run the comparison to see how the recommendation changes."))
    if(!is.null(q$error))return(p(q$error))
    tagList(p("Results use the settings at the time of this search. Run again after changing settings."),
      lapply(q,function(z)div(class="dashboard-panel",h4(z$date),p(policy_label(z$opt$policy)),
        p(paste("MLB",score_text(z$opt$scores$mlb),"· MLBPA",score_text(z$opt$scores$mlbpa))),
        p(paste("Competitive balance",percent_text(z$opt$simulation$metrics$competitive_balance_change,2),
          "· Player pay",money_text(z$opt$simulation$metrics$player_comp_change,2))),
        p(if(z$opt$qualified)"Meets the economic requirements."else"No qualifying package found."))))
  })
  function(key) {
    if(!key %in% names(catalog()))return()
    removeModal();updateRadioButtons(session,"nav_group",selected="MLB Financial Data")
    updateRadioButtons(session,"finance_tab",selected="Transaction Economics")
    updateRadioButtons(session,"tx_filter",selected="All")
    updateSelectInput(session,"tx_record",choices=setNames(names(catalog()),vapply(catalog(),"[[",character(1),"label")),selected=key)
  }
}
