library(shiny)
library(tidyverse)
library(plotly)
library(DT)
source("trade_ledger.R", local = TRUE)
source("scenario_accounting.R", local = TRUE)
source("bargaining_model.R", local = TRUE)
source("optimizer.R", local = TRUE)
source("research_validation.R", local = TRUE)
source("transaction_impact.R", local = TRUE)
source("transaction_ui.R", local = TRUE)
source("front_office_preview.R", local = TRUE)
source("matchup_preview.R", local = TRUE)
source("player_financial_bridge.R", local = TRUE)

payroll_data <- read_csv("mlb_model1_project_data_2011_2025.csv")
revenue_data <- read_csv("mlb_model2_project_data_2017_2025.csv")

payroll_effect <- 0.0273665
payroll_effect_se <- 0.0144992
revenue_effect <- 0.4535044

gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}

clamp <- function(x, low, high) {
  pmax(low, pmin(high, x))
}

rescale01 <- function(x) {
  good <- is.finite(x)
  
  if (sum(good) == 0) {
    return(rep(0.5, length(x)))
  }
  
  lo <- min(x[good])
  hi <- max(x[good])
  
  if (abs(hi - lo) < 0.0000001) {
    return(rep(0.5, length(x)))
  }
  
  result <- (x - lo) / (hi - lo)
  result[!good] <- 0.5
  result
}

fmt1 <- function(x) {
  sprintf("%.1f", x)
}

fmt2 <- function(x) {
  sprintf("%.2f", x)
}

fmt3 <- function(x) {
  sprintf("%.3f", x)
}

money_text <- function(x, digits = 1) {
  number <- if (digits == 1) {
    sprintf("%.1f", abs(x))
  } else {
    sprintf("%.2f", abs(x))
  }
  
  paste0(
    ifelse(x >= 0, "+$", "-$"),
    number,
    "M"
  )
}

percent_text <- function(x, digits = 1) {
  number <- if (digits == 1) {
    sprintf("%.1f", abs(x))
  } else {
    sprintf("%.2f", abs(x))
  }
  
  paste0(
    ifelse(x > 0, "+", ifelse(x < 0, "-", "")),
    number,
    "%"
  )
}

score_text <- function(x) {
  paste0(
    sprintf("%.1f", x),
    "/100"
  )
}

metric_card <- function(label, output_id, explanation) {
  div(
    class = "metric",
    div(
      class = "metric-label",
      label
    ),
    div(
      class = "metric-value",
      textOutput(output_id)
    ),
    div(
      class = "metric-help",
      explanation
    )
  )
}

formula_box <- function(tex) {
  withMathJax(
    div(
      class = "formula-box",
      HTML(
        paste0(
          "\\[",
          tex,
          "\\]"
        )
      )
    )
  )
}

historical_efficiency <- payroll_data %>%
  filter(
    Year >= 2016,
    Year <= 2025,
    Year != 2020
  ) %>%
  mutate(
    Relative_Winning = Win_Pct / 0.500,
    Payroll_Efficiency = Relative_Winning / Payroll_Index
  ) %>%
  group_by(Team_ID) %>%
  summarise(
    Avg_Historical_Efficiency = mean(
      Payroll_Efficiency,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

base <- payroll_data %>%
  filter(Year == 2025) %>%
  transmute(
    Team_ID,
    Team_Name,
    Payroll_M = Payroll_Opening_Day / 1000000,
    Win_Pct
  ) %>%
  inner_join(
    revenue_data %>%
      filter(Year == 2025) %>%
      select(
        Team_ID,
        Revenue_Millions
      ),
    by = "Team_ID"
  ) %>%
  left_join(
    historical_efficiency,
    by = "Team_ID"
  )

base_avg_payroll <- mean(base$Payroll_M)
base_total_payroll <- sum(base$Payroll_M)
base_gini <- gini(base$Payroll_M)
baseline_win_spread <- sd(base$Win_Pct * 162)
baseline_prearb_pool <- 50
baseline_modeled_comp <- base_total_payroll + baseline_prearb_pool

base <- base %>%
  mutate(
    Model3_Wins_From_25M =
      payroll_effect *
      (
        log(
          (Payroll_M + 25) /
            base_avg_payroll
        ) -
          log(
            Payroll_M /
              base_avg_payroll
          )
      ) *
      162
  )

current_cba_policy <- function() {
  list(
    upper_rule = "cbt",
    upper_value = 244,
    upper_tax_rate = 20,
    lower_rule = "none",
    lower_value = NA_real_,
    lower_tax_rate = 0,
    revenue_sharing = 20,
    required_spending = 0,
    central_media = FALSE,
    player_revenue_share = 50,
    minimum_salary_under2 = 0.78,
    minimum_salary_2plus = 0.78,
    prearb_pool = 50,
    arbitration = "3",
    free_agency = "6",
    qualifying_offer = TRUE
  )
}

mlb_proposal_policy <- function() {
  list(
    upper_rule = "cap",
    upper_value = 258.2,
    upper_tax_rate = 0,
    lower_rule = "floor",
    lower_value = 180.2,
    lower_tax_rate = 0,
    revenue_sharing = 20,
    required_spending = 0,
    central_media = TRUE,
    player_revenue_share = 50,
    minimum_salary_under2 = 1.00,
    minimum_salary_2plus = 1.00,
    prearb_pool = 65,
    arbitration = "3",
    free_agency = "5_age30",
    qualifying_offer = FALSE,
    cap_start=2028,media_start=2028,arbitration_bonus_pct=5,
    arbitration_bonus_start=2028,veteran_bonus=0.5,prearb_min_bonus=0.1
  )
}

# Illustrative union scenario; unconfirmed rates are assumptions.
mlbpa_proposal_policy <- function() {
  p <- current_cba_policy()
  p$upper_value <- 300
  p$lower_rule <- "tax"
  p$lower_value <- 150
  p$lower_tax_rate <- 25
  p$required_spending <- 50
  p$central_media <- TRUE
  p$minimum_salary_under2 <- 1.50
  p$minimum_salary_2plus <- 1.50
  p$prearb_pool <- 180
  p$free_agency <- "5_age30"
  p$qualifying_offer <- FALSE
  p$media_start <- 2027
  p$super_two_pct <- 44
  p$min_arb_salary <- 3
  p
}

simulate_policy <- function(
    policy,
    payroll_coef = payroll_effect,
    behavior_multiplier = 1,
    full = TRUE,
    baseline = base
) {
  policy <- complete_policy(policy)
  base <- baseline
  base_avg_payroll <- mean(base$Payroll_M)
  base_total_payroll <- sum(base$Payroll_M)
  baseline_win_spread <- sd(base$Win_Pct*162)
  baseline_modeled_comp <- base_total_payroll+50
  cap_deferred <- policy$upper_rule=="cap" && policy$scenario_year<policy$cap_start
  if(cap_deferred) {
    policy$upper_rule <- "cbt"
    policy$upper_value <- 244
    policy$upper_tax_rate <- 20
    if(policy$lower_rule=="floor") policy$lower_rule <- "none"
  }
  transfers <- policy_transfers(policy,base)
  costs <- player_pay_costs(policy,nrow(base))
  cbt_offset <- if("CBT_Payroll_M" %in% names(base)) base$CBT_Payroll_M-base$Payroll_M else rep(0,nrow(base))
  net_transfer <- transfers$net

  required_payroll_from_sharing <-
    pmax(
      net_transfer,
      0
    ) *
    (
      policy$required_spending /
        100
    )
  
  new_payroll <-
    base$Payroll_M + costs$minimum + costs$earlier_arb + costs$arb_floor +
    required_payroll_from_sharing
  
  if (policy$lower_rule == "floor") {
    new_payroll <-
      pmax(
        new_payroll,
        policy$lower_value - cbt_offset
      )
  }
  
  if (policy$lower_rule == "tax") {
    lower_gap <-
      pmax(
        policy$lower_value -
          new_payroll - cbt_offset,
        0
      )
    
    lower_response <-
      clamp(
        (
          policy$lower_tax_rate /
            100
        ) *
          behavior_multiplier,
        0,
        1
      )
    
    new_payroll <-
      new_payroll +
      lower_gap *
      lower_response
  }
  
  if (policy$upper_rule == "cap") {
    new_payroll <-
      pmin(
        new_payroll,
        policy$upper_value - cbt_offset
      )
  }
  
  if (policy$upper_rule == "cbt") {
    upper_excess <-
      pmax(
        new_payroll + cbt_offset -
          policy$upper_value,
        0
      )
    
    upper_response <-
      clamp(
        (
          policy$upper_tax_rate /
            100
        ) *
          behavior_multiplier,
        0,
        1
      )
    
    new_payroll <-
      new_payroll -
      upper_excess *
      upper_response
  }
  
  new_payroll <- pmax(new_payroll,1)
  scenario_avg_payroll <- mean(new_payroll)
  
  current_relative <-
    base$Payroll_M /
    base_avg_payroll
  
  new_relative <-
    new_payroll /
    scenario_avg_payroll
  
  raw_change_win_pct <-
    payroll_coef *
    (
      log(new_relative) -
        log(current_relative)
    )
  
  change_win_pct <-
    raw_change_win_pct -
    mean(raw_change_win_pct)
  
  change_wins <-
    change_win_pct *
    162
  
  new_win_pct <-
    base$Win_Pct +
    change_win_pct
  
  new_wins <-
    new_win_pct *
    162
  
  attendance_change <- if("Attendance" %in% names(base)) base$Attendance*expm1(policy$attendance_coef*change_win_pct) else rep(NA_real_,nrow(base))
  revenue_change <- if("Attendance" %in% names(base)) {
    base$Revenue_Millions*expm1(policy$revenue_growth-0.047417+0.156374*policy$attendance_coef*change_win_pct+0.141378*change_win_pct)
  } else
    base$Revenue_Millions *
    (
      exp(
        revenue_effect *
          change_win_pct
      ) -
        1
    )
  
  cbt_tax <-
    rep(
      0,
      nrow(base)
    )
  
  low_spending_tax <-
    rep(
      0,
      nrow(base)
    )
  
  if (policy$upper_rule == "cbt") {
    cbt_tax <-
      pmax(
        new_payroll + cbt_offset -
          policy$upper_value,
        0
      ) *
      (
        policy$upper_tax_rate /
          100
      )
  }
  
  if (policy$lower_rule == "tax") {
    low_spending_tax <-
      pmax(
        policy$lower_value -
          new_payroll - cbt_offset,
        0
      ) *
      (
        policy$lower_tax_rate /
          100
      )
  }
  
  payroll_change <-
    new_payroll -
    base$Payroll_M
  
  added_payroll <-
    pmax(
      payroll_change,
      0
    )
  
  if (sum(added_payroll) > 0) {
    recipient_efficiency <-
      weighted.mean(
        base$Avg_Historical_Efficiency,
        added_payroll,
        na.rm = TRUE
      )
    
    recipient_marginal_wins <-
      weighted.mean(
        base$Model3_Wins_From_25M,
        added_payroll,
        na.rm = TRUE
      )
  } else {
    recipient_efficiency <- NA_real_
    recipient_marginal_wins <- NA_real_
  }
  
  total_payroll <- sum(new_payroll)
  
  payroll_change_total <-
    total_payroll -
    base_total_payroll
  
  new_gini <-
    gini(new_payroll)
  
  new_win_spread <-
    sd(new_wins)
  
  competitive_balance_change <-
    (
      (
        baseline_win_spread -
          new_win_spread
      ) /
        baseline_win_spread
    ) *
    100
  
  modeled_player_comp <-
    total_payroll + policy$prearb_pool + sum(costs$arbitration_bonus+costs$veteran_bonus)
  
  total_revenue_change <-
    sum(revenue_change)
  
  direct_club_effect <-
    total_revenue_change - payroll_change_total - sum(costs$prearb_delta+costs$arbitration_bonus+costs$veteran_bonus)
  
  low_revenue <- base$Revenue_Millions <= median(base$Revenue_Millions)
  metrics <- list(
    score_context=list(baseline=base,assumptions=policy[bargaining_assumptions],coef=payroll_coef,response=behavior_multiplier),
    revenue_gini_reduction=gini(base$Revenue_Millions)-gini(base$Revenue_Millions+transfers$net),
    low_revenue_payroll_change=sum(payroll_change[low_revenue]),
    worst_club_effect_share=min((revenue_change+transfers$net-payroll_change-costs$prearb_delta-costs$arbitration_bonus-costs$veteran_bonus)/base$Revenue_Millions),
    baseline_gini=gini(base$Payroll_M),baseline_win_spread=baseline_win_spread,
    minimum_salary_cost=sum(costs$minimum),earlier_arb_cost=sum(costs$earlier_arb),arbitration_floor_cost=sum(costs$arb_floor),prearb_pool=policy$prearb_pool,
    prearb_delta=sum(costs$prearb_delta),prearb_min_allocation=policy$prearb_min_bonus*policy$prearb_eligible*nrow(base),
    prearb_remaining=policy$prearb_pool-policy$prearb_min_bonus*policy$prearb_eligible*nrow(base),
    arbitration_bonus=sum(costs$arbitration_bonus),veteran_bonus=sum(costs$veteran_bonus),
    media_redistributed=sum(pmax(transfers$media,0)),cap_deferred=cap_deferred,
    attendance_change=if(all(is.na(attendance_change))) NA_real_ else sum(attendance_change),
    total_payroll = total_payroll,
    payroll_change = payroll_change_total,
    player_comp = modeled_player_comp,
    player_comp_change =
      modeled_player_comp -
      baseline_modeled_comp,
    gini = new_gini,
    win_spread = new_win_spread,
    competitive_balance_change =
      competitive_balance_change,
    revenue_change =
      total_revenue_change,
    club_effect =
      direct_club_effect,
    cbt_tax =
      sum(cbt_tax),
    low_spending_tax =
      sum(low_spending_tax),
    recipient_efficiency =
      recipient_efficiency,
    recipient_marginal_wins =
      recipient_marginal_wins
  )
  
  if (!full) {
    return(
      list(
        metrics = metrics
      )
    )
  }
  
  team <- tibble(
    Team_ID = base$Team_ID,
    Team_Name = base$Team_Name,
    Original_Payroll_M = base$Payroll_M,
    New_Payroll_M = new_payroll,
    Payroll_Change_M = payroll_change,
    Original_Wins = base$Win_Pct * 162,
    Predicted_Wins = new_wins,
    Change_Wins = change_wins,
    Historical_Efficiency =
      base$Avg_Historical_Efficiency,
    Model3_Wins_From_25M =
      base$Model3_Wins_From_25M,
    Revenue_Change_M = revenue_change,
    Media_Transfer_M=transfers$media,Ordinary_Revenue_Transfer_M=transfers$ordinary,
    Minimum_Salary_Increment_M=costs$minimum,Earlier_Arbitration_Cost_M=costs$earlier_arb,Arbitration_Floor_Cost_M=costs$arb_floor,
    Prearb_Pool_Cost_Change_M=costs$prearb_delta,Arbitration_Bonus_M=costs$arbitration_bonus,
    Veteran_Bonus_M=costs$veteran_bonus,Predicted_Attendance_Change=attendance_change,
    Projected_CBT_Payroll_M=new_payroll+cbt_offset,
    Direct_Club_Effect_M=revenue_change+transfers$net-payroll_change-costs$prearb_delta-costs$arbitration_bonus-costs$veteran_bonus,
    CBT_Exposure_M =
      cbt_tax,
    Low_Spending_Tax_M =
      low_spending_tax
  )
  
  list(
    team = team,
    metrics = metrics
  )
}

policy_label <- function(policy) {
  upper <-
    switch(
      policy$upper_rule,
      "cap" = "Salary Cap",
      "cbt" = "Competitive Balance Tax (CBT)",
      "none" = "No Upper Payroll Restriction"
    )
  
  lower <-
    switch(
      policy$lower_rule,
      "floor" = "Payroll Floor",
      "tax" = "Low-Spending Tax",
      "none" = "No Lower Payroll Restriction"
    )
  
  paste0(
    upper,
    " + ",
    lower
  )
}

research_section <- function(n,title,summary,...) {
  div(id=paste0("research-",n),class="research-section",
    div(class="research-section-head",span(class="research-number",sprintf("%02d",n)),div(h3(title),p(summary))),
    div(class="research-body",...))
}

app_table <- function(data,...,selection="none") {
  t<-DT::datatable(data,...,selection=selection,colnames=fo_pretty_label(names(data)))
  fo_format_table(t,data)
}

ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
        html, body {
          height: 100%;
          margin: 0;
          overflow: hidden;
          background: #f5f6f8;
        }

        .container-fluid {
          padding: 0;
          height: 100vh;
        }

        .app-header {
          flex-shrink: 0;
          height: 95px;
          padding: 16px 28px;
          background: #142b43;
          color: white;
          border-bottom: 4px solid #477dab;
          height: 128px;
        }

        .app-title {
          font-size: 28px;
          font-weight: 700;
        }

        .app-subtitle {
          color: #b9c9d9;
          margin-top: 4px;
          font-size: 14px;
        }

        .dashboard-row {
          height: 100%;
          margin: 0;
        }

        .control-col {
          height: 100%;
          overflow-y: auto;
          padding: 18px 20px 80px 20px;
          background: white;
          border-right: 1px solid #dddddd;
        }

        .results-col {
          height: 100%;
          overflow-y: auto;
          padding: 18px 28px 90px 28px;
        }

        .control-card {
          border: 1px solid #dddddd;
          border-radius: 9px;
          padding: 15px;
          margin-bottom: 16px;
          background: #fafafa;
        }

        .preset-box {
          border: 1px solid #cdd8e4;
          border-radius: 9px;
          padding: 15px;
          margin-bottom: 16px;
          background: white;
        }

        .metric {
          border: 1px solid #dddddd;
          border-radius: 9px;
          padding: 14px;
          margin-bottom: 15px;
          background: white;
          min-height: 125px;
        }

        .metric-label {
          font-size: 12px;
          font-weight: 700;
          text-transform: uppercase;
          color: #555555;
        }

        .metric-value {
          font-size: 25px;
          font-weight: 700;
          margin-top: 5px;
          overflow-wrap: anywhere;
        }

        .metric-help {
          font-size: 12px;
          color: #666666;
          margin-top: 7px;
          line-height: 1.4;
        }

        .section-title {
          font-size: 19px;
          font-weight: 700;
          margin-bottom: 12px;
        }

        .best-box {
          border: 2px solid #333333;
          border-radius: 10px;
          padding: 20px;
          background: white;
          margin: 15px 0;
        }

        .best-title {
          font-size: 27px;
          font-weight: 700;
          margin-bottom: 15px;
        }

        .deal-section {
          border-top: 1px solid #dddddd;
          padding-top: 12px;
          margin-top: 12px;
        }

        .deal-row {
          padding: 6px 0;
          font-size: 15px;
        }

        .plain-box {
          background: #f7f7f7;
          border-left: 4px solid #777777;
          padding: 13px 16px;
          margin: 14px 0;
        }

        .important-box {
          background: white;
          border: 2px solid #555555;
          border-radius: 9px;
          padding: 16px;
          margin: 14px 0;
        }

        .model-details {
          background: white;
          border: 1px solid #dddddd;
          border-radius: 8px;
          margin-bottom: 12px;
          padding: 12px 15px;
        }

        .model-details summary {
          cursor: pointer;
          font-size: 18px;
          font-weight: 700;
        }

        .formula-box {
          background: #f7f7f7;
          border: 1px solid #dddddd;
          border-radius: 8px;
          padding: 7px 12px;
          margin: 12px 0;
          overflow-x: auto;
        }

        .model-flow {
          display: flex;
          align-items: center;
          flex-wrap: wrap;
          gap: 8px;
          margin: 18px 0;
        }

        .flow-box {
          border: 1px solid #bbbbbb;
          border-radius: 8px;
          padding: 10px 14px;
          background: white;
          font-weight: 700;
        }

        .flow-arrow {
          font-size: 22px;
          font-weight: 700;
        }

        .small-note {
          color: #666666;
          font-size: 12px;
        }

        .btn {
          margin-right: 5px;
          margin-bottom: 5px;
        }

        .dashboard-row {
          height: calc(100vh - 128px);
          display: flex;
        }

        .dashboard-row:before,
        .dashboard-row:after {
          display: none;
        }

        .dashboard-row > .shiny-panel-conditional {
          flex: 0 0 33.333333%;
          min-width: 0;
          height: 100%;
        }

        .dashboard-row > .shiny-panel-conditional > .control-col {
          width: 100%;
          float: none;
        }

        .dashboard-row > .results-col {
          flex: 1 1 0;
          min-width: 0;
          width: auto;
          float: none;
        }

        body { font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif; color: #172b3e; background: #f2f5f8; }
        .app-header .shiny-input-radiogroup { margin: 8px 0 0; }
        .app-header .radio-inline { color: #d5e2ef; font-weight: 600; margin-left: 0; margin-right: 20px; }
        .app-header .control-label { display: none; }
        .app-header input[type=radio] { accent-color: #94c5f1; }
        .preset-box .btn { width: calc(50% - 6px); min-height: 41px; white-space: normal; font-size: 12px; color: #245d91; border-color: #cdd8e4; }
        .preset-box .btn:focus { color: white; background: #245d91; }
        .control-card { background: white; border-color: #dce3ea; padding: 12px 14px; }
        .control-card summary { cursor: pointer; list-style: none; }
        .control-card summary:after { content: '+'; float: right; color: #72869b; }
        .control-card[open] summary:after { content: '−'; }
        .section-title { font-size: 15px; color: #172b3e; }
        .metric { border-color: #dce3ea; box-shadow: 0 2px 7px #142b4309; }
        .metric-value { color: #245d91; }
        .nav-tabs > li.active > a { color: #245d91; font-weight: 700; border-top: 3px solid #245d91; }
        .nav-tabs > li > a { color: #52667b; font-weight: 600; }
        .btn-primary { background: #245d91; border-color: #245d91; }
        .plain-box { border-left-color: #477dab; background: #edf3f8; }
        .important-box, .best-box { border: 1px solid #cdd8e4; }
        .results-col h2 { font-size: 25px; letter-spacing: -.3px; }
        .irs-bar, .irs-single, .irs-from, .irs-to { background: #245d91 !important; }
        .model-details summary { color: #245d91; }

        .page-head { margin-bottom:18px; }
        .page-head h2 { margin:0 0 6px;font-weight:700;font-size:24px; }
        .page-head p { color:#657387;font-size:13px; }
        .eyebrow { color:#657f9b;font-size:10px;letter-spacing:1.4px;font-weight:700;margin-bottom:6px; }
        .dashboard-panel,.research-section { background:white;border:1px solid #dce3ea;border-radius:7px;padding:16px 18px;margin:0 0 14px; }
        .dashboard-panel h3 { font-size:16px;font-weight:700;margin:0 0 8px; }
        .dashboard-panel p { color:#657387;font-size:13px; }
        .research-jump { display:flex;flex-wrap:wrap;gap:6px;margin:0 0 16px; }
        .research-jump a { padding:5px 10px;border:1px solid #dce3ea;border-radius:4px;background:white;font-size:12px;color:#245d91; }
        .research-section { scroll-margin-top:12px; }
        .research-section-head { display:flex;gap:12px;align-items:flex-start;border-bottom:1px solid #e7ecf1;padding-bottom:10px;margin-bottom:13px; }
        .research-number { font-size:13px;font-weight:700;padding:7px 9px;border-radius:5px;background:#edf3f8;color:#245d91; }
        .research-section h3 { font-size:20px;margin:0 0 5px;font-weight:700; }
        .research-section-head p { color:#657387;margin:0; }
        .research-body { max-width:1000px;line-height:1.55; }
        .research-body h4 { font-size:15px;font-weight:700;margin:18px 0 7px; }
        .research-finding { background:#edf3f8;padding:10px 14px;border-left:3px solid #245d91;margin:12px 0; }
        .research-weights,.detail-grid { display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px; }
        .formula-box { background:#f7f9fb; }
        #transaction_view + .tab-content { padding-top:12px; }
        .transaction-detail { background:#f7f9fb;border:1px solid #dce3ea;border-radius:6px;padding:15px;margin-top:14px; }
        .detail-grid { grid-template-columns:repeat(3,minmax(0,1fr)); }
        .detail-grid > div { padding:8px;border-bottom:1px solid #dce3ea;overflow-wrap:anywhere; }
        .detail-label { display:block;font-size:11px;color:#657387; }
        .detail-notes { margin-top:12px;line-height:1.5;overflow-wrap:anywhere; }
        .dashboard-panel table.dataTable th,.dashboard-panel table.dataTable td { padding:8px 9px!important;vertical-align:middle;line-height:1.35;white-space:nowrap; }
        .dashboard-panel table.dataTable { font-size:12px; }
        .dashboard-panel table.dataTable tbody tr.selected { background:#dceaf6!important;color:#172b3e!important; }
        @media(max-width:900px){ .detail-grid,.research-weights{grid-template-columns:1fr} }


        .app-header { height:136px;padding:18px 30px 0;border-bottom:4px solid #477dab; }
        .app-header .shiny-input-radiogroup { margin:22px 0 0;width:100%; }
        .app-header .radio-inline,.section-nav .radio-inline { padding:10px 0 12px;margin:0 23px 0 0;cursor:pointer; }
        .app-header .radio-inline input,.section-nav .radio-inline input { position:absolute;opacity:0;width:1px; }
        .app-header .radio-inline:has(input:checked) { border-bottom:3px solid #a6c6e4;color:white; }
        .app-header .radio-inline:has(input:focus-visible),.section-nav .radio-inline:has(input:focus-visible) { outline:2px solid #91b7dd; }
        .section-nav { height:49px;background:white;border-bottom:1px solid #dce3ea;padding:0 28px;overflow-x:auto; }
        .section-nav .shiny-input-radiogroup { margin:0;width:max-content; }
        .section-nav .radio-inline { color:#657387;font-weight:600;padding:15px 0 10px;font-size:13px; }
        .section-nav .radio-inline:has(input:checked) { color:#245d91;border-bottom:3px solid #245d91; }
        .dashboard-row { height:calc(100vh - 185px); }
        .dashboard-row > .shiny-panel-conditional { flex:0 0 clamp(280px,24.3vw,350px); }
        .control-col { padding:18px 18px 48px;border-color:#dce3ea; }
        .results-col { padding:23px 26px 65px; }
        .preset-box { padding:0 0 14px;border:0;border-bottom:1px solid #dce3ea;border-radius:0;margin-bottom:12px; }
        .preset-box .section-title { font-size:18px; }
        .preset-box .btn { color:#172b3e;text-align:left;font-weight:600; }
        .preset-box #reset_inputs { background:#245d91;color:white; }
        .control-card { border:0;border-bottom:1px solid #dce3ea;border-radius:0;margin:0;padding:14px 0 15px; }
        .control-card .section-title { font-size:13px; }
        .control-col .control-label { font-size:12px; }
        .control-col .form-group { margin-bottom:12px; }
        .control-col .form-control { border-color:#cdd8e4;box-shadow:none;font-size:12px; }
        .metric { min-height:112px;padding:13px 14px;border-radius:7px;box-shadow:none; }
        .metric-label { font-size:11px;text-transform:none;color:#52667b;min-height:25px; }
        .metric-value { font-size:24px;color:#172b3e; }
        .metric-help { font-size:11px;color:#657387; }
        .support-row { margin:11px 0; }.support-label { display:flex;justify-content:space-between;font-size:12px; }
        .support-track { height:7px;background:#e5ebf1;border-radius:5px;margin-top:5px;overflow:hidden; }
        .support-fill { background:#245d91;height:100%;border-radius:5px; }
        @media(max-width:900px){ .app-header{height:auto;padding-bottom:6px}.app-header .shiny-input-radiogroup{margin-top:8px}.section-nav{height:auto;min-height:49px}.dashboard-row{height:auto;display:block}.metric{min-height:0}.results-col,.control-col{padding:15px}.section-nav .radio-inline{margin-right:14px} }


        .container-fluid { display:flex;flex-direction:column;height:100vh;min-height:0; }
        .app-header { height:auto;flex:0 0 auto;padding:16px 26px 0; }
        .app-header .shiny-input-radiogroup,.section-nav .shiny-input-radiogroup { width:100%;margin:8px 0 0; }
        .section-nav { height:auto;min-height:45px;flex:0 0 auto;overflow:visible;padding:0 26px; }
        .section-nav .shiny-input-radiogroup { margin:0; }
        .app-header .shiny-options-group,.section-nav .shiny-options-group { display:flex;flex-wrap:wrap;align-items:center;gap:0 22px; }
        .app-header .radio,.section-nav .radio { display:inline-flex;margin:0; }
        #nav_group label,.section-nav label { position:relative;display:inline-flex;align-items:center;margin:0!important;padding:10px 0!important;white-space:nowrap;cursor:pointer;font-weight:600;border-bottom:3px solid transparent; }
        #nav_group label { color:#d5e2ef; } .section-nav label { color:#52667b;font-size:13px; }
        #nav_group label:has(input:checked) { border-color:#a6c6e4;color:white; }
        .section-nav label:has(input:checked) { border-color:#245d91;color:#245d91; }
        #nav_group input,.section-nav input { position:absolute!important;opacity:0;width:1px!important;height:1px!important;margin:0!important; }
        .dashboard-row { flex:1 1 auto;height:auto;min-height:0; }
        .control-col,.results-col { min-height:0; }
        .control-card { padding:14px 0; }.control-card .section-title { font-size:15px;margin-bottom:12px; }
        .assumption-grid { display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:16px;margin-bottom:18px; }
        .assumption-grid .shiny-input-container { width:100%; }
        .assumption-card { padding:16px;border:1px solid #dce3ea;border-radius:7px;background:white; }
        .assumption-card h3 { font-size:16px;margin:0 0 10px; }
        .primary-metrics .metric { border-top:3px solid #245d91;background:linear-gradient(180deg,#fff,#f9fbfd); }
        .dashboard-panel { box-shadow:0 2px 8px #162d4310; }
        @media(max-width:900px) { html,body{height:auto;overflow:auto}.container-fluid{height:auto;min-height:100vh;display:block}.dashboard-row{display:block;height:auto}.app-header,.section-nav{padding-left:15px;padding-right:15px} }

        .dashboard-panel table.dataTable td { white-space:normal;overflow-wrap:break-word;word-break:normal; }
        table.dataTable td.dt-right,table.dataTable td.dt-type-numeric,table.dataTable td.dt-body-right,table.dataTable td.dt-type-date { white-space:nowrap!important;overflow-wrap:normal!important;word-break:normal!important;font-variant-numeric:tabular-nums; }
        .fo-panel table.dataTable td { overflow-wrap:break-word;word-break:normal; }
        .dashboard-panel table.dataTable tbody tr { cursor:default; }
        #transaction_view ~ .tab-content table.dataTable tbody tr { cursor:pointer; }
        .trade-movement { border-top:1px solid #e0e6ec;padding:9px 0; }.trade-movement p { margin:4px 0;font-size:13px; }
        .trade-sides { display:grid;grid-template-columns:1fr 1fr;gap:14px;margin:10px 0 20px; }.trade-sides > div {background:white;border:1px solid #dce3ea;border-radius:7px;padding:12px;}
        .modal-dialog { max-width:95vw; }.modal-body { max-height:70vh;overflow-y:auto; }
        @media(max-width:900px) {.trade-sides{grid-template-columns:1fr}}

        /* Compact sports-analytics masthead; use real data, not ornamental metrics. */
        .app-header { display:flex;align-items:center;justify-content:space-between;gap:18px;padding:12px 25px 0;background:linear-gradient(112deg,#142b43,#173b53);border-bottom:3px solid #7095b6; }
        .brand-lockup { display:flex;align-items:center;gap:12px;padding-bottom:12px;min-width:0; }
        .baseball-mark { flex:0 0 42px;display:grid;place-items:center; }
        .app-title { font-size:23px;line-height:1.16;letter-spacing:-.5px; }
        .app-subtitle { font-size:11px;line-height:1.3;letter-spacing:.25px;margin-top:4px; }
        .app-header #nav_group { margin:0;width:auto;flex:0 0 auto;align-self:flex-end; }
        .app-header #nav_group .shiny-options-group { gap:0 20px; }
        .app-header #nav_group label { padding:12px 0 15px!important; }
        .app-header .shiny-input-container,.section-nav .shiny-input-container { margin-bottom:0!important; }
        .section-nav { min-height:44px; }
        .section-nav .shiny-options-group { gap:0 24px; }
        .section-nav label { padding:11px 0 9px!important; }
        .results-col { background:#f1f5f8; }
        .preset-grid { display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:7px;margin-bottom:9px; }
        .preset-grid .btn { width:100%;min-height:40px;margin:0!important;border-radius:6px;padding:7px 8px; }
        .preset-grid .btn:hover { border-color:#4b79a1;background:#edf4fa; }
        .preset-grid #reset_inputs:hover { background:#1b4b76; }
        .primary-metrics { display:flex;flex-wrap:wrap; }
        .primary-metrics > div { display:flex; }
        .primary-metrics .metric { width:100%;min-height:118px; }
        .primary-metrics .metric-value { font-size:clamp(18px,1.7vw,26px);font-variant-numeric:tabular-nums;letter-spacing:-.5px; }
        .primary-metrics .metric-label { min-height:26px; }
        .dashboard-panel { border-radius:9px; }
        .chart-legend { display:flex;align-items:center;flex-wrap:wrap;gap:6px;font-size:11px;color:#52667b;margin:0 0 4px; }
        .chart-legend span:nth-child(even) { margin-right:11px; }
        .legend-gain { color:#25816b; }.legend-loss{color:#b65b54;}.legend-neutral{color:#718398;}
        .control-card .section-title { border-left:3px solid #8aa9c2;padding-left:9px; }
        .research-section { border-left:3px solid #8aa9c2; }
        .trade-sides h4 { font-weight:700;color:#245d91; }
        @media(max-width:1100px){.app-header{flex-wrap:wrap;gap:0}.app-header #nav_group{width:100%}.app-header #nav_group label{padding:7px 0 9px!important}.brand-lockup{padding-bottom:4px}}
        @media(max-width:900px){.primary-metrics > div{width:50%}.primary-metrics .metric{min-height:105px}.app-title{font-size:21px}}
        @media(max-width:480px){.primary-metrics > div{width:100%}.primary-metrics .metric{min-height:0}}

        .tx-center { color:#172b3e; }
        .tx-workspace { display:grid;grid-template-columns:minmax(265px,1fr) minmax(0,3fr);gap:16px;align-items:start; }
        .tx-workspace .tx-controls { background:#162c43;color:#f5f8fb;border:1px solid #2a455e; }
        .tx-controls h3,.tx-controls .control-label,.tx-controls .checkbox,.tx-controls .radio-inline { color:#f5f8fb; }
        .tx-controls p,.tx-controls .shiny-text-output { color:#c5d4e1;font-size:12px;margin-bottom:12px; }
        .tx-controls .plain-box { color:#233b50; }
        .tx-results { min-width:0; }
        .tx-stat-grid { display:grid;grid-template-columns:repeat(auto-fit,minmax(135px,1fr));gap:10px;margin:10px 0 15px; }
        .tx-stat { background:linear-gradient(145deg,#162c43,#20445a);color:#f5f8fb;border-radius:8px;padding:14px; }
        .tx-stat .metric-label { color:#b8cfe1;min-height:28px;font-weight:500; }
        .tx-stat strong { display:block;font-size:21px;letter-spacing:-.5px;font-variant-numeric:tabular-nums;overflow-wrap:anywhere; }
        .tx-stat p { color:#c5d4e1;font-size:11px;margin:5px 0 0; }
        .tx-team-line { display:flex;flex-wrap:wrap;gap:12px;align-items:center;padding:12px 0;border-top:1px solid #dce3ea;font-size:13px; }
        .tx-team-line strong { min-width:130px; }
        @media(max-width:1100px){.tx-workspace{grid-template-columns:1fr}.tx-controls .shiny-input-container{width:100%}}
        @media (max-width: 900px) {
          html, body {
            overflow: auto;
          }

          .container-fluid {
            height: auto;
            min-height: 100vh;
          }

          .dashboard-row {
            display: block;
            height: auto;
          }

          .dashboard-row > .shiny-panel-conditional,
          .dashboard-row > .results-col,
          .control-col,
          .results-col {
            width: 100%;
            height: auto;
            overflow: visible;
          }
        }
      ")
    )
  ),
  
  div(
    class = "app-header",
    
    div(class="brand-lockup",
      div(class="baseball-mark",tags$svg(viewBox="0 0 48 48",width="38",height="38",role="presentation",
        tags$circle(cx=24,cy=24,r=21,fill="#faf8f0"),
        tags$path(d="M11 9 Q23 24 11 39 M37 9 Q25 24 37 39",fill="none",stroke="#aa5753",`stroke-width`=2),
        lapply(c(13,19,25,31,35),function(y)tags$path(d=paste0("M12 ",y," l6 -2 M30 ",y-2," l6 2"),stroke="#aa5753",`stroke-width`=1.6)))),
      div(div(class="app-title","Build Your Own MLB CBA"),div(class="app-subtitle","Baseball economics · 30 clubs · 2027 policy simulator"))),
    radioButtons("nav_group", NULL, c("CBA Simulator", "MLB Financial Data", "Player Analytics", "Front Office", "Research & Background"), inline = TRUE)
  ),
  div(class="section-nav",
    conditionalPanel("input.nav_group == 'CBA Simulator'",radioButtons("sim_tab",NULL,c("Results","Compare","Negotiation","Best Compromise","All 30 Clubs"),inline=TRUE)),
    conditionalPanel("input.nav_group == 'MLB Financial Data'",radioButtons("finance_tab",NULL,c("Live Payroll","Transactions"="Signings & Trades","Transaction Economics","Offseason Economics","2027 Economic Forecasts"),inline=TRUE)),
    conditionalPanel("input.nav_group == 'Player Analytics'",radioButtons("player_tab",NULL,c("hWAR Leaders","Player Profiles"),inline=TRUE)),
    conditionalPanel("input.nav_group == 'Front Office'",radioButtons("fo_tab",NULL,c("Trade Simulator","Signing Simulator","Projected Standings","Contract Valuation Lab","Free-Agent Lab"),inline=TRUE)),
    conditionalPanel("input.nav_group == 'Research & Background'",radioButtons("research_tab",NULL,c("CBA Explained"="Learn","Latest Negotiations"="CBA News","How the Model Works","Model Assumptions","Sources"),inline=TRUE))),
  
  fluidRow(
    class = "dashboard-row",
    
    conditionalPanel(
      condition = "!input.dashboard_tab || ['Results','Negotiation','Best Compromise','Compare','All 30 Clubs'].indexOf(input.dashboard_tab) >= 0",
      column(
      width = 4,
      class = "control-col",
      div(class="eyebrow","CBA POLICY CONTROLS"),
      
      div(
        class = "preset-box",
        
        div(
          class = "section-title",
          "Load a CBA Setup"
        ),
        
        div(class="preset-grid",
          actionButton("load_current","Current CBA"),actionButton("load_mlb","MLB Proposal"),
          actionButton("load_mlbpa","MLBPA Proposal"),actionButton("reset_inputs","Create Your Own CBA")),
        
        p(
          class = "small-note",
          "Choose a starting agreement, then adjust any provision. Results update automatically."
        ),
        selectInput("baseline_mode","Comparison baseline",c("2027 Live Payroll scenario"="2027","2025 historical research"="2025"),selected="2027"),
        selectInput("scenario_year","Policy effective year",c("2027","2028"),selected="2027"),
        p(class="small-note","2028 tests later provisions against the same baseline, not a 2028 forecast.")
      ),
      
      div(class="control-card",div(class="section-title","1. High-Payroll Rule"),
        
        selectInput(
          "upper_rule",
          "Upper payroll restriction",
          choices =
            c(
              "Salary Cap" = "cap",
              "Competitive Balance Tax (CBT)" = "cbt",
              "No Upper Payroll Restriction" = "none"
            ),
          selected = "cbt"
        ),
        
        conditionalPanel(
          condition = "input.upper_rule == 'cap'",
          
          sliderInput(
            "cap_value",
            "Salary Cap ($M)",
            min = 180,
            max = 400,
            value = 258.2,
            step = 0.1
          ),
          selectInput("cap_start","Cap/floor implementation begins",c("2027","2028"))
        ),
        
        conditionalPanel(
          condition = "input.upper_rule == 'cbt'",
          
          sliderInput(
            "cbt_value",
            "CBT Threshold ($M)",
            min = 180,
            max = 350,
            value = 244,
            step = 1
          ),
          
          sliderInput(
            "cbt_rate",
            "CBT Rate (%)",
            min = 0,
            max = 100,
            value = 20,
            step = 5
          )
        )
      ),
      
      div(class="control-card",div(class="section-title","2. Low-Payroll Rule"),
        
        selectInput(
          "lower_rule",
          "Lower payroll restriction",
          choices =
            c(
              "Payroll Floor" = "floor",
              "Low-Spending Tax" = "tax",
              "No Lower Payroll Restriction" = "none"
            ),
          selected = "none"
        ),
        
        conditionalPanel(
          condition = "input.lower_rule == 'floor'",
          
          sliderInput(
            "floor_value",
            "Payroll Floor ($M)",
            min = 50,
            max = 220,
            value = 180.2,
            step = 0.1
          )
        ),
        
        conditionalPanel(
          condition = "input.lower_rule == 'tax'",
          
          sliderInput(
            "low_benchmark",
            "Low-Payroll Benchmark ($M)",
            min = 50,
            max = 220,
            value = 150,
            step = 1
          ),
          
          sliderInput(
            "low_tax_rate",
            "Low-Spending Tax Rate (%)",
            min = 0,
            max = 100,
            value = 25,
            step = 5
          )
        )
      ),
      
      div(class="control-card",div(class="section-title","3. Revenue Structure"),
        
        sliderInput(
          "revenue_sharing",
          "Revenue Sharing (%)",
          min = 0,
          max = 50,
          value = 20,
          step = 5
        ),
        
        sliderInput(
          "required_spending",
          "Revenue-Sharing Transfer Required for Payroll (%)",
          min = 0,
          max = 100,
          value = 50,
          step = 5
        ),
        
        checkboxInput(
          "central_media",
          "Centralize Media Revenues",
          value = FALSE
        ),
        conditionalPanel("input.central_media",
          sliderInput("media_share_pct","Share of estimated media revenue pooled (%)",0,100,100,step=5),
          selectInput("media_start","Media sharing begins",c("2027","2028")),
          p(class="small-note","Team media revenue is unavailable; 20% is an illustrative assumption. Pooled media is excluded from ordinary revenue sharing.")),
        
        sliderInput(
          "player_revenue_share",
          "Illustrative Player Revenue Share (%) — not modeled as a negotiated wage rule",
          min = 40,
          max = 60,
          value = 50,
          step = 1
        )
      ),
      
      div(class="control-card",div(class="section-title","4. Player Pay"),
        
        sliderInput(
          "minimum_salary_under2",
          "Minimum Salary: Under 2 Years of Major League Service ($M)",
          min = 0.78,
          max = 2,
          value = 0.90,
          step = 0.01
        ),
        
        sliderInput(
          "minimum_salary_2plus",
          "Minimum Salary: 2+ Years of Major League Service ($M)",
          min = 0.78,
          max = 2,
          value = 1.00,
          step = 0.01
        ),
        
        sliderInput(
          "prearb_pool",
          "Pre-Arbitration Bonus Pool ($M)",
          min = 50,
          max = 200,
          value = 65,
          step = 5
        ),
        sliderInput("prearb_min_bonus","Minimum qualifying pre-arbitration bonus ($M)",0,0.25,0,step=0.01),
        sliderInput("arbitration_bonus_pct","Arbitration settlement bonus (%)",0,15,0,step=0.5),
        selectInput("arbitration_bonus_start","Settlement bonus begins",c("2027","2028"),selected="2028"),
        sliderInput("veteran_bonus","Veteran signing bonus per club ($M; assumed full take-up)",0,1,0,step=0.05),
        
        p(
          class = "small-note",
          "Costs use adjustable full-season-equivalent player counts, not observed service-time rosters. Central bonuses are separate from team payroll. The minimum bonus allocation below is inside the pool, not extra money."
        )
      ),
      
      div(class="control-card",div(class="section-title","5. Player Rights"),
        
        selectInput(
          "arbitration",
          "Salary Arbitration Eligibility",
          choices =
            c(
              "Generally Current Timing (~3 Years)" = "3",
              "Earlier Eligibility (~2 Years)" = "2"
            ),
          selected = "3"
        ),
        sliderInput("super_two_pct","Super Two eligibility share (%) — scenario assumption",22,100,22,step=1),
        sliderInput("min_arb_salary","Minimum arbitration salary ($M; 0 = no separate floor)",0,5,0,step=0.25),
        
        selectInput(
          "free_agency",
          "Free Agency / Period of Reserve",
          choices =
            c(
              "Generally 6 Years of Major League Service" = "6",
              "5 Years for Certain Age-30+ Players" = "5_age30"
            ),
          selected = "5_age30"
        ),
        
        checkboxInput(
          "qualifying_offer",
          "Keep Qualifying Offer / Draft Pick Compensation",
          value = FALSE
        ),
        
        p(
          class = "small-note",
          "Clubs can negotiate contract extensions with players at any time. These rules determine when an unsigned player can reach the open market."
        )
      ),
      
      div(class="control-card",div(class="section-title","Research assumptions"),
        sliderInput("behavior_response","Club response to taxes",.5,1.5,1,step=.1),
        p(class="small-note","1 = standard response. Other values test weaker or stronger reactions."),
        p(class="small-note","The other research inputs are available in Research & Background → Model Assumptions.")
      )
      )
    ),
    
    column(
      width = 8,
      class = "results-col",
      
      tabsetPanel(
        id = "dashboard_tab", type="hidden",
        selected = "Results",
        
        tabPanel(
          "Results",
          div(class="page-head",div(class="eyebrow","CBA SIMULATOR"),h2(textOutput("policy_name",inline=TRUE)),
            p("Economic effects across all 30 clubs, relative to the selected baseline."),textOutput("baseline_description")),
          uiOutput("policy_timing_note"),
          fluidRow(class="primary-metrics",
            column(3,metric_card("Competitive Balance Change","balance_change","Positive means closer team records.")),
            column(3,metric_card("Player Compensation Change","player_comp_change","Estimated change in total player pay.")),
            column(3,metric_card("Payroll Gini coefficient","gini","Lower means more equal payrolls.")),
            column(3,metric_card("Direct Club Financial Effect","club_effect","Change in revenue minus change in player pay; not total profit."))),
          div(class="dashboard-panel",h3("Payroll Change vs. Predicted Win Change"),
            p("Hover over a point to see the club, payroll change and predicted win change."),
            div(class="chart-legend",span(class="legend-gain","●"),span("Win gains"),span(class="legend-loss","●"),span("Win losses"),span(class="legend-neutral","●"),span("No change")),
            plotlyOutput("payroll_plot",height="330px"),
            p(class="small-note","Green: predicted win gains. Red: predicted win losses. Identical outcomes share a larger marker; hover to see the clubs. Points are never moved artificially.")),
          fluidRow(column(6,div(class="dashboard-panel",h3("Negotiation Summary"),uiOutput("result_support_bars"),
            p(class="small-note","Scores compare the economic results with the current CBA and both proposals. They are not probabilities of agreement."))),
            column(6,div(class="dashboard-panel",h3("Economic Interpretation"),uiOutput("what_changed")))),
          div(class="dashboard-panel",h3("Payroll inequality"),p("The Lorenz curve shows the share of total payroll held by clubs, from lowest to highest payroll. The closer the curve is to the diagonal, the more equal the payroll distribution."),
            plotlyOutput("payroll_lorenz",height="280px"),p(class="small-note","The Gini coefficient is the area between the Lorenz curve and the equality line divided by the area below the equality line.")),
          h3("Additional Economic Results"),
          fluidRow(column(3,metric_card("Win Total Spread","win_spread","Standard deviation of predicted club wins.")),
            column(3,metric_card("Total Player Compensation","player_comp","Payroll plus central bonuses.")),
            column(3,metric_card("Total Payroll Change","payroll_change","Across all clubs.")),
            column(3,metric_card("Predicted Revenue Change","revenue_change","One-season modeled effect."))),
          fluidRow(column(6,metric_card("CBT Exposure","cbt_exposure","Remaining modeled tax exposure.")),
            column(6,metric_card("Low-Spending Tax Exposure","low_tax_exposure","Remaining modeled penalty exposure."))),
          tags$details(class="model-details",tags$summary("Player Compensation and Revenue Redistribution"),DTOutput("compensation_breakdown")),
          div(class="dashboard-panel",h3("Largest Predicted Changes"),DTOutput("impact_table"))
        ),

        tabPanel(
          "Negotiation",
          
          br(),
          
          h2("Negotiation Analysis"),
          
          fluidRow(
            
            column(
              3,
              metric_card(
                "MLB Support",
                "mlb_score",
                "Weighted score based on competitive balance, club costs, revenue structure and alignment with MLB bargaining priorities."
              )
            ),
            
            column(
              3,
              metric_card(
                "MLBPA Support",
                "mlbpa_score",
                "Weighted score based on player compensation, labor-market freedom, pre-arbitration pay, player rights and spending incentives."
              )
            ),
            
            column(
              3,
              metric_card(
                "Compromise Score",
                "compromise_score",
                "Uses the weaker side's support because a CBA cannot be a strong compromise if either side strongly rejects it."
              )
            ),
            
            column(
              3,
              metric_card(
                "Estimated Negotiation Viability",
                "viability_score",
                "A model-based bargaining index that also penalizes large gaps between MLB and MLBPA support. It is not a literal probability of ratification."
              )
            )
          ),
          
          div(
            class = "important-box",
            
            h3(
              textOutput("negotiation_verdict")
            ),
            
            p(
              "These are structured bargaining indicators, not predictions of how individual owners or players will vote."
            )
          ),
          
          h3("How MLB's Score Is Built"),
          
          DTOutput("mlb_components"),
          
          h3("Why MLB Might Support or Oppose It"),
          
          uiOutput("mlb_reasons"),
          
          h3("How MLBPA's Score Is Built"),
          
          DTOutput("mlbpa_components"),
          h3("Bargaining score benchmark audit"),
          p("Compare each raw indicator with the Current CBA, MLB and MLBPA scenarios under the same empirical assumptions. Component scores are relative to these reference outcomes, not acceptance probabilities."),
          DTOutput("bargaining_benchmarks"),
          
          h3("Why the MLBPA Might Support or Oppose It"),
          
          uiOutput("mlbpa_reasons")
        ),
        
        tabPanel(
          "Best Compromise",
          
          br(),
          
          h2(
            "Best Economically Strong, Negotiable Compromise"
          ),
          
          div(
            class = "important-box",
            
            p(
              strong("How it works:"),
              " The simulator maximizes the existing Compromise Score (the weaker side’s support) among economically sound, realistic packages. The support indices incorporate modeled economic outcomes and bargaining positions. No minimum support-score cutoff applies."
            ),
            
            p(
              "The economic ranking does not automatically reward a package simply because it uses a Salary Cap, CBT, Payroll Floor or Low-Spending Tax."
            )
          ),
          
          actionButton(
            "find_best",
            "Run Best Compromise Search",
            class = "btn-primary"
          ),
          checkboxInput("lock_player_terms","Hold selected player pay and rights fixed",FALSE),
          tags$details(tags$summary("Recommendation constraints"),p("Manual sliders remain unrestricted. The optimizer requires a cap or CBT, a $220–300M cap beginning no earlier than 2028 with a compatible floor, or a $220–325M CBT with a 10–60% tax. Lower thresholds $120–190M; at least $50M gap between upper/lower thresholds; hard floor no more than 80% of cap. Sharing and compensation ranges are inspired by reported proposals, not agreed legal terms. Equal media pooling is preserved."),p("Minimum salaries $0.78–1.5M, pre-arbitration pool $50–180M with sufficient funding, Super Two 22–44%, arbitration floor up to $3M, settlement bonus up to 5%, veteran bonus up to $0.5M. Model-based cost guard: direct leaguewide club effect cannot fall below −5% of league revenue and no club below −15% of its revenue in any tested assumption. These are illustrative risk tolerances, not observed operating margins or a solvency guarantee.")),
          p(class="small-note","Find the highest support score among agreements that meet the economic and contract requirements. Test the result under different assumptions."),
          
          br(),
          br(),
          
          uiOutput("best_policy_box"),
          
          uiOutput("best_search_summary"),
          h3("Alternative objectives"),
          DTOutput("optimizer_alternatives"),
          selectInput("best_objective","Choose a package to apply",choices=c("Run the search first"="1")),
          
          fluidRow(
            
            column(
              4,
              metric_card(
                "Shortlisted Packages",
                "best_packages",
                "Promising agreements tested under different economic assumptions."
              )
            ),
            
            column(
              4,
              metric_card(
                "Assumptions per Package",
                "best_assumptions",
                "Each package is tested under three payroll-effect assumptions and three behavioral-response assumptions."
              )
            ),
            
            column(
              4,
              metric_card(
                "Total Simulations",
                "best_runs",
                "Initial agreements tested, plus tests under different assumptions."
              )
            )
          ),
          
          fluidRow(
            
            column(
              3,
              metric_card(
                "Payroll Gini coefficient",
                "best_gini",
                "Payroll inequality under the recommended package."
              )
            ),
            
            column(
              3,
              metric_card(
                "Win Total Spread",
                "best_win_spread",
                "Predicted standard deviation of club win totals."
              )
            ),
            
            column(
              3,
              metric_card(
                "Competitive Balance Change",
                "best_balance_change",
                "Positive means club records are closer together than the selected baseline."
              )
            ),
            
            column(
              3,
              metric_card(
                "Modeled Player Compensation",
                "best_player_comp",
                "Total simulated payroll plus the selected Pre-Arbitration Bonus Pool and centrally funded signing bonuses."
              )
            )
          ),
          
          fluidRow(
            
            column(
              3,
              metric_card(
                "MLB Support",
                "best_mlb_support",
                "Modeled bargaining support from MLB and club interests."
              )
            ),
            
            column(
              3,
              metric_card(
                "MLBPA Support",
                "best_mlbpa_support",
                "Modeled bargaining support from the players' side."
              )
            ),
            
            column(
              3,
              metric_card(
                "Compromise Score",
                "best_compromise_score",
                "Uses the weaker side's support."
              )
            ),
            
            column(
              3,
              metric_card(
                "Estimated Negotiation Viability",
                "best_viability",
                "Model-based bargaining viability. This is not a literal probability that the CBA will be accepted."
              )
            )
          ),
          
          h3("Why the Model Chose It"),
          
          uiOutput("best_reason"),
          
          actionButton(
            "apply_best",
            "Apply Selected Package to Controls"
          )
        ),
        
        tabPanel(
          "Compare",
          
          br(),
          
          h2(
            "Compare CBA Setups"
          ),
          
          p(
            "Compare the selected CBA setup with the three fixed starting scenarios and the recommended compromise. Changes to the sliders affect the selected setup, not the fixed columns."
          ),
          
          DTOutput("comparison_table"),
          
          p(
            class = "small-note",
            "Run Best Compromise to add the recommended system to the comparison."
          )
        ),
        
        tabPanel(
          "All 30 Clubs",
          
          br(),
          
          h2("All 30 MLB Clubs"),
          
          p(
            "Click a numeric column once to sort highest-to-lowest. Click it again to sort lowest-to-highest."
          ),
          
          selectInput("club_detail", "Explore a club", choices = setNames(base$Team_ID, base$Team_Name)),
          DTOutput("club_detail_table"),
          h3("All club results"),
          DTOutput("team_table")
        ),
        
        tabPanel(
          "How the Model Works",
          div(class="page-head",div(class="eyebrow","RESEARCH & BACKGROUND"),h2("How I Built the Model"),
            p("Eight connected research models, from payroll and the CBA to player valuation, opponent strength and contract economics.")),
          div(class="research-jump",lapply(seq_along(c("Question & data","Model 1","Model 2","Model 3","Model 4","Model 5A","Model 5B","Model 5C","Bargaining & optimization","Limitations","Model 6 — hWAR","Model 7 — Matchups","Model 8 — Front office")),function(i)
            tags$a(c("Question & data","Model 1","Model 2","Model 3","Model 4","Model 5A","Model 5B","Model 5C","Bargaining & optimization","Limitations","Model 6 — hWAR","Model 7 — Matchups","Model 8 — Front office")[i],href=paste0("#research-",i)))),
          research_section(1,"Research Question & Data","How can MLB improve competitive balance while protecting player compensation and maintaining an efficient market for player labor?",
            div(class="model-flow",span(class="flow-box","Payroll"),span(class="flow-arrow","→"),span(class="flow-box","Winning"),span(class="flow-arrow","→"),span(class="flow-box","Revenue"),span(class="flow-arrow","→"),span(class="flow-box","CBA policy")),
            h4("Data collection"),p("Models 1 and 3 use 2011–2025 team payroll and winning (450 team-seasons before exclusions). Model 2 uses 2017–2025 team revenue and winning. Model 5 adds 2026 payroll, wins and attendance and saved projected 2027 payroll. Team identifiers connect the datasets, and monetary amounts are expressed in millions of dollars."),
            p("Relative payroll adjusts for leaguewide growth. The shortened 2020 season is excluded from the Model 1 regression; Model 2 excludes 2020–2021. Missing lagged observations are omitted. See Sources for dataset definitions and snapshot dates.")),
          research_section(2,"Model 1 — Payroll and Winning","Estimate the historical relationship between relative payroll and winning percentage.",
            formula_box("RelativePayroll_{it}=\\frac{Payroll_{it}}{\\overline{Payroll}_t},\\quad AdjustedPayroll_{it}=\\ln(RelativePayroll_{it})"),
            p("The logarithm captures diminishing marginal returns: the same payroll increase matters more for a low-payroll club."),
            formula_box("WinPct_{it}=\\beta_0+\\beta_1\\ln(RelativePayroll_{it})+\\beta_2WinPct_{i,t-1}+\\alpha_i+\\gamma_t+\\epsilon_{it}"),
            p("Team fixed effects control for persistent club differences; year fixed effects account for common season conditions. Prior-year winning captures persistence."),
            div(class="research-finding",strong("Estimated payroll coefficient: 0.0273665.")," The estimated prior-win coefficient is approximately 0.3654. The payroll relationship is suggestive rather than definitive causal evidence."),
            formula_box("\\Delta Wins_i=162\\times0.0273665\\ln\\left(\\frac{RelativePayroll_{i,new}}{RelativePayroll_{i,base}}\\right)"),
            p("Holding average league payroll fixed, doubling a club's payroll predicts approximately 3.07 additional wins before adjusting for the fact that every win is another team's loss."),
            plotOutput("model1_curve",height="220px")),
          research_section(3,"Model 2 — Winning and Revenue","Estimate how winning affects team revenue.",
            formula_box("AdjustedRevenue_{it}=\\beta_0+\\beta_1WinPct_{it}+\\beta_2PriorRevenue_{it}+\\alpha_i+\\gamma_t+\\epsilon_{it}"),
            p("Adjusted revenue is log-relative revenue. Prior revenue and fixed effects control for persistent financial differences and common trends."),
            div(class="research-finding",strong("Estimated winning coefficient: 0.4535044.")," The historical simulator uses this coefficient to estimate incremental revenue."),
            formula_box("\\Delta Revenue_i=Revenue_{i,base}\\left[e^{0.4535044\\Delta WinPct_i}-1\\right]"),
            p("Ten additional wins imply approximately 2.84% more revenue under Model 2. This conditional estimate does not fully capture postseason revenue or long-term franchise value. Model 5 separately estimates attendance-linked revenue."),
            plotOutput("model2_curve",height="220px")),
          research_section(4,"Model 3 — Payroll Efficiency","Evaluate relative production and diminishing marginal returns to player spending.",
            formula_box("RelativeWinning_i=\\frac{WinPct_i}{0.500},\\quad Efficiency_i=\\frac{RelativeWinning_i}{RelativePayroll_i}"),
            p("A ratio above one indicates more relative winning than relative payroll; it is not a direct measure of overall team quality."),
            formula_box("MarginalWins_i(\\Delta P)=162\\times0.0273665\\ln\\left(\\frac{P_i+\\Delta P}{P_i}\\right)"),
            p("A $25M increase predicts roughly 1.28 gross wins at $75M payroll versus 0.42 at $250M. The simulator calculates payroll-weighted historical efficiency and marginal returns for clubs gaining payroll."),
            plotOutput("marginal_returns_plot",height="240px")),
          research_section(5,"Model 4 — CBA Policy Simulation","Combine financial and labor rules with Models 1–3 to evaluate each club.",
            h4("Step 1 — Revenue sharing and compensation"),
            formula_box("MediaPool_i=Revenue_i\\times MediaShare\\times PoolRate"),
            formula_box("OrdinaryPool_i=(Revenue_i-MediaPool_i)\\times SharingRate"),
            formula_box("NetTransfer_i=\\overline{MediaPool}-MediaPool_i+\\overline{OrdinaryPool}-OrdinaryPool_i"),
            p("Revenue transfers are zero-sum. Pooled media is excluded from ordinary sharing; team-specific media revenue is unavailable, so its share is adjustable."),
            formula_box("P_i^*=P_{i,base}+MinSalaryCost_i+ArbitrationCost_i+\\max(NetTransfer_i,0)\\times RequiredSpend"),
            p("Minimum salary increases use estimated player counts by service tier. Earlier arbitration uses the assumed eligible cohort and incremental pay; a separate cohort receives arbitration salary-floor increases. These are gross costs before payroll-rule offsets."),
            h4("Step 2 — Payroll rules"),
            formula_box("Floor:\\ P_i'=\\max(P_i^*,Floor-Offset_i)"),
            formula_box("LowTax:\\ P_i'=P_i^*+\\max(Benchmark-P_i^*-Offset_i,0)\\times Response"),
            formula_box("Cap:\\ P_i'=\\min(P_i^*,Cap-Offset_i)"),
            formula_box("CBT:\\ P_i'=P_i^*-\\max(P_i^*+Offset_i-CBT,0)\\times Response"),
            p("Apply lower restrictions before upper restrictions. Tax response is tax rate times the behavioral multiplier, clipped to 0–1. The cash-to-CBT offset remains fixed; deferred 2028 cap/floor terms are inactive during 2027."),
            h4("Step 3 — Winning and competitive balance"),
            formula_box("Adjusted\\Delta WinPct_i=Raw\\Delta WinPct_i-\\overline{Raw\\Delta WinPct}"),
            formula_box("WinSpread=SD(162\\times PredictedWinPct_i)"),
            formula_box("BalanceChange=100\\times\\frac{Spread_{base}-Spread_{new}}{Spread_{base}}"),
            p("Every win corresponds to a loss. Centering changes preserves total expected wins. Positive competitive balance change means closer team records; payroll Gini separately measures spending inequality."),
            h4("Step 4 — Player and club financial outcomes"),
            formula_box("PlayerComp=\\sum_i P_i'+PrearbPool+SettlementBonuses+VeteranBonuses"),
            formula_box("DirectClubEffect_i=\\Delta Revenue_i+NetTransfer_i-\\Delta Payroll_i-\\Delta CentralBonuses_i"),
            p("Minimum qualifying bonuses are allocated inside the pre-arbitration pool. The direct club effect excludes other operating expenses and tax settlement flows, so it is not profit. CBT and low-spending penalties are reported separately. Free agency and qualifying offers affect bargaining preferences, not individual roster decisions.")),
          research_section(6,"Model 5A — Estimate 2026 Revenue","Build a more recent financial baseline before projecting 2027.",
            formula_box("\\widehat R_{2026}=R_{2025}\\exp[0.047417+0.156374\\ln(A_{2026}/A_{2025})+0.141378\\Delta WinPct]"),
            p("Forbes revenue estimates are released with a lag. Prior revenue, attendance growth and win changes estimate the missing season. The growth intercept is a model estimate, not guaranteed revenue growth."),
            div(class="research-finding",strong("Estimated, not observed.")," The 2026 values remain labeled estimates until the corresponding financial figures are available.")),
          research_section(7,"Model 5B — Winning and Attendance","Translate predicted winning changes into attendance changes.",
            formula_box("\\widehat A_{2027}=A_{2026}\\exp(0.7638659\\Delta WinPct)"),
            p("Ten additional wins imply approximately 4.83% higher attendance under the model. Capacity, ticket prices and local demand may constrain actual attendance."),
            plotOutput("model5_curve",height="220px")),
          research_section(8,"Model 5C — Forecast 2027","Connect the saved payroll snapshot to expected wins, attendance and revenue.",
            formula_box("\\widehat R_{2027}=\\widehat R_{2026}\\exp[0.047417+0.156374\\ln(\\widehat A_{2027}/A_{2026})+0.141378\\Delta WinPct]"),
            p("In the private preview, compare 2027 payroll with 2026 payroll and remove the median log payroll change as a common spending trend. Use the Model 7 schedule engine to translate relative spending into matchup-specific wins rather than subtracting an identical number of wins from each club. Every game contributes one expected winner, preserving 2,430 wins. Then project attendance and revenue; subsequent transaction scenarios use incremental effects only."),
            p("The transaction ledger keeps salary, cash, CBT and deferred obligations separate. Unknown or unreconciled transactions are not added to saved payrolls.")),
          research_section(9,"Bargaining & Optimization","Maximize the weaker side's modeled support among economically eligible realistic packages.",
            h4("Support scores based on economic comparisons"),
            formula_box("Score_j=100\\times\\operatorname{clip}\\left(\\frac{x_j-\\min(x_{j,ref})}{\\max(x_{j,ref})-\\min(x_{j,ref})},0,1\\right)"),
            p("Each reference scenario (Current CBA, MLB proposal, MLBPA proposal) is evaluated with identical empirical assumptions. Identical reference values receive neutral 50. Indicators are oriented so higher is favorable. The weights reflect assumptions about how much each side values different economic outcomes. The scores are not voting probabilities."),
            div(class="research-weights",div(h4("MLB support"),tags$ul(tags$li("30% — Competitive balance and payroll equality"),tags$li("25% — Aggregate and worst-club financial effects"),tags$li("20% — Revenue equality and media pooling"),tags$li("25% — Cap and floor alignment"))),
              div(h4("MLBPA support"),tags$ul(tags$li("30% — Total compensation"),tags$li("25% — Labor-market freedom and CBT exposure"),tags$li("15% — Early-career pay"),tags$li("20% — Arbitration, free agency and qualifying-offer rights"),tags$li("10% — Lower-revenue club spending and media pooling")))),
            formula_box("Compromise=\\min(S_{MLB},S_{MLBPA})"),
            formula_box("Viability=\\operatorname{clip}[0.70\\min(S_M,S_P)+0.30(S_M+S_P)/2-0.40|S_M-S_P|,0,100]"),
            h4("Finding the best agreement"),
            p("Compare realistic agreements, refine promising combinations and test three estimates of payroll effects and three estimates of club responses. Choose the highest support score among agreements meeting the economic requirements. No minimum score is imposed."),
            p("Eligibility requires nonnegative central and median competitive balance and compensation changes, a funded bonus pool, and illustrative club-cost limits. Model 3 compares marginal returns to payroll when agreements have similar support. The highest eligible score is verified within the searched combinations, not every conceivable slider value.")),
          research_section(10,"Validation & Limitations","Explain what the model can and cannot establish.",
            p("Historical regression and chronological holdout tests remain in the research code. Model 1 compares payroll-based predictions with .500 and prior-year performance; Model 2 uses revenue benchmarks. The app checks 30-club coverage, zero-sum wins and transfers, contract accounting and the eligible optimizer maximum."),
            p("The models do not establish causal certainty or estimate ratification probabilities. Individual performance, injuries, roster decisions, capacity constraints, complete operating costs and private financial records are not modeled. Historical and projected payroll definitions differ. The tool supports economic comparisons rather than predicting actual agreement outcomes.")),
          fo_methods_ui()
        ),

        tabPanel("Model Assumptions",
          div(class="page-head",div(class="eyebrow","RESEARCH & BACKGROUND"),h2("Model Assumptions"),p("Optional sensitivity analysis. These inputs are not CBA provisions. Player counts and media shares are illustrative estimates, not observed team-level records.")),
          div(class="assumption-grid",
            div(class="assumption-card",h3("Minimum pay and pre-arbitration"),p("Estimated full-season player counts determine gross costs."),sliderInput("prearb_eligible","Estimated number of players eligible for the bonus pool per team",0,25,12,step=1),
sliderInput("min_players_under2","Estimated minimum-salary players with under 2 years of service",0,26,12,step=1),
sliderInput("min_players_2plus","Estimated minimum-salary players with 2+ years of service",0,14,5,step=1)),
            div(class="assumption-card",h3("Arbitration costs"),p("A $3M arbitration floor and an estimated $2M existing salary imply a $1M increase per affected player, before payroll offsets."),sliderInput("arbitration_payroll","Estimated salary of arbitration-eligible players per team ($M)",0,80,22,step=1),
sliderInput("arb_candidates","Estimated players per team with 2 years of service",0,12,4,step=1),
sliderInput("arb_raise","Estimated salary increase from earlier arbitration ($M/player)",0,8,2,step=0.25),
sliderInput("arb_floor_players","Estimated players affected by an arbitration salary minimum",0,12,3,step=1),
sliderInput("arb_floor_baseline","Estimated current salary of those players ($M)",0.78,5,2,step=0.01)),
            div(class="assumption-card",h3("Media and economic coefficients"),p("Media revenue is an assumption; regression coefficients are research estimates."),sliderInput("media_revenue_pct","Assumed media share of club revenue (%)",0,50,20,step=5),
sliderInput("payroll_coef","Estimated effect of payroll on winning",0.005,0.06,0.0273665,step=0.0001),
sliderInput("attendance_coef","Effect of winning on attendance (Model 5)",0,1.5,0.7638659,step=0.01),
sliderInput("revenue_growth","Estimated underlying revenue growth (Model 5)",0,0.1,0.047417,step=0.001))
          ),actionButton("reset_assumptions","Restore research defaults",class="btn-primary")
        ),

        tabPanel(
          "Learn",
          
          br(),
          
          h2("MLB CBA Terms"),
          tags$details(class="model-details",tags$summary("Current CBA and bargaining proposals"),
          tags$details(
            class = "model-details",
            tags$summary("Current CBA — terms and assumptions"),
            p("This tab shows the existing Current CBA preset used in the research simulator. The economic results use the selected baseline and simplified rules, not the actual financial results of the agreement."),
            p(class = "small-note", "The CBT rate is simplified and revenue-sharing and player revenue-share inputs are modeling assumptions. The preset is not a complete legal summary of the CBA."),
            h3("Policy Settings"),
            DTOutput("current_policy_settings"),
            h3("Modeled Economic Results"),
            DTOutput("current_policy_results"),
            p(tags$a("MLB Collective Bargaining Agreement information", href = "https://www.mlb.com/glossary/transactions/collective-bargaining-agreement", target = "_blank", rel = "noopener noreferrer"))
          ),

          tags$details(
            class = "model-details",
            tags$summary("MLB Proposal — terms and assumptions"),
            p("Last reviewed: November 23, 2026."),
            h3("Reported proposal"),
            p("2028: $258.2M cap and $180.2M floor, using CBT payroll. The current CBT and revenue-sharing system would continue in 2027."),
            p("2027: $1M minimum salary, $65M pre-arbitration pool, changes to free agency and draft compensation. MLB also proposed arbitration settlement and veteran bonuses."),
            p(tags$a("MLB — November 4", href = "https://www.mlb.com/news/mlb-increases-cap-and-floor-in-latest-proposal-sets-2028-target", target = "_blank", rel = "noopener noreferrer"), " | ", tags$a("MLB — November 23", href = "https://www.mlb.com/news/mlb-cba-proposal-minimum-salary-increase", target = "_blank", rel = "noopener noreferrer")),
            p(class = "small-note", "The cap and floor are inactive in the 2027 scenario and can be tested in 2028 against the selected baseline, not a 2028 forecast. The CBT offset, player-count assumptions and spending response are simplified estimates."),
            h3("Policy Settings"),
            DTOutput("mlb_policy_settings"),
            h3("Modeled Economic Results"),
            DTOutput("mlb_policy_results")
          ),

          tags$details(
            class = "model-details",
            tags$summary("MLBPA Proposal — terms and assumptions"),
            p("Last reviewed: November 23, 2026."),
            h3("Reported proposal"),
            p("The union opposes a hard cap. Its opening proposal includes a $300M CBT threshold, a tax on spending below $150M, a $1.5M minimum salary and a $180M pre-arbitration pool."),
            p("The union proposed doubling Super Two eligibility from 22% to 44%, a $3M arbitration salary minimum, earlier free agency for certain players age 30 or older, removing qualifying-offer draft penalties and increasing support for lower-revenue clubs."),
            p(tags$a("MLB Trade Rumors — Super Two details",href="https://www.mlbtraderumors.com/2026/05/mlbpa-releases-details-of-collective-bargaining-proposal.html",target="_blank",rel="noopener noreferrer")),
            p("On November 23, the union accepted equal media-revenue sharing across clubs, with increased support for lower-revenue clubs starting in 2027 under its proposal."),
            p(tags$a("MLBPA — May 27", href = "https://www.mlbplayers.com/press-releases/mlbpa-makes-opening-proposals-to-benefit-all-players-and-build-upon-industry-momentum", target = "_blank", rel = "noopener noreferrer"), " | ", tags$a("AP — reported numeric details", href = "https://apnews.com/article/mlb-labor-negotiations-f2892f59d219d68249c2133afb86291e", target = "_blank", rel = "noopener noreferrer"), " | ", tags$a("MLBPA — November 23", href = "https://www.mlbplayers.com/press-releases/players-agree-to-mlb-proposal-to-share-all-media-revenue-equally-across-clubs-addressing-concerns-about-revenue-disparity-and-blackouts", target = "_blank", rel = "noopener noreferrer")),
            p(class = "small-note", "The scenario uses the published thresholds and compensation figures. The 20% upper tax, 25% lower tax, 20% revenue-sharing rate and 50% required payroll spending are model assumptions. The Super Two expansion and arbitration floor use assumed affected player counts; detailed revenue-sharing incentives are not yet modeled."),
            h3("Illustrative Policy Settings"),
            DTOutput("mlbpa_policy_settings"),
            h3("Modeled Economic Results"),
            DTOutput("mlbpa_policy_results")
          )
          ),
          
          h3("Competitive Balance Tax (CBT)"),
          
          p(
            "A tax system applied to clubs with CBT payroll above a specified threshold. It is often called the luxury tax."
          ),
          
          h3("Salary Cap"),
          
          p(
            "A hard maximum payroll under a cap system."
          ),
          
          h3("Payroll Floor"),
          
          p(
            "A minimum payroll that clubs must reach."
          ),
          
          h3("Pre-Arbitration Player"),
          
          p(
            "A player who has not yet reached salary-arbitration eligibility."
          ),
          
          h3("Pre-Arbitration Bonus Program"),
          
          p(
            "A system that provides additional compensation to eligible pre-arbitration players."
          ),
          
          h3("Major League Service"),
          
          p(
            "The service-time system used to determine eligibility for several player rights, including salary arbitration and free agency."
          ),
          
          h3("Salary Arbitration"),
          
          p(
            "A salary-setting process that gives eligible players more leverage before free agency."
          ),
          
          h3("Free Agency"),
          
          p(
            "The point at which an eligible player can negotiate with clubs throughout MLB. Clubs may negotiate extensions with their own players before free agency."
          ),
          
          h3("Qualifying Offer / Draft Pick Compensation"),
          
          p(
            "A free-agent system that can create Draft pick consequences when a player rejects a qualifying offer and signs elsewhere."
          ),
          
          h3("Revenue Sharing"),
          
          p(
            "A system that redistributes some club revenue across MLB."
          ),
          
          h3("PPI"),
          
          p(
            "Prospect Promotion Incentive. A system designed to reward clubs for promoting certain top prospects without delaying their Major League service."
          ),
          
          h3("40-Man Roster"),
          
          p(
            "The larger roster that protects players from certain transactions and establishes eligibility for the active Major League roster."
          ),
          
          h3("Option"),
          
          p(
            "A transaction that allows a club to send an eligible player to the Minor Leagues while keeping that player on the 40-man roster."
          ),
          
          h3("Rule 5 Draft"),
          
          p(
            "A Draft that allows clubs to select certain eligible players who were not protected on another club's 40-man roster."
          )
        ),

        tabPanel(
          "Live Payroll",
          br(),
          h2("Live Payroll"),
          div(class="plain-box",strong("Independent partial accounting scenario"),p("Start with our 2026 research payrolls carried forward, then apply reviewed annual salary differences. This is not a complete 2027 roster or verified payroll total. CBT starts as the historical salary proxy; only documented CBT changes are applied. No future signings are predicted.")),
          textOutput("payroll_snapshot_status"),
          p(class = "small-note", "Transaction-driven changes are relative to the 2026 payroll anchor. Reported annual salaries and explicitly reviewed nominal AAV spending proxies are applied as provisional model inputs; exact cash salary and unknown departures remain pending. CBT is calculated separately. Automated discovery collects leads; only reviewed reports affect the model."),
          p(class="small-note",strong("Sources: "),"The historical baseline link is not the source of a live change. Click a club row to see its individual transaction reports and applied payroll changes."),
          DTOutput("live_payroll_table"),
          h3("Reviewed payroll adjustments"),textOutput("live_adjustment_status"),DTOutput("live_adjustments_table")
        ),

        tabPanel(
          "2027 Economic Forecasts",
          br(),
          h2("2027 Economic Forecasts"),
          p("Model 5 connects projected payroll, winning, attendance and team revenue for all 30 clubs."),
          div(class = "plain-box",
            strong("Why estimate 2026 revenue?"),
            p("Forbes typically releases its MLB team revenue estimates several months after the season ends. Since 2026 revenue estimates are not available in our dataset yet, I use historical revenue, attendance and winning percentage to estimate 2026 revenue. This gives the model a more recent starting point for 2027 than using 2025 revenue.")
          ),
          textOutput("forecast_snapshot_status"),
          p("These are payroll-based economic scenarios, not full predictions of the standings. Projected payroll may change throughout the offseason and may use a different definition from historical Opening Day payroll. The model also assumes a 162-game season."),
          DTOutput("economic_forecast_table"),
          fo_roster_coverage_ui(),
          p(class = "small-note", "Revenue is shown in millions of dollars. 2026 revenue is estimated by Model 5; 2027 revenue and attendance are projected. Once Forbes releases its 2026 figures, the estimates can be compared with them.")
        ),

        tabPanel(
          "Signings & Trades",
          div(class="page-head",div(class="eyebrow","MLB FINANCIAL DATA"),h2("Transactions"),
            p("Track offseason signings, trades, arbitration settlements, extensions and options.")),
          div(class="plain-box",strong("Click a transaction to open its details. "),"A dash means the amount is unknown or does not apply. We only count a contract once in team payroll."),
          tabsetPanel(id="transaction_view",type="pills",
            tabPanel("Free Agency",div(class="dashboard-panel",h3("Free Agency Tracker"),p("Contract value and AAV differ from salary and CBT charge."),DTOutput("free_agency_table"))),
            tabPanel("Trades",div(class="dashboard-panel",h3("Trade History"),p("Each row shows a complete trade, including the players each team receives. Click to see reported salary and payroll changes."),
              selectInput("trade_season","Payroll season",choices=c("All seasons"="all","2027"="2027"),selected="2027"),DTOutput("trade_history_table"))),
            tabPanel("Player Impact",fo_report_ui()),
            tabPanel("Annual Contracts",div(class="dashboard-panel",h3("Annual Contract Accounting"),
              p("Annual salary and applied Live Payroll scenario changes. Provisional amounts are estimates against the 2026 carry-forward, not reconciled 2027 obligations. Pending amounts remain unknown."),
              fluidRow(column(4,selectInput("contract_season","Obligation season",c("All",as.character(2026:2035)),selected="2027")),
                column(4,selectInput("contract_type","Transaction type",c("All","Free agency","Qualifying offer","Arbitration settlement","Extension","Club option","Player option","Mutual option","Mutual option declined","Non-tender","Trade"))),
                column(4,selectInput("contract_club","Club",c("All",sort(base$Team_ID))))),
              DTOutput("contract_ledger_table")))),
          p(class="small-note","Selected reports since October 29, 2026; not a complete or automatically refreshed transaction log."),
          p(tags$a("MLB transaction history",href="https://www.mlb.com/transactions",target="_blank",rel="noopener noreferrer"))
        ),

        tabPanel("hWAR Leaders",fo_leaders_ui(setNames(base$Team_ID,base$Team_Name))),
        tabPanel("Player Profiles",fo_profile_ui()),
        tabPanel("Trade Simulator",fo_move_ui(setNames(base$Team_ID,base$Team_Name))),
        tabPanel("Signing Simulator",fo_move_ui(setNames(base$Team_ID,base$Team_Name),signing=TRUE)),
        tabPanel("Projected Standings",fo_schedule_ui()),
        tabPanel("Contract Valuation Lab",fo_contract_ui()),
        tabPanel("Free-Agent Lab",fo_fa_ui()),
        tabPanel("Transaction Economics",fo_note("Compare a player-based estimate with the existing payroll-based transaction analysis below. They are alternative win estimates, not additive.",actionButton("fo_go_report","View player-based signing impact")),transaction_impact_ui(setNames(base$Team_ID,base$Team_Name))),
        tabPanel("Offseason Economics",fo_note("Payroll history and player movement measure different things. Explore the player-based impact of individual reported signings without adding the same change twice.",actionButton("fo_go_offseason_report","Explore signing impact")),transaction_offseason_ui()),
        tabPanel(
          "CBA News",
          h2("CBA Negotiations"),
          p("Last reviewed: November 23, 2026. These are manually reviewed updates, not a live news feed."),
          h3("November 23"),
          p("The union agreed to equal media-revenue sharing. MLB updated its player compensation offer."),
          p(tags$a("MLBPA media proposal", href = "https://www.mlbplayers.com/press-releases/players-agree-to-mlb-proposal-to-share-all-media-revenue-equally-across-clubs-addressing-concerns-about-revenue-disparity-and-blackouts", target = "_blank", rel = "noopener noreferrer"), " | ", tags$a("MLB salary proposal", href = "https://www.mlb.com/news/mlb-cba-proposal-minimum-salary-increase", target = "_blank", rel = "noopener noreferrer")),
          h3("November 20"),
          p("The union proposed changes to international amateur signings without an international draft."),
          p(tags$a("MLBPA statement", href = "https://www.mlbplayers.com/press-releases/mlbpa-makes-international-amateur-signing-proposal-to-protect-support-the-rights-of-young-players", target = "_blank", rel = "noopener noreferrer")),
          h3("November 19"),
          p("MLB reaffirmed its position on a salary cap and floor."),
          p(tags$a("MLB bargaining update", href = "https://www.mlb.com/news/commissioner-rob-manfred-discusses-salary-cap-and-floor-proposals", target = "_blank", rel = "noopener noreferrer")),
          h3("November 4"),
          p("MLB proposed delaying the cap and floor until 2028."),
          p(tags$a("MLB transition proposal", href = "https://www.mlb.com/news/mlb-increases-cap-and-floor-in-latest-proposal-sets-2028-target", target = "_blank", rel = "noopener noreferrer")),
          p(class = "small-note", "News discovery is paused after provider rate limits. New reports do not automatically change the economic model.")
        ),

        tabPanel(
          "Sources",
          br(),
          h2("Data Sources"),
          h3("Historical data"),
          p("The original research datasets cover payroll and winning from 2011–2025, and team revenue from 2017–2025. Model 5 also uses 2026 payroll, winning and attendance from the research dataset."),
          tags$ul(
            tags$li(tags$a("Forbes MLB team valuations and financial estimates", href = "https://www.forbes.com/mlb-valuations/", target = "_blank", rel = "noopener noreferrer")),
            tags$li(tags$a("The Baseball Cube: 2026 MLB season summary", href = "https://www.thebaseballcube.com/content/mlb_summary/2026/", target = "_blank", rel = "noopener noreferrer"))
          ),
          h3("Projected payroll"),
          p("Independent Live Payroll scenario: 2026 opening-day team payroll from the original research dataset is carried forward. Reviewed transaction deltas are sourced individually; provisional assumptions are marked. Unresolved departures, salary schedules and CBT terms are excluded from adjustments, not treated as verified zero. Historical cash payroll is a CBT proxy until a separate opening CBT ledger is established."),
          h3("Free agency"),
          p("Sources are shown with individual reports. Contract value, cash payroll and CBT payroll are kept separate to avoid counting contracts twice."),
          h3("Update schedule"),
          p("The data pipeline checks hourly from 7 AM to 9 PM Pacific. Model results are recalculated when relevant inputs change. Reviewed transaction adjustments must be entered before the pipeline recalculates; no authorized external transaction feed is active."),
          p(class = "small-note", "Choose the 2025 historical baseline or the 2027 projected baseline. The latter uses the partial Live Payroll scenario and CBT proxy. It measures incremental reported transaction effects, not an actual 2027 payroll census.")
        )
      )
    )
  )
)

server <- function(
    input,
    output,
    session
) {
  
  observe({
    group <- input$nav_group
    req(group)
    tab <- switch(group,"CBA Simulator"=input$sim_tab,"MLB Financial Data"=input$finance_tab,"Player Analytics"=input$player_tab,"Front Office"=input$fo_tab,"Research & Background"=input$research_tab)
    req(tab)
    updateTabsetPanel(session,"dashboard_tab",selected=tab)
  })
  observeEvent(input$reset_assumptions,{
    p <- policy_assumptions()
    for(id in c("prearb_eligible","min_players_under2","min_players_2plus","arbitration_payroll","arb_candidates","arb_raise","arb_floor_players","arb_floor_baseline","media_revenue_pct","payroll_coef","attendance_coef","revenue_growth")) updateSliderInput(session,id,value=p[[id]])
    updateSliderInput(session,"behavior_response",value=1)
  })
  output$club_detail_table <- renderDT({
    req(input$club_detail)
    d <- simulation()$team
    row <- d[d$Team_ID == input$club_detail,,drop=FALSE]
    req(nrow(row)==1)
    fields <- c("Original_Payroll_M","New_Payroll_M","Payroll_Change_M","Change_Wins","Direct_Club_Effect_M")
    app_table(data.frame(Measure=c("Baseline payroll ($M)","Scenario payroll ($M)","Payroll change ($M)","Predicted win change","Direct club financial effect ($M)"),Result=round(as.numeric(unlist(row[1,fields])),2)),rownames=FALSE,options=list(dom="t",ordering=FALSE))
  })
  scenario_baseline <- reactive({
    if(is.null(input$baseline_mode)||input$baseline_mode=="2025") return(base)
    projection_baseline(base,economic_forecast(),payroll_snapshot())
  })
  comparison_context <- reactive({
    p <- current_policy()
    p[c("scenario_year","media_revenue_pct","media_share_pct","min_players_under2","min_players_2plus","arbitration_payroll","arb_candidates","arb_raise","arb_floor_players","arb_floor_baseline","prearb_eligible","attendance_coef","revenue_growth","payroll_coef")]
  })
  preset_with_context <- function(p) modifyList(complete_policy(p),comparison_context())
  output$baseline_description <- renderText({
    if(identical(input$baseline_mode,"2027")) paste("2027 projection baseline | snapshot",unique(payroll_snapshot()$Snapshot_Date),"| incomplete carry-forward; CBT proxy")
    else "2025 historical baseline | Opening Day payroll as CBT proxy"
  })
  output$policy_timing_note <- renderUI({
    m <- simulation()$metrics
    tagList(if(m$cap_deferred) div(class="plain-box","Cap and floor begin in 2028 under this proposal. The 2027 scenario uses a simplified CBT. Choose 2028 to test later terms against the same baseline."),
      if(m$prearb_remaining<0) div(class="important-box","The assumed minimum bonus allocation exceeds the selected pool. Increase the pool or reduce the qualifying-player count or minimum bonus. This package is not feasible."))
  })
  output$compensation_breakdown <- renderDT({
    m <- simulation()$metrics
    fixed_policy_table(data.frame(Component=c("Minimum salary cost before payroll-rule offsets","Earlier arbitration cost before payroll-rule offsets","Arbitration salary floor cost before payroll-rule offsets","Pre-arbitration pool (total)","Pre-arbitration pool increase","Minimum bonus allocation within pool","Remaining bonus pool after minimum allocation","Arbitration settlement bonus","Veteran bonuses","Media redistribution (zero-sum)","Attendance change (2027 baseline only)"),
      Estimate=c(fmt1(m$minimum_salary_cost),fmt1(m$earlier_arb_cost),fmt1(m$arbitration_floor_cost),fmt1(m$prearb_pool),fmt1(m$prearb_delta),fmt1(m$prearb_min_allocation),fmt1(m$prearb_remaining),fmt1(m$arbitration_bonus),fmt1(m$veteran_bonus),fmt1(m$media_redistributed),ifelse(is.na(m$attendance_change),"Not modeled",format(round(m$attendance_change),big.mark=","))),Unit=c(rep("$M",10),"Visits")))
  })
  selected_tx_type <- reactiveVal(NULL)
  selected_contract_detail <- reactiveVal(NULL)
  selected_free_agency_detail <- reactiveVal(NULL)
  selected_trade_detail <- reactiveVal(NULL)
  open_transaction <- function(table,detail,selected) {
    observeEvent(input[[paste0(table,"_rows_selected")]],{
      i <- input[[paste0(table,"_rows_selected")]]
      if(length(i)!=1) return()
      selected(i)
      selected_tx_type(detail)
      showModal(modalDialog(uiOutput(detail),size="l",easyClose=TRUE,footer=tagList(actionButton("analyze_selected_tx","Analyze economic impact",class="btn-primary"),modalButton("Close details"))))
      DT::selectRows(DT::dataTableProxy(table,session=session),NULL)
    },ignoreInit=TRUE)
  }
  open_transaction("contract_ledger_table","contract_detail",selected_contract_detail)
  open_transaction("free_agency_table","free_agency_detail",selected_free_agency_detail)
  open_transaction("trade_history_table","trade_detail",selected_trade_detail)
  tx_table <- function(tab,money_cols,order_col) {
    app_table(tab,rownames=FALSE,escape=-c(which(names(tab)=="Player"),ncol(tab)),class="compact stripe hover",selection="single",
      options=list(pageLength=10,lengthMenu=c(10,25,50),scrollX=TRUE,autoWidth=FALSE,order=list(list(order_col,"desc")),
      columnDefs=list(list(targets=which(names(tab)%in%money_cols)-1,
        render=JS("function(data,type){if(data===null||data===''){return type==='display'?'—':data;} if(type==='display')return '$'+Number(data).toLocaleString('en-US',{minimumFractionDigits:2,maximumFractionDigits:3})+'M';return data;}")))))
  }
  detail_money <- function(x) if(length(x)==0||is.na(x)) "—" else paste0("$",format(round(x,3),trim=TRUE,nsmall=2,big.mark=","),"M")
  detail_value <- function(x) if(length(x)==0||is.na(x)||identical(x,"")) "—" else as.character(x)
  detail_panel <- function(title,subtitle,labels,values,notes=NULL,url=NULL) {
    div(class="transaction-detail",h3(title),p(class="small-note",subtitle),
      div(class="detail-grid",lapply(seq_along(labels),function(i)div(span(class="detail-label",labels[i]),strong(values[i])))),
      if(length(notes)&&!is.na(notes)&&nzchar(notes)) p(class="detail-notes",notes),
      if(length(url)&&!is.na(url)&&grepl("^https://[^[:space:]]+$",url)) tags$a("Read source report",href=url,target="_blank",rel="noopener noreferrer"))
  }
  contract_rows <- reactive({
    d <- contract_applied_view(contract_ledger(),live_adjustments())
    if(!is.null(input$contract_season)&&input$contract_season!="All") d <- d[d$Obligation_Season==as.integer(input$contract_season),,drop=FALSE]
    if(!is.null(input$contract_type)&&input$contract_type!="All") d <- d[d$Transaction_Type==input$contract_type,,drop=FALSE]
    if(!is.null(input$contract_club)&&input$contract_club!="All") d <- d[d$Club==input$contract_club,,drop=FALSE]
    d
  })
  output$contract_ledger_table <- renderDT({
    d <- contract_rows()
    tab <- data.frame(Player=d$Player,Club=d$Club,Type=d$Transaction_Type,Date=d$Signed_Date,Season=d$Obligation_Season,
      Salary=d$Salary_M,"Payroll change"=d$Applied_Payroll_M,"CBT change"=d$Applied_CBT_M,
      `Payroll data status`=d$Applied_Status,`CBT status`=d$Applied_CBT_Status,Source=source_link(d$Source_URL),check.names=FALSE)
    tx_table(tab,c("Salary","Payroll change","CBT change"),3)
  })
  output$contract_detail <- renderUI({
    d <- contract_rows(); i <- selected_contract_detail()
    if(length(i)!=1||i>nrow(d)) return(p(class="small-note","Select a transaction to view all financial obligations."))
    x <- d[i,,drop=FALSE]
    fields <- c("Salary_M","Prior_Salary_M","Salary_Change_From_Prior_M","Published_Projection_M","Difference_From_Projection_M",
      "Cash_Payment_M","CBT_Charge_M","Signing_Bonus_M","Deferred_Earnings_M","Deferred_Payment_M","Option_M","Buyout_M","Retained_Salary_M",
      "Incremental_Payroll_M","Incremental_CBT_M","Applied_Payroll_M","Applied_CBT_M")
    labels <- c("Salary","Prior salary","Reported raise","Published projection","Difference vs projection","Cash payment","CBT charge",
      "Signing bonus","Deferred earnings","Deferred payment","Option","Buyout","Retained salary","Reconciled payroll","Reconciled CBT","Applied scenario payroll","Applied scenario CBT")
    detail_panel(paste(x$Player,"·",x$Club),paste(x$Transaction_Type,"·",x$Status,"· effective",x$Effective_Season,"· obligation",x$Obligation_Season,"·",x$Snapshot_Reconciliation),
      labels,vapply(fields,function(k)detail_money(x[[k]]),character(1)),paste(x$Notes,if(!is.na(x$Adjustment_Notes))x$Adjustment_Notes else "",x$Applied_Status,x$Applied_CBT_Status),x$Source_URL)
  })
  validation <- reactive({research_validation(payroll_data,revenue_data)})
  output$regression_table <- renderDT({
    app_table(validation()$coefficients,rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>% formatRound(c("Estimate","SE","T","R_squared","Adjusted_R_squared"),4) %>% formatSignif("P",4)
  })
  output$validation_table <- renderDT({
    app_table(validation()$validation,rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>% formatRound("RMSE",5)
  })
  output$model1_curve <- renderPlot({
    x <- seq(.5,2,length.out=100)
    plot(x,162*payroll_effect*log(x),type="l",lwd=3,col="#245d91",xlab="New payroll / baseline payroll",ylab="Predicted gross win change",bty="l"); abline(h=0,v=1,lty=2,col="gray65")
  })
  output$model2_curve <- renderPlot({
    x <- seq(-15,15,length.out=100)
    plot(x,100*expm1(revenue_effect*x/162),type="l",lwd=3,col="#245d91",xlab="Change in wins",ylab="Predicted revenue change (%)",bty="l"); abline(h=0,v=0,lty=2,col="gray65")
  })
  output$model5_curve <- renderPlot({
    x <- seq(-15,15,length.out=100)
    plot(x,100*expm1(.7638659*x/162),type="l",lwd=3,col="#245d91",xlab="Change in wins",ylab="Predicted attendance change (%)",bty="l"); abline(h=0,v=0,lty=2,col="gray65")
  })
  output$marginal_returns_plot <- renderPlot({
    x <- seq(50,350,by=5)
    plot(x,162*payroll_effect*log1p(25/x),type="l",lwd=3,col="#245d91",xlab="Initial payroll ($M)",ylab="Predicted marginal wins",main="Diminishing marginal returns to an additional $25M")
    grid()
  })
  policy_settings_table <- function(policy) {
    upper <- if (policy$upper_rule == "cap") "Salary cap" else if (policy$upper_rule == "cbt") "CBT" else "None"
    lower <- if (policy$lower_rule == "floor") "Payroll floor" else if (policy$lower_rule == "tax") "Low-spending tax" else "None"
    data.frame(
      Rule = c("High-payroll rule", "Upper payroll threshold ($M)", "Upper tax rate (%)",
        "Low-payroll rule", "Lower payroll threshold ($M)", "Lower tax rate (%)",
        "Revenue sharing assumption (%)", "Required payroll spending assumption (%)", "Equal media sharing",
        "Minimum salary: under 2 years ($M)", "Minimum salary: 2+ years ($M)",
        "Pre-arbitration bonus pool ($M)", "Free agency eligibility", "Qualifying offer"),
      Setting = c(upper, policy$upper_value, policy$upper_tax_rate, lower,
        ifelse(is.na(policy$lower_value), "Not applicable", policy$lower_value),
        policy$lower_tax_rate, policy$revenue_sharing, policy$required_spending,
        ifelse(policy$central_media, "Yes", "No"), policy$minimum_salary_under2, policy$minimum_salary_2plus,
        policy$prearb_pool, ifelse(policy$free_agency == "6", "Generally 6 years", "5 years for certain age-30+ players"),
        ifelse(policy$qualifying_offer, "Retained", "Removed"))
    )
  }
  policy_results_table <- function(policy) {
    m <- simulate_policy(preset_with_context(policy),baseline=scenario_baseline())$metrics
    data.frame(
      Metric = c("Payroll Gini coefficient", "Win Total Spread", "Competitive Balance Change",
        "Modeled Player Compensation ($M)", "Total Payroll Change ($M)",
        "Predicted Revenue Change ($M)", "Direct Club Financial Effect ($M)"),
      Result = c(fmt3(m$gini), fmt2(m$win_spread),
        paste0(fmt1(m$competitive_balance_change), "%"), fmt1(m$player_comp),
        fmt1(m$payroll_change), fmt1(m$revenue_change), fmt1(m$club_effect))
    )
  }
  fixed_policy_table <- function(d) {
    app_table(d, rownames = FALSE, options = list(dom = "t", ordering = FALSE, paging = FALSE))
  }
  output$current_policy_settings <- renderDT(fixed_policy_table(policy_settings_table(current_cba_policy())))
  output$current_policy_results <- renderDT(fixed_policy_table(policy_results_table(current_cba_policy())))
  output$mlb_policy_settings <- renderDT(fixed_policy_table(policy_settings_table(mlb_proposal_policy())))
  output$mlb_policy_results <- renderDT(fixed_policy_table(policy_results_table(mlb_proposal_policy())))
  output$mlbpa_policy_settings <- renderDT(fixed_policy_table(policy_settings_table(mlbpa_proposal_policy())))
  output$mlbpa_policy_results <- renderDT(fixed_policy_table(policy_results_table(mlbpa_proposal_policy())))

  # Read only the cached research datasets. No network requests from the app.
  cached_data <- function(path, required) {
    validate(need(file.exists(path), "Data is not available yet."))
    d <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    validate(need(!is.null(d) && all(required %in% names(d)), "The saved dataset could not be read."))
    d
  }
  # Keep direct HTTPS source URLs and escape all user-supplied link text.
  source_link <- function(url, label = "Source") {
    vapply(url, function(u) {
      if (is.na(u) || !grepl("^https://[^[:space:]]+$", u)) return("")
      as.character(tags$a(label, href = u, target = "_blank", rel = "noopener noreferrer"))
    }, character(1), USE.NAMES = FALSE)
  }
  payroll_snapshot <- reactive({
    cached_data("data/payroll_changes.csv",
      c("Team_ID", "Team_Name", "Baseline_Date", "Snapshot_Date", "Latest_Payroll_Millions",
        "Latest_CBT_Millions", "Payroll_Change_Millions", "CBT_Change_Millions", "Source_URL"))
  })
  economic_forecast <- reactive({
    f <- cached_data("data/mlb_2027_economic_forecasts.csv",
      c("Team_ID", "Team_Name", "Snapshot_Date", "Projected_2027_Payroll_Millions",
        "Payroll_2026_Millions", "Win_Pct_2026", "Expected_Wins_2027", "Attendance_2026", "Attendance_2027", "Revenue_2026", "Revenue_2027",
        "Revenue_Change_Pct", "Payroll_Source_URL"))
    fo_financial_forecast(f)
  })
  fo_server(input, output, session, economic_forecast)
  fo_move_server(input, output, session, economic_forecast)
  fo_move_server(input, output, session, economic_forecast, signing=TRUE)
  fo_profile_detail_server(input, output, session)
  fo_bridge_server(input, output, session, economic_forecast)
  fo_open_report <- function(){updateRadioButtons(session,"nav_group",selected="MLB Financial Data");updateRadioButtons(session,"finance_tab",selected="Signings & Trades");updateTabsetPanel(session,"transaction_view",selected="Player Impact")}
  observeEvent(input$fo_go_report,fo_open_report())
  observeEvent(input$fo_go_offseason_report,fo_open_report())
  output$payroll_snapshot_status <- renderText({
    d <- payroll_snapshot()
    paste("Latest saved snapshot:", paste(unique(d$Snapshot_Date), collapse = ", "),
          "| Baseline:", paste(unique(d$Baseline_Date), collapse = ", "))
  })
  output$forecast_snapshot_status <- renderText({
    paste("Payroll snapshot used:", paste(unique(economic_forecast()$Snapshot_Date), collapse = ", "))
  })
  live_adjustments <- reactive(cached_data("data/live_payroll_effective_adjustments.csv",c("Event_ID","Club","Player","Reported_Date","Prior_Payroll_M","New_Payroll_M","Prior_CBT_M","New_CBT_M","Payroll_Status","CBT_Status","Source_URL","Notes")))
  live_payroll_rows <- reactive({
    d <- payroll_snapshot()
    d[order(-d$Latest_Payroll_Millions), , drop=FALSE]
  })
  output$live_payroll_table <- renderDT({
    d <- live_payroll_rows()
    e <- live_adjustments()
    applied <- e$Payroll_Status!="Pending" | e$CBT_Status=="Confirmed"
    tab <- data.frame(
      Club = d$Team_Name,
      `Payroll ($M)` = round(d$Latest_Payroll_Millions, 1),
      `CBT Proxy ($M)` = round(d$Latest_CBT_Millions, 1),
      `Payroll Change ($M)` = round(d$Payroll_Change_Millions, 1),
      `CBT Change ($M)` = round(d$CBT_Change_Millions, 1),
      `Applied adjustments` = vapply(d$Team_ID,function(k)sum(e$Club==k & applied),integer(1)),
      `2026 baseline source` = source_link(d$Source_URL,"Historical baseline"), check.names = FALSE
    )
    app_table(tab, rownames = FALSE, escape = -ncol(tab), selection="single",
      options = list(pageLength = 30, scrollX = TRUE, order = list(list(1, "desc"))))
  })
  selected_payroll_club <- reactiveVal(NULL)
  observeEvent(input$live_payroll_table_rows_selected,{
    i <- input$live_payroll_table_rows_selected
    d <- live_payroll_rows()
    if(length(i)!=1L || i>nrow(d)) return()
    selected_payroll_club(d$Team_ID[i])
    showModal(modalDialog(uiOutput("live_payroll_source_detail"),size="l",easyClose=TRUE,footer=modalButton("Close sources")))
    DT::selectRows(DT::dataTableProxy("live_payroll_table",session=session),NULL)
  },ignoreInit=TRUE)
  output$live_payroll_source_detail <- renderUI({
    club <- selected_payroll_club(); if(is.null(club)) return(NULL)
    d <- live_payroll_rows()
    b <- d[d$Team_ID==club,,drop=FALSE]; req(nrow(b)==1L)
    e <- live_adjustments(); e <- e[e$Club==club,,drop=FALSE]
    e <- e[order(e$Reported_Date,e$Event_ID),,drop=FALSE]
    delta <- ifelse(e$Payroll_Status=="Pending",NA_real_,e$New_Payroll_M-e$Prior_Payroll_M)
    cbt <- ifelse(e$CBT_Status=="Confirmed",e$New_CBT_M-e$Prior_CBT_M,NA_real_)
    running <- b$Base_Payroll_Millions[1]+cumsum(ifelse(is.na(delta),0,delta))
    stopifnot(abs(b$Base_Payroll_Millions[1]+sum(delta,na.rm=TRUE)-b$Latest_Payroll_Millions[1])<1e-6)
    fmt <- function(x)if(is.na(x))"—"else sprintf("%+.3fM",x)
    amount <- function(x)sprintf("$%.3fM",x)
    report <- function(u)if(!is.na(u)&&grepl("^https://[^[:space:]]+$",u))tags$a("Original report",href=u,target="_blank",rel="noopener noreferrer")else "Unavailable"
    div(class="transaction-detail",h3(paste(b$Team_Name,"— Payroll sources")),
      p(class="small-note","Historical baseline (2026 Opening Day): ",report(b$Source_URL[1]),". The baseline is carried forward; the following reports support the incremental changes, not a full current payroll census."),
      div(class="plain-box",strong(paste("Starting payroll:",amount(b$Base_Payroll_Millions[1]))),
        p(paste("Net applied payroll change:",fmt(b$Payroll_Change_Millions[1]))),
        strong(paste("Current modeled payroll:",amount(b$Latest_Payroll_Millions[1]))),
        p(paste(sum(e$Payroll_Status!="Pending"),"applied salary adjustments;",sum(e$Payroll_Status=="Pending"),"pending. Provisional inputs include nominal AAV proxies when annual salary is unknown."))),
      if(!nrow(e))p("No reviewed transactions for this club yet.") else
        tags$table(class="table table-sm",tags$thead(tags$tr(tags$th("Player / date"),tags$th("Payroll change"),tags$th("Running payroll"),tags$th("CBT change"),tags$th("Status"),tags$th("Transaction source"))),
          tags$tbody(lapply(seq_len(nrow(e)),function(j)tags$tr(
            tags$td(e$Player[j],tags$div(class="small-note",e$Reported_Date[j])),
            tags$td(fmt(delta[j])),
            tags$td(amount(running[j]),if(is.na(delta[j]))tags$div(class="small-note","Not applied")),
            tags$td(fmt(cbt[j])),
            tags$td(e$Payroll_Status[j],if(grepl("nominal AAV spending proxy ONLY",e$Notes[j],fixed=TRUE))tags$div(class="small-note","Nominal AAV proxy")),
            tags$td(report(e$Source_URL[j])))))),
      p(class="small-note","Running totals order reviewed transactions by report date and ID; unknown changes are excluded, not verified as zero. Full source notes remain in Reviewed payroll adjustments."))
  })
  output$live_adjustment_status <- renderText({d <- live_adjustments();paste(nrow(d),"tracked club-event rows;",sum(d$Payroll_Status!="Pending"),"applied (",sum(d$Payroll_Status=="Provisional"),"provisional );",sum(d$Payroll_Status=="Pending"),"pending salary reconciliation;",sum(d$CBT_Status=="Pending"),"pending CBT. Coverage is partial.")})
  output$live_adjustments_table <- renderDT({d <- live_adjustments(); tab <- data.frame(Club=d$Club,Player=d$Player,Prior=d$Prior_Payroll_M,New=d$New_Payroll_M,Change=ifelse(d$Payroll_Status=="Pending",NA_real_,d$New_Payroll_M-d$Prior_Payroll_M),Salary=d$Payroll_Status,CBT=d$CBT_Status,Notes=d$Notes,Source=source_link(d$Source_URL));app_table(tab,rownames=FALSE,escape=-ncol(tab),options=list(scrollX=TRUE,pageLength=10))})
  output$economic_forecast_table <- renderDT({
    d <- economic_forecast()
    tab <- data.frame(
      Club = d$Team_Name,
      `Payroll ($M)` = round(d$Projected_2027_Payroll_Millions, 1),
      `Expected Wins` = round(d$Expected_Wins_2027, 1),
      `2026 Attendance` = round(d$Attendance_2026),
      `2027 Attendance` = round(d$Attendance_2027),
      `2026 Est. Revenue ($M)` = round(d$Revenue_2026, 1),
      `2027 Proj. Revenue ($M)` = round(d$Revenue_2027, 1),
      `Revenue Change (%)` = round(d$Revenue_Change_Pct, 2),
      `2026 baseline source` = source_link(d$Payroll_Source_URL,"Historical baseline"), check.names = FALSE
    )
    app_table(tab, rownames = FALSE, escape = -ncol(tab),
      options = list(pageLength = 30, scrollX = TRUE)) %>%
      formatRound(c("2026 Attendance", "2027 Attendance"), digits = 0)
  })
  free_agency_rows <- reactive({
    d <- cached_data("data/free_agency_tracker.csv",c("Player","Previous_Team_ID","New_Team_ID","Reported_Date","Status","Years","Total_Value_Millions",
      "Nominal_AAV_Millions","Deferred_Millions","Payroll_2027_Impact_Millions","CBT_2027_Impact_Millions","Source_URL"))
    contracts <- contract_ledger()
    annual <- contracts[contracts$Obligation_Season==2027 & contracts$Transaction_Type %in% c("Free agency","Qualifying offer"),,drop=FALSE]
    d$Reported_Salary_M <- annual$Salary_M[match(d$Signing_ID,annual$Contract_ID)]
    d
  })
  output$free_agency_table <- renderDT({
    d <- free_agency_rows()
    tab <- data.frame(Player=vapply(seq_len(nrow(d)),function(i)as.character(fo_name_link(d$Player[i],d$Previous_Team_ID[i])),character(1)),`Projected hWAR`=vapply(seq_len(nrow(d)),function(i){q<-fo_player_match(d$Player[i],d$Previous_Team_ID[i]);if(nrow(q)==1)round(q$hWAR,2)else NA_real_},numeric(1)),From=d$Previous_Team_ID,To=d$New_Team_ID,Date=d$Reported_Date,Years=d$Years,
      Value=d$Total_Value_Millions,AAV=d$Nominal_AAV_Millions,"2027 salary"=d$Reported_Salary_M,
      Status=d$Status,Source=source_link(d$Source_URL),check.names=FALSE)
    tx_table(tab,c("Value","AAV","2027 salary"),4)
  })
  output$free_agency_detail <- renderUI({
    d <- free_agency_rows(); i <- selected_free_agency_detail()
    if(length(i)!=1||i>nrow(d)) return(p(class="small-note","Select a signing to view the financial terms."))
    x <- d[i,,drop=FALSE]
    effect <- tx_recorded_signing_effect(projection_baseline(base,economic_forecast(),payroll_snapshot()),x,live_adjustments())
    ec <- effect$clubs
    tagList(detail_panel(paste(x$Player,"·",x$New_Team_ID),paste(x$Status,"·",x$Reported_Date,"· previous club",detail_value(x$Previous_Team_ID)),
      c("Years","Contract value","Nominal AAV","2027 reported salary","Deferred amount","2027 payroll impact","2027 CBT impact"),
      c(detail_value(x$Years),detail_money(x$Total_Value_Millions),detail_money(x$Nominal_AAV_Millions),detail_money(x$Reported_Salary_M),
        detail_money(x$Deferred_Millions),detail_money(x$Payroll_2027_Impact_Millions),detail_money(x$CBT_2027_Impact_Millions)),
      "Reported terms may differ from final accounting. Unknown impacts are not added to the snapshot.",x$Source_URL),
      div(class="dashboard-panel",h4("Player research"),fo_name_link(x$Player,x$Previous_Team_ID),p(class="small-note","Open the player profile for career statistics and projected hWAR. Select Analyze economic impact to view matchup-adjusted wins.")),
      div(class="dashboard-panel",h4("Applied payroll and projected wins by club"),
        p(class="small-note",if(effect$partial)"Partial impact: wins reflect only the applied payroll changes. A missing club amount is not zero." else "Model-based wins from the recorded payroll changes, not individual player talent."),
        tags$table(class="table table-sm",tags$thead(tags$tr(tags$th("Club"),tags$th("Payroll change"),tags$th("Projected wins change"),tags$th("CBT change"),tags$th("Status"))),
          tags$tbody(lapply(seq_len(nrow(ec)),function(j)tags$tr(tags$td(paste(ec$Team_ID[j],ec$Role[j])),
            tags$td(detail_money(ec$Payroll_Change[j])),tags$td(if(is.na(ec$Conditional_Wins[j]))"—"else sprintf("%+.3f",ec$Conditional_Wins[j])),
            tags$td(detail_money(ec$CBT_Change[j])),tags$td(ec$Payroll_Status[j]))))),
        p(class="small-note","Compared with the current saved scenario without this signing's applied changes. League-wide projected win changes sum to zero. Other clubs’ changes follow the Model 7 matchup schedule when a complete 162-game baseline is available. This is the payroll-based estimate, not the player-based estimate.")))
  })

  trades <- reactive({
    d <- tryCatch(read_trade_ledger(), error = function(e) NULL)
    validate(need(!is.null(d), "The trade history is not available."))
    d
  })
  observe({
    seasons <- sort(unique(trades()$Payroll_Season), decreasing = TRUE)
    updateSelectInput(session, "trade_season", choices = c("All seasons" = "all", seasons),
      selected = if (is.null(input$trade_season)) seasons[1] else input$trade_season)
  })
  trade_rows <- reactive({
    d <- trades()
    if(!is.null(input$trade_season)&&input$trade_season!="all") d <- d[d$Payroll_Season==input$trade_season,,drop=FALSE]
    d
  })
  trade_salary_reports <- reactive({
    read.csv("data/trade_salary_reports.csv",stringsAsFactors=FALSE,na.strings="")
  })
  trade_report <- function(x) {
    e <- trade_salary_reports()
    e[e$Movement_ID==x$Movement_ID & e$Season==as.integer(x$Payroll_Season),,drop=FALSE]
  }
  trade_salary_text <- function(x) {
    if(!is.na(x$Salary_Before_Trade_Millions)) return(detail_money(x$Salary_Before_Trade_Millions))
    e <- trade_report(x)
    if(nrow(e)) e$Salary[1] else "Not reported"
  }
  trade_deals <- reactive({
    d <- trade_rows()
    split(d,factor(d$Trade_ID,levels=unique(d$Trade_ID)))
  })
  output$trade_history_table <- renderDT({
    deals <- trade_deals()
    if(!length(deals)) return(app_table(data.frame(Trade="No trades for this season"),options=list(dom="t")))
    tab <- bind_rows(lapply(deals,function(x){
      clubs <- sort(unique(c(x$From_Team_ID,x$To_Team_ID)))
      side <- function(k) paste(x$Player[x$To_Team_ID==k],collapse=", ")
      salary <- vapply(seq_len(nrow(x)),function(i){
        z <- trade_salary_text(x[i,,drop=FALSE]); if(z=="Not reported") return(NA_character_)
        paste(x$Player[i],z,sep=": ")
      },character(1))
      data.frame(Date=x$Transaction_Date[1],
        "Team A receives"=paste0(clubs[1],": ",if(nzchar(side(clubs[1]))) side(clubs[1]) else "Cash / other consideration"),
        "Team B receives"=paste0(clubs[2],": ",if(nzchar(side(clubs[2]))) side(clubs[2]) else "Cash / other consideration"),
        "Reported salary"=if(all(is.na(salary)))"Not reported"else paste(na.omit(salary),collapse="; "),
        Source=source_link(x$Source_URL[1]),check.names=FALSE)
    }))
    app_table(tab,rownames=FALSE,escape=-ncol(tab),class="compact stripe hover",selection="single",
      options=list(pageLength=10,scrollX=TRUE,autoWidth=FALSE,order=list(list(0,"desc"))))
  })
  output$trade_detail <- renderUI({
    deals <- trade_deals(); i <- selected_trade_detail()
    if(length(i)!=1||i>length(deals)) return(p("Choose a trade."))
    x <- deals[[i]]; clubs <- sort(unique(c(x$From_Team_ID,x$To_Team_ID)))
    known <- function(z) if(is.na(z)) "Not reported" else detail_money(z)
    div(class="transaction-detail",h3(paste(clubs,collapse=" ↔ ")),p(class="small-note",paste(x$Transaction_Date[1],x$Status[1],sep=" · ")),
      div(class="trade-sides",lapply(clubs,function(k)div(h4(paste(k,"receives")),p(if(any(x$To_Team_ID==k))paste(x$Player[x$To_Team_ID==k],collapse=", ")else"Cash / other consideration")))),
      lapply(seq_len(nrow(x)),function(j){
        q <- x[j,,drop=FALSE]; e <- trade_report(q)
        div(class="trade-movement",strong(fo_name_link(q$Player,q$From_Team_ID)," ",paste(q$From_Team_ID,"→",q$To_Team_ID)),
          p(paste("Annual salary:",trade_salary_text(q))),
          p(paste("Salary kept by former team:",if(!is.na(q$Salary_Retained_By_From_Club_Millions))known(q$Salary_Retained_By_From_Club_Millions)else if(nrow(e))e$Retention[1]else"Not reported")),
          if(nrow(e))p(e$Notes[1],tags$a(" Salary report",href=e$Source_URL[1],target="_blank",rel="noopener noreferrer"),if(!is.na(e$Second_Source[1]))tags$a(" | Second report",href=e$Second_Source[1],target="_blank",rel="noopener noreferrer")),
          p(paste("Former team payroll change:",known(q$From_Club_Salary_Change_Millions),"| New team:",known(q$To_Club_Salary_Change_Millions))),
          p(paste("Luxury tax payroll changes:",known(q$From_Club_CBT_Change_Millions),"/",known(q$To_Club_CBT_Change_Millions))))
      }),
      p(paste(unique(na.omit(x$Deal_Notes)),collapse=" ")),
      p(class="small-note","Missing salary details are not assumed to be zero. Trade amounts are not added to the saved team payrolls without checking whether they are already included."),
      tags$a("Official trade report",href=x$Source_URL[1],target="_blank",rel="noopener noreferrer"))
  })

  current_policy <- reactive({
    
    upper_value <-
      if (input$upper_rule == "cap") {
        input$cap_value
      } else if (input$upper_rule == "cbt") {
        input$cbt_value
      } else {
        NA_real_
      }
    
    upper_tax_rate <-
      if (input$upper_rule == "cbt") {
        input$cbt_rate
      } else {
        0
      }
    
    lower_value <-
      if (input$lower_rule == "floor") {
        input$floor_value
      } else if (input$lower_rule == "tax") {
        input$low_benchmark
      } else {
        NA_real_
      }
    
    lower_tax_rate <-
      if (input$lower_rule == "tax") {
        input$low_tax_rate
      } else {
        0
      }
    
    list(
      upper_rule = input$upper_rule,
      upper_value = upper_value,
      upper_tax_rate = upper_tax_rate,
      lower_rule = input$lower_rule,
      lower_value = lower_value,
      lower_tax_rate = lower_tax_rate,
      revenue_sharing = input$revenue_sharing,
      required_spending = input$required_spending,
      central_media = input$central_media,
      player_revenue_share = input$player_revenue_share,
      minimum_salary_under2 = input$minimum_salary_under2,
      minimum_salary_2plus = input$minimum_salary_2plus,
      prearb_pool = input$prearb_pool,
      arbitration = input$arbitration,
      free_agency = input$free_agency,
      qualifying_offer = input$qualifying_offer,
      scenario_year=as.integer(input$scenario_year %||% "2027"),
      media_share_pct=input$media_share_pct %||% 100,media_revenue_pct=input$media_revenue_pct %||% 20,
      media_start=as.integer(input$media_start %||% "2027"),cap_start=as.integer(input$cap_start %||% "2027"),
      min_players_under2=input$min_players_under2 %||% 12,min_players_2plus=input$min_players_2plus %||% 5,
      prearb_min_bonus=input$prearb_min_bonus %||% 0,prearb_eligible=input$prearb_eligible %||% 12,
      arbitration_payroll=input$arbitration_payroll %||% 22,arbitration_bonus_pct=input$arbitration_bonus_pct %||% 0,
      arbitration_bonus_start=as.integer(input$arbitration_bonus_start %||% "2028"),veteran_bonus=input$veteran_bonus %||% 0,
      super_two_pct=input$super_two_pct %||% 22,arb_candidates=input$arb_candidates %||% 4,arb_raise=input$arb_raise %||% 2,
      min_arb_salary=input$min_arb_salary %||% 0,arb_floor_players=input$arb_floor_players %||% 3,arb_floor_baseline=input$arb_floor_baseline %||% 2,
      attendance_coef=input$attendance_coef %||% 0.7638659,revenue_growth=input$revenue_growth %||% 0.047417,payroll_coef=input$payroll_coef %||% payroll_effect
    )
  })
  
  observe({
    req(input$upper_rule, input$lower_rule)
    if (
      input$upper_rule == "cap" &&
      input$lower_rule == "floor"
    ) {
      
      req(
        input$cap_value,
        input$floor_value
      )
      
      if (
        input$floor_value >=
        input$cap_value
      ) {
        
        updateSliderInput(
          session,
          "cap_value",
          value =
            min(
              400,
              input$floor_value + 5
            )
        )
      }
    }
  })
  
  simulation <- reactive({
    
    simulate_policy(
      current_policy(),
      payroll_coef = current_policy()$payroll_coef,
      behavior_multiplier = input$behavior_response,
      full = TRUE,baseline=scenario_baseline()
    )
  })
  
  scores <- reactive({
    
    negotiation_scores(
      current_policy(),
      simulation()$metrics
    )
  })
  
  fo_actual_server(input,output,session,economic_forecast,free_agency_rows,trades)
  open_economic_transaction <- register_transaction_economics(input,output,session,
    reactive(projection_baseline(base,economic_forecast(),payroll_snapshot())),current_policy,comparison_context,free_agency_rows,trades)
  fo_profile_context_server(input,output,session,free_agency_rows,trades,open_economic_transaction)
  observeEvent(input$analyze_selected_tx,{
    type <- selected_tx_type(); req(type)
    key <- switch(type,
      contract_detail={x <- contract_rows()[selected_contract_detail(),,drop=FALSE];paste("contract",x$Contract_ID,x$Club,x$Obligation_Season,sep=":")},
      free_agency_detail={x <- free_agency_rows()[selected_free_agency_detail(),,drop=FALSE];k <- paste("contract",x$Signing_ID,x$New_Team_ID,2027,sep=":");if(any(contract_ledger()$Contract_ID==x$Signing_ID & contract_ledger()$Obligation_Season==2027))k else paste0("signing:",x$Signing_ID)},
      trade_detail=paste0("trade:",trade_deals()[[selected_trade_detail()]]$Trade_ID[1]))
    open_economic_transaction(key)
  },ignoreInit=TRUE)
  best_result <- reactiveVal(NULL)
  
  load_setup <- function(p) {
    p <- complete_policy(p)
    updateSelectInput(session,"cap_start",selected=as.character(p$cap_start))
    updateSelectInput(session,"media_start",selected=as.character(p$media_start))
    updateSelectInput(session,"arbitration_bonus_start",selected=as.character(p$arbitration_bonus_start))
    updateSliderInput(session,"arbitration_bonus_pct",value=p$arbitration_bonus_pct)
    updateSliderInput(session,"veteran_bonus",value=p$veteran_bonus)
    updateSliderInput(session,"prearb_min_bonus",value=p$prearb_min_bonus)
    updateSliderInput(session,"super_two_pct",value=p$super_two_pct)
    updateSliderInput(session,"min_arb_salary",value=p$min_arb_salary)
    updateSelectInput(session, "upper_rule", selected = p$upper_rule)
    if (p$upper_rule == "cap") updateSliderInput(session, "cap_value", value = p$upper_value)
    if (p$upper_rule == "cbt") {
      updateSliderInput(session, "cbt_value", value = p$upper_value)
      updateSliderInput(session, "cbt_rate", value = p$upper_tax_rate)
    }
    updateSelectInput(session, "lower_rule", selected = p$lower_rule)
    if (p$lower_rule == "floor") updateSliderInput(session, "floor_value", value = p$lower_value)
    if (p$lower_rule == "tax") {
      updateSliderInput(session, "low_benchmark", value = p$lower_value)
      updateSliderInput(session, "low_tax_rate", value = p$lower_tax_rate)
    }
    updateSliderInput(session, "revenue_sharing", value = p$revenue_sharing)
    updateSliderInput(session, "required_spending", value = p$required_spending)
    updateCheckboxInput(session, "central_media", value = p$central_media)
    updateSliderInput(session, "player_revenue_share", value = p$player_revenue_share)
    updateSliderInput(session, "minimum_salary_under2", value = p$minimum_salary_under2)
    updateSliderInput(session, "minimum_salary_2plus", value = p$minimum_salary_2plus)
    updateSliderInput(session, "prearb_pool", value = p$prearb_pool)
    updateSelectInput(session, "arbitration", selected = p$arbitration)
    updateSelectInput(session, "free_agency", selected = p$free_agency)
    updateCheckboxInput(session, "qualifying_offer", value = p$qualifying_offer)
    updateSliderInput(session, "behavior_response", value = 1)
  }

  observeEvent(input$load_current, load_setup(current_cba_policy()))
  observeEvent(input$load_mlb, load_setup(mlb_proposal_policy()))
  observeEvent(input$load_mlbpa, load_setup(mlbpa_proposal_policy()))

  observeEvent(input$reset_inputs, {
    p <- current_cba_policy()
    p$required_spending <- 50
    p$minimum_salary_under2 <- 0.90
    p$minimum_salary_2plus <- 1.00
    p$prearb_pool <- 65
    p$free_agency <- "5_age30"
    p$qualifying_offer <- FALSE
    load_setup(p)
    updateSliderInput(session, "cap_value", value = 258.2)
    updateSliderInput(session, "floor_value", value = 180.2)
    updateSliderInput(session, "low_benchmark", value = 150)
    updateSliderInput(session, "low_tax_rate", value = 25)
  })

  observeEvent(input$find_best, {
    terms <- current_policy()
    baseline <- scenario_baseline()
    result <- tryCatch(withProgress(message="Searching CBA packages...",value=0,{
      optimize_cba(terms,baseline,input$behavior_response,isTRUE(input$lock_player_terms),
        progress=function(i,n,stage) {
          setProgress(value=if(stage=="screen") .65*i/n else .65+.35*i/n,
            detail=if(stage=="screen") paste("Screening",i,"of",n,"packages") else paste("Sensitivity tests",i,"of",n))
        })
    }),error=function(e) { showNotification(conditionMessage(e),type="warning",duration=10); NULL })
    best_result(result)
    if(is.null(result)) return()
    updateSelectInput(session,"best_objective",choices=setNames(as.character(seq_len(nrow(result$alternatives))),result$alternatives$Objective),selected="1")
  })
  observeEvent(list(input$baseline_mode,input$scenario_year), { best_result(NULL) },ignoreInit=TRUE)

  output$policy_name <- renderText({
    policy_label(
      current_policy()
    )
  })
  
  output$player_comp_change <- renderText(money_text(simulation()$metrics$player_comp_change,1))
  output$club_effect <- renderText(money_text(simulation()$metrics$club_effect,1))
  output$result_support_bars <- renderUI({
    s <- scores()
    tagList(lapply(c("MLB Support","MLBPA Support","Compromise Score"),function(label){
      v <- switch(label,"MLB Support"=s$mlb,"MLBPA Support"=s$mlbpa,s$compromise)
      div(class="support-row",div(class="support-label",span(label),strong(score_text(v))),
        div(class="support-track",div(class="support-fill",style=paste0("width:",v,"%"))))
    }))
  })
  output$gini <- renderText({
    fmt3(
      simulation()$metrics$gini
    )
  })
  
  output$win_spread <- renderText({
    paste0(
      fmt2(
        simulation()$metrics$win_spread
      ),
      " wins"
    )
  })
  
  output$balance_change <- renderText({
    percent_text(
      simulation()$metrics$competitive_balance_change,
      1
    )
  })
  
  output$player_comp <- renderText({
    paste0(
      "$",
      fmt2(
        simulation()$metrics$player_comp /
          1000
      ),
      "B"
    )
  })
  
  output$payroll_change <- renderText({
    money_text(
      simulation()$metrics$payroll_change,
      1
    )
  })
  
  output$revenue_change <- renderText({
    money_text(
      simulation()$metrics$revenue_change,
      1
    )
  })
  
  output$cbt_exposure <- renderText({
    paste0(
      "$",
      fmt1(
        simulation()$metrics$cbt_tax
      ),
      "M"
    )
  })
  
  output$low_tax_exposure <- renderText({
    paste0(
      "$",
      fmt1(
        simulation()$metrics$low_spending_tax
      ),
      "M"
    )
  })
  
  output$what_changed <- renderUI({
    
    m <- simulation()$metrics
    team <- simulation()$team
    
    payroll_up <-
      sum(
        team$Payroll_Change_M >
          0.01
      )
    
    payroll_down <-
      sum(
        team$Payroll_Change_M <
          -0.01
      )
    
    tags$ul(
      
      tags$li(
        paste0(
          payroll_up,
          " clubs increase payroll and ",
          payroll_down,
          " clubs reduce payroll."
        )
      ),
      
      tags$li(
        paste0(
          "Total MLB payroll changes by ",
          money_text(
            m$payroll_change,
            1
          ),
          "."
        )
      ),
      
      tags$li(
        paste0(
          "Win Total Spread changes from ",
          fmt2(
            m$baseline_win_spread
          ),
          " wins to ",
          fmt2(
            m$win_spread
          ),
          " wins."
        )
      ),
      
      tags$li(
        paste0(
          "Competitive Balance Change is ",
          percent_text(
            m$competitive_balance_change,
            1
          ),
          "."
        )
      ),
      
      tags$li(
        paste0(
          "Modeled player compensation changes by ",
          money_text(
            m$player_comp_change,
            1
          ),
          "."
        )
      )
    )
  })
  
  output$payroll_plot <- renderPlotly({
    plot_data <- simulation()$team %>% group_by(Payroll_Change_M,Change_Wins) %>%
      summarise(Clubs=paste(Team_Name,collapse="<br>"),Count=n(),.groups="drop") %>%
      mutate(Hover=paste0("<b>",Count,ifelse(Count==1," club"," clubs"),"</b><br>",Clubs,
        "<br>Payroll change: ",money_text(Payroll_Change_M,2),"<br>Predicted win change: ",sprintf("%+.3f",Change_Wins)),
        PointSize=8+3*sqrt(Count),Color=ifelse(Change_Wins>1e-8,"#25816b",ifelse(Change_Wins< -1e-8,"#b65b54","#718398")))
    xr <- max(10,abs(plot_data$Payroll_Change_M))*1.10
    yr <- max(.5,abs(plot_data$Change_Wins))*1.15
    plot_ly(plot_data,x=~Payroll_Change_M,y=~Change_Wins,type="scatter",mode="markers",
      text=~Hover,hoverinfo="text",marker=list(size=plot_data$PointSize,color=plot_data$Color,opacity=.85,line=list(color="#ffffff",width=1.2))) %>%
      layout(xaxis=list(title="Change in payroll ($M)",range=c(-xr,xr),automargin=TRUE,zeroline=TRUE,zerolinecolor="#9aa9b8",gridcolor="#e8edf2"),
        yaxis=list(title="Predicted change in wins",range=c(-yr,yr),automargin=TRUE,zeroline=TRUE,zerolinecolor="#9aa9b8",gridcolor="#e8edf2"),
        font=list(family="Arial",color="#31465b"),paper_bgcolor="#ffffff",plot_bgcolor="#ffffff",
        hovermode="closest",margin=list(b=60,l=65,r=20,t=10)) %>% config(displaylogo=FALSE)
  })

  output$payroll_lorenz <- renderPlotly({
    s <- simulation()$team
    lorenz <- function(x) c(0,cumsum(sort(x))/sum(x))
    p <- seq(0,1,length.out=nrow(s)+1)
    plot_ly(x=p,y=p,type="scatter",mode="lines",name="Equal payrolls",line=list(color="#9aa9b8",dash="dash"),hoverinfo="skip") %>%
      add_lines(x=p,y=lorenz(s$Original_Payroll_M),name="Baseline",line=list(color="#718398")) %>%
      add_lines(x=p,y=lorenz(s$New_Payroll_M),name="Selected CBA",line=list(color="#245d91",width=3)) %>%
      layout(xaxis=list(title="Cumulative share of clubs",tickformat=".0%"),yaxis=list(title="Cumulative share of payroll",tickformat=".0%"),
        legend=list(orientation="h",x=0,y=-.23),margin=list(b=90,l=65,t=8,r=10)) %>% config(displaylogo=FALSE)
  })
  output$impact_table <- renderDT({
    
    simulation()$team %>%
      arrange(
        desc(
          abs(Change_Wins)
        )
      ) %>%
      slice_head(
        n = 10
      ) %>%
      transmute(
        Club = Team_Name,
        
        `Baseline Payroll ($M)` =
          round(
            Original_Payroll_M,
            1
          ),
        
        `Simulated Payroll ($M)` =
          round(
            New_Payroll_M,
            1
          ),
        
        `Payroll Change ($M)` =
          round(
            Payroll_Change_M,
            1
          ),
        
        `Predicted Win Change` =
          round(
            Change_Wins,
            2
          ),
        
        `Revenue Change ($M)` =
          round(
            Revenue_Change_M,
            1
          )
      ) %>%
      app_table(
        rownames = FALSE,
        options =
          list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            ordering = TRUE
          )
      )
  })
  
  output$team_table <- renderDT({
    
    table_data <-
      simulation()$team %>%
      transmute(
        Club = Team_Name,
        
        `Baseline Payroll ($M)` =
          round(
            Original_Payroll_M,
            1
          ),
        
        `Simulated Payroll ($M)` =
          round(
            New_Payroll_M,
            1
          ),
        
        `Payroll Change ($M)` =
          round(
            Payroll_Change_M,
            1
          ),
        
        `Predicted Wins` =
          round(
            Predicted_Wins,
            1
          ),
        
        `Predicted Win Change` =
          round(
            Change_Wins,
            2
          ),
        
        `Historical Payroll Efficiency` =
          round(
            Historical_Efficiency,
            2
          ),
        
        `Revenue Change ($M)` =
          round(
            Revenue_Change_M,
            1
          ),
        
        `CBT Exposure ($M)` =
          round(
            CBT_Exposure_M,
            1
          ),
        
        `Low-Spending Tax ($M)` =
          round(
            Low_Spending_Tax_M,
            1
          )
      )
    
    app_table(
      table_data,
      rownames = FALSE,
      options =
        list(
          pageLength = 30,
          lengthChange = FALSE,
          searching = TRUE,
          info = FALSE,
          
          order =
            list(
              list(
                0,
                "asc"
              )
            ),
          
          columnDefs =
            list(
              list(
                targets = 0,
                orderSequence =
                  c(
                    "asc",
                    "desc"
                  )
              ),
              
              list(
                targets = 1:9,
                orderSequence =
                  c(
                    "desc",
                    "asc"
                  )
              )
            )
        )
    )
  })
  
  output$mlb_score <- renderText({
    score_text(
      scores()$mlb
    )
  })
  
  output$mlbpa_score <- renderText({
    score_text(
      scores()$mlbpa
    )
  })
  
  output$compromise_score <- renderText({
    score_text(
      scores()$compromise
    )
  })
  
  output$viability_score <- renderText({
    paste0(
      fmt1(
        scores()$viability
      ),
      "/100"
    )
  })
  
  output$negotiation_verdict <- renderText({
    s <- scores()
    paste(if(s$mlb<s$mlbpa) "MLB is the limiting side." else if(s$mlbpa<s$mlb) "MLBPA is the limiting side." else "Both indices are equal.",
      "Difference between support scores:",fmt1(s$gap),"points.")
  })

  output$bargaining_benchmarks <- renderDT({
    app_table(scores()$benchmark_components %>% mutate(across(where(is.numeric),~round(.x,3))),rownames=FALSE,options=list(dom="t",scrollX=TRUE))
  })
  output$mlb_components <- renderDT({
    
    scores()$mlb_components %>%
      mutate(
        Weight =
          paste0(
            Weight,
            "%"
          ),
        
        Score =
          round(
            Score,
            1
          )
      ) %>%
      app_table(
        rownames = FALSE,
        options =
          list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            info = FALSE
          )
      )
  })
  
  output$mlbpa_components <- renderDT({
    
    scores()$mlbpa_components %>%
      mutate(
        Weight =
          paste0(
            Weight,
            "%"
          ),
        
        Score =
          round(
            Score,
            1
          )
      ) %>%
      app_table(
        rownames = FALSE,
        options =
          list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            info = FALSE
          )
      )
  })
  
  output$mlb_reasons <- renderUI({
    
    policy <- current_policy()
    m <- simulation()$metrics
    
    reasons <- c()
    
    if (
      m$competitive_balance_change >
      0
    ) {
      
      reasons <-
        c(
          reasons,
          paste0(
            "POSITIVE: Win Total Spread falls, producing a ",
            fmt1(
              m$competitive_balance_change
            ),
            "% improvement in competitive balance versus the selected baseline."
          )
        )
      
    } else {
      
      reasons <-
        c(
          reasons,
          paste0(
            "CONCERN: Win Total Spread increases, producing a ",
            fmt1(
              abs(
                m$competitive_balance_change
              )
            ),
            "% deterioration in competitive balance."
          )
        )
    }
    
    if (m$payroll_change > 0) {
      
      reasons <-
        c(
          reasons,
          paste0(
            "CONCERN: Combined club payroll increases by approximately $",
            fmt1(
              m$payroll_change
            ),
            "M."
          )
        )
      
    } else {
      
      reasons <-
        c(
          reasons,
          paste0(
            "POSITIVE: Combined club payroll decreases by approximately $",
            fmt1(
              abs(
                m$payroll_change
              )
            ),
            "M."
          )
        )
    }
    
    if (policy$central_media) {
      reasons <-
        c(
          reasons,
          "POSITIVE: The package centralizes media revenues, aligning with MLB's current negotiating direction."
        )
    }
    
    if (policy$upper_rule == "cap") {
      reasons <-
        c(
          reasons,
          "POSITIVE: The package uses a Salary Cap, a central feature of MLB's current proposal."
        )
    }
    
    if (policy$lower_rule == "floor") {
      reasons <-
        c(
          reasons,
          "POSITIVE: The Payroll Floor requires lower-spending clubs to increase Major League payroll."
        )
    }
    
    tags$ul(
      lapply(
        reasons,
        tags$li
      )
    )
  })
  
  output$mlbpa_reasons <- renderUI({
    
    policy <- current_policy()
    m <- simulation()$metrics
    
    reasons <- c()
    
    if (policy$upper_rule == "cap") {
      
      reasons <-
        c(
          reasons,
          "MAJOR CONCERN: A Salary Cap directly restricts the upper end of the player labor market."
        )
      
    } else if (policy$upper_rule == "cbt") {
      
      reasons <-
        c(
          reasons,
          "PARTIAL POSITIVE: The CBT taxes high spending but does not create a hard maximum payroll."
        )
      
    } else {
      
      reasons <-
        c(
          reasons,
          "POSITIVE: There is no upper payroll restriction on club spending."
        )
    }
    
    if (m$player_comp_change >= 0) {
      
      reasons <-
        c(
          reasons,
          paste0(
            "POSITIVE: Modeled player compensation increases by approximately $",
            fmt1(
              m$player_comp_change
            ),
            "M."
          )
        )
      
    } else {
      
      reasons <-
        c(
          reasons,
          paste0(
            "CONCERN: Modeled player compensation decreases by approximately $",
            fmt1(
              abs(
                m$player_comp_change
              )
            ),
            "M."
          )
        )
    }
    
    if (
      policy$minimum_salary_under2 >=
      0.90
    ) {
      
      reasons <-
        c(
          reasons,
          paste0(
            "POSITIVE: The minimum salary for players with less than two years of Major League service is $",
            fmt2(
              policy$minimum_salary_under2
            ),
            "M."
          )
        )
    }
    
    if (policy$prearb_pool > 50) {
      
      reasons <-
        c(
          reasons,
          paste0(
            "POSITIVE: The Pre-Arbitration Bonus Pool increases to $",
            fmt1(
              policy$prearb_pool
            ),
            "M."
          )
        )
    }
    
    if (policy$arbitration == "2") {
      reasons <-
        c(
          reasons,
          "POSITIVE: Earlier salary-arbitration eligibility gives players additional salary leverage."
        )
    }
    
    if (
      policy$free_agency ==
      "5_age30"
    ) {
      
      reasons <-
        c(
          reasons,
          "POSITIVE: Certain age-30+ players can reach free agency after five years of Major League service."
        )
    }
    
    if (!policy$qualifying_offer) {
      
      reasons <-
        c(
          reasons,
          "POSITIVE: Qualifying Offer / Draft Pick Compensation is eliminated."
        )
    }
    
    if (
      policy$lower_rule %in%
      c(
        "floor",
        "tax"
      )
    ) {
      
      reasons <-
        c(
          reasons,
          "POSITIVE: The low-payroll rule creates pressure for lower-spending clubs to put more money into Major League payroll."
        )
    }
    
    tags$ul(
      lapply(
        reasons,
        tags$li
      )
    )
  })
  
  output$best_policy_box <- renderUI({
    
    b <- best_result()
    
    req(b)
    
    policy <- b$policy
    
    upper_text <-
      if (policy$upper_rule == "cap") {
        
        paste0(
          "Salary Cap: $",
          fmt1(
            policy$upper_value
          ),
          "M"
        )
        
      } else if (policy$upper_rule == "cbt") {
        
        paste0(
          "CBT Threshold: $",
          fmt1(
            policy$upper_value
          ),
          "M at ",
          fmt1(
            policy$upper_tax_rate
          ),
          "%"
        )
        
      } else {
        
        "No Upper Payroll Restriction"
      }
    
    lower_text <-
      if (policy$lower_rule == "floor") {
        
        paste0(
          "Payroll Floor: $",
          fmt1(
            policy$lower_value
          ),
          "M"
        )
        
      } else if (policy$lower_rule == "tax") {
        
        paste0(
          "Low-Spending Tax Benchmark: $",
          fmt1(
            policy$lower_value
          ),
          "M at ",
          fmt1(
            policy$lower_tax_rate
          ),
          "%"
        )
        
      } else {
        
        "No Lower Payroll Restriction"
      }
    
    arbitration_text <-
      ifelse(
        policy$arbitration == "2",
        "Earlier Eligibility (~2 Years)",
        "Generally Current Timing (~3 Years)"
      )
    
    free_agency_text <-
      ifelse(
        policy$free_agency == "5_age30",
        "5 Years for Certain Age-30+ Players",
        "Generally 6 Years of Major League Service"
      )
    
    div(
      class = "best-box",
      
      div(
        class = "best-title",
        policy_label(policy)
      ),
      
      div(
        class = "deal-section",
        
        h4("Payroll System"),
        
        div(
          class = "deal-row",
          strong("High-Payroll Rule: "),
          upper_text
        ),
        
        div(
          class = "deal-row",
          strong("Low-Payroll Rule: "),
          lower_text
        )
      ),
      
      div(
        class = "deal-section",
        
        h4("Revenue Structure"),
        
        div(
          class = "deal-row",
          strong("Revenue Sharing: "),
          paste0(
            fmt1(
              policy$revenue_sharing
            ),
            "%"
          )
        ),
        
        div(
          class = "deal-row",
          strong("Revenue-Sharing Transfer Required for Payroll: "),
          paste0(
            fmt1(
              policy$required_spending
            ),
            "%"
          )
        ),
        
        div(
          class = "deal-row",
          strong("Centralize Media Revenues: "),
          ifelse(
            policy$central_media,
            "Yes",
            "No"
          )
        ),
        
        div(
          class = "deal-row",
          strong("Baseball Revenue Going to Players: "),
          paste0(
            fmt1(
              policy$player_revenue_share
            ),
            "%"
          )
        )
      ),
      
      div(
        class = "deal-section",
        
        h4("Player Pay"),
        
        div(
          class = "deal-row",
          strong("Minimum Salary, <2 Years Major League Service: "),
          paste0(
            "$",
            fmt2(
              policy$minimum_salary_under2
            ),
            "M"
          )
        ),
        
        div(
          class = "deal-row",
          strong("Minimum Salary, 2+ Years Major League Service: "),
          paste0(
            "$",
            fmt2(
              policy$minimum_salary_2plus
            ),
            "M"
          )
        ),
        
        div(
          class = "deal-row",
          strong("Pre-Arbitration Bonus Pool: "),
          paste0(
            "$",
            fmt1(
              policy$prearb_pool
            ),
            "M"
          )
        )
      ),
      
      div(
        class = "deal-section",
        
        h4("Player Rights"),
        
        div(
          class = "deal-row",
          strong("Salary Arbitration: "),
          arbitration_text
        ),
        
        div(
          class = "deal-row",
          strong("Free Agency: "),
          free_agency_text
        ),
        
        div(
          class = "deal-row",
          strong("Qualifying Offer / Draft Pick Compensation: "),
          ifelse(
            policy$qualifying_offer,
            "Keep",
            "Eliminate"
          )
        ),
        
        p(
          class = "small-note",
          "Clubs may negotiate extensions with players before free agency."
        )
      )
    )
  })
  
  output$optimizer_alternatives <- renderDT({
    b <- best_result();req(b)
    d <- b$alternatives
    policies <- b$alternative_policies
    app_table(data.frame(Objective=d$Objective,Setup=vapply(policies,policy_label,character(1)),`MLB support`=round(d$central_mlb,1),`MLBPA support`=round(d$central_mlbpa,1),`Compromise score (/100)`=round(d$central_compromise,2),`Economic index (/100)`=round(100*d$median_economic,2),`Median competitive balance change (%)`=round(d$median_balance,2),`Median player change ($M)`=round(d$median_player_comp,1),`Balance and player pay maintained`=ifelse(d$qualified,"Yes","No"),check.names=FALSE),rownames=FALSE,options=list(dom="t",ordering=FALSE,scrollX=TRUE))
  })
  output$best_search_summary <- renderUI({
    
    b <- best_result()
    
    req(b)
    
    total_runs <-
      b$screened + b$tested *
      b$assumptions
    
    div(
      class = "plain-box",
      
      strong(if(b$qualified) "Highest mutual support among economically eligible screened packages" else "No economically qualifying package found — financial tradeoff only"),
      p(paste0("Compromise score: ",fmt2(b$best$central_compromise),"/100. Economic comparison score: ",fmt2(100*b$best$median_economic),"/100. Neither is an acceptance probability.")),
      p(paste("Higher-scoring candidate audit:",b$checked_for_max,"packages checked in descending mutual support order.")),
      p(paste0("Starting weaker-side support: ",fmt1(b$starting_support),"/100. Highest central weaker-side support found (realistic ranges and cost limits): ",fmt1(b$best_central_support),"/100. A high score alone does not show that an agreement is affordable or improves competitive balance. The chosen package maximizes central mutual support among economically eligible screened packages; no minimum bargaining score applies.")),
      p(paste("Initial search:",format(b$screened,big.mark=","),"packages. Agreements tested under different assumptions:",b$tested,"packages. The search finds the best eligible agreement among the combinations tested.")),
      strong("Search process:"),
      
      p(
        paste0(
          "The model tested ",
          format(
            b$tested,
            big.mark = ","
          ),
          " policy packages under ",
          b$assumptions,
          " assumptions each, producing ",
          format(
            total_runs,
            big.mark = ","
          ),
          " total simulations."
        )
      ),
      
      p(
        paste0(
          "The recommended package has MLB Support of ",
          fmt1(
            b$scores$mlb
          ),
          "/100, MLBPA Support of ",
          fmt1(
            b$scores$mlbpa
          ),
          "/100 and Estimated Negotiation Viability of ",
          fmt1(
            b$scores$viability
          ),
          "/100."
        )
      )
    )
  })
  
  output$best_packages <- renderText({
    
    b <- best_result()
    
    req(b)
    
    format(
      b$tested,
      big.mark = ","
    )
  })
  
  output$best_assumptions <- renderText({
    
    b <- best_result()
    
    req(b)
    
    b$assumptions
  })
  
  output$best_runs <- renderText({
    
    b <- best_result()
    
    req(b)
    
    format(
      b$tested *
        b$assumptions,
      big.mark = ","
    )
  })
  
  output$best_gini <- renderText({
    
    b <- best_result()
    
    req(b)
    
    fmt3(
      b$simulation$metrics$gini
    )
  })
  
  output$best_win_spread <- renderText({
    
    b <- best_result()
    
    req(b)
    
    paste0(
      fmt2(
        b$simulation$metrics$win_spread
      ),
      " wins"
    )
  })
  
  output$best_balance_change <- renderText({
    
    b <- best_result()
    
    req(b)
    
    percent_text(
      b$simulation$metrics$competitive_balance_change,
      1
    )
  })
  
  output$best_player_comp <- renderText({
    
    b <- best_result()
    
    req(b)
    
    paste0(
      "$",
      fmt2(
        b$simulation$metrics$player_comp /
          1000
      ),
      "B"
    )
  })
  
  output$best_mlb_support <- renderText({
    
    b <- best_result()
    
    req(b)
    
    score_text(
      b$scores$mlb
    )
  })
  
  output$best_mlbpa_support <- renderText({
    
    b <- best_result()
    
    req(b)
    
    score_text(
      b$scores$mlbpa
    )
  })
  
  output$best_compromise_score <- renderText({
    
    b <- best_result()
    
    req(b)
    
    score_text(
      b$scores$compromise
    )
  })
  
  output$best_viability <- renderText({
    
    b <- best_result()
    
    req(b)
    
    paste0(
      fmt1(
        b$scores$viability
      ),
      "/100"
    )
  })
  
  output$best_reason <- renderUI({
    
    b <- best_result()
    
    req(b)
    
    m <- b$simulation$metrics
    s <- b$scores
    
    tags$ul(
      
      tags$li(
        paste0(
          "Competitive Balance Change: ",
          percent_text(
            m$competitive_balance_change,
            1
          ),
          "."
        )
      ),
      
      tags$li(
        paste0(
          "Win Total Spread changes from ",
          fmt2(
            m$baseline_win_spread
          ),
          " to ",
          fmt2(
            m$win_spread
          ),
          " wins."
        )
      ),
      
      tags$li(
        paste0(
          "Modeled player compensation changes by ",
          money_text(
            m$player_comp_change,
            1
          ),
          "."
        )
      ),
      
      tags$li(
        paste0(
          "MLB Support is ",
          fmt1(
            s$mlb
          ),
          "/100 and MLBPA Support is ",
          fmt1(
            s$mlbpa
          ),
          "/100."
        )
      ),
      
      tags$li(
        paste0(
          "Estimated Negotiation Viability is ",
          fmt1(
            s$viability
          ),
          "/100."
        )
      ),
      
      tags$li(
        if(b$locked) "Player pay and rights were held fixed." else "The search considered alternative compensation and player-rights packages."
      )
    )
  })
  
  observeEvent(input$apply_best, {
    b <- best_result();req(b)
    i <- as.integer(input$best_objective %||% "1")
    req(i>=1,i<=length(b$alternative_policies))
    load_setup(b$alternative_policies[[i]])
    updateSliderInput(session,"behavior_response",value=b$behavior)
  })

  output$comparison_table <- renderDT({
    policy_column <- function(policy) {
      policy <- preset_with_context(policy)
      sim <- simulate_policy(policy,payroll_coef=policy$payroll_coef,behavior_multiplier=1,full=FALSE,baseline=scenario_baseline())
      metrics_column(sim$metrics,negotiation_scores(policy,sim$metrics))
    }
    metrics_column <- function(m, s) {
      c(fmt3(m$gini), paste0(fmt2(m$win_spread), " wins"),
        percent_text(m$competitive_balance_change, 1),
        paste0("$", fmt2(m$player_comp / 1000), "B"),
        money_text(m$payroll_change, 1), money_text(m$revenue_change, 1),
        score_text(s$mlb), score_text(s$mlbpa), score_text(s$compromise),
        score_text(s$viability))
    }
    b <- best_result()
    comparison <- data.frame(
      Metric = c("Payroll Gini coefficient", "Win Total Spread", "Competitive Balance Change",
        "Modeled Player Compensation", "Total Payroll Change", "Predicted Revenue Change",
        "MLB Support", "MLBPA Support", "Compromise Score", "Negotiation Viability"),
      "Current CBA" = policy_column(current_cba_policy()),
      "MLB Proposal Scenario" = policy_column(mlb_proposal_policy()),
      "MLBPA Scenario" = policy_column(mlbpa_proposal_policy()),
      "Selected Setup" = metrics_column(simulation()$metrics, scores()),
      "Best Compromise" = if (is.null(b)) rep("Run Best Compromise", 10)
        else metrics_column(b$simulation$metrics, b$scores),
      check.names = FALSE
    )
    app_table(comparison, rownames = FALSE,
      options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, scrollX = TRUE))
  })
}

shinyApp(
  ui = ui,
  server = server
)
