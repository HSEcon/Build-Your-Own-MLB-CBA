# Benchmark-relative bargaining utility indices. Not estimated acceptance probabilities.
# Preset outcomes use the same baseline, scenario year and empirical assumptions as the candidate.
bargaining_cache <- new.env(parent=emptyenv())
bargaining_assumptions <- c("scenario_year","media_revenue_pct","min_players_under2",
  "min_players_2plus","prearb_eligible","arbitration_payroll","arb_candidates","arb_raise",
  "arb_floor_players","arb_floor_baseline","attendance_coef","revenue_growth","payroll_coef")

bargaining_features <- function(policy,m) {
  p <- complete_policy(policy)
  c(balance=m$competitive_balance_change, payroll_equality=m$baseline_gini-m$gini,
    club_net=m$club_effect, club_downside=m$worst_club_effect_share,
    revenue_equality=m$revenue_gini_reduction, player_pay=m$player_comp_change,
    early_pay=m$minimum_salary_cost+m$earlier_arb_cost+m$arbitration_floor_cost+
      m$prearb_delta+m$arbitration_bonus,
    lower_club_spending=m$low_revenue_payroll_change,
    market_tax=-m$cbt_tax,
    media=if(p$central_media) p$media_share_pct else 0,
    cap=as.numeric(p$upper_rule=="cap"), floor=as.numeric(p$lower_rule=="floor"),
    freedom=as.numeric(p$upper_rule!="cap"),
    arbitration=if(p$arbitration=="2") 100 else p$super_two_pct,
    free_agency=as.numeric(p$free_agency=="5_age30"), qo_removed=as.numeric(!p$qualifying_offer))
}

bargaining_reference <- function(ctx) {
  key <- digest::digest(ctx)
  if(exists(key,envir=bargaining_cache,inherits=FALSE)) return(get(key,envir=bargaining_cache))
  policies <- list(current=current_cba_policy(),MLB=mlb_proposal_policy(),MLBPA=mlbpa_proposal_policy())
  f <- t(vapply(policies,function(p) {
    p <- modifyList(complete_policy(p),ctx$assumptions)
    m <- simulate_policy(p,payroll_coef=ctx$coef,behavior_multiplier=ctx$response,
      baseline=ctx$baseline,full=FALSE)$metrics
    bargaining_features(p,m)
  },numeric(16)))
  ref <- list(low=apply(f,2,min),high=apply(f,2,max),features=f)
  if(length(ls(bargaining_cache))>100) rm(list=ls(bargaining_cache),envir=bargaining_cache)
  assign(key,ref,envir=bargaining_cache)
  ref
}

benchmark_score <- function(value,low,high) {
  span <- high-low
  # Identical reference outcomes do not establish a preference scale.
  out <- rep(50,length(value)); names(out) <- names(value)
  varied <- is.finite(span)&span>1e-9
  out[varied] <- 100*clamp((value[varied]-low[varied])/span[varied],0,1)
  out
}

negotiation_scores <- function(policy,metrics,details=TRUE) {
  stopifnot(!is.null(metrics$score_context))
  ref <- bargaining_reference(metrics$score_context)
  raw <- bargaining_features(policy,metrics)
  z <- benchmark_score(raw,ref$low,ref$high)
  # Retain the existing category weights. Within categories use equal weighting
  # where no evidence identifies relative preferences. Explicit proposal preferences
  # remain ordinal; no invented acceptance probabilities or absolute dollar-to-point rates.
  owner <- c(mean(z[c("balance","payroll_equality")]),
    mean(z[c("club_net","club_downside")]),
    mean(z[c("revenue_equality","media")]),
    mean(z[c("cap","floor")]))
  union <- c(z[["player_pay"]],mean(z[c("freedom","market_tax")]),z[["early_pay"]],
    mean(z[c("arbitration","free_agency","qo_removed")]),
    mean(z[c("lower_club_spending","media")]))
  ow <- c(30,25,20,25); uw <- c(30,25,15,20,10)
  mlb <- sum(ow*owner)/100; mlbpa <- sum(uw*union)/100
  compromise <- min(mlb,mlbpa); gap <- abs(mlb-mlbpa)
  viability <- clamp(.70*compromise+.30*mean(c(mlb,mlbpa))-.40*gap,0,100)
  result <- list(mlb=mlb,mlbpa=mlbpa,compromise=compromise,viability=viability,
    joint_support=sqrt(mlb*mlbpa),gap=gap)
  if(!details) return(result)
  result$mlb_components <- tibble(Component=c("Competitive balance and payroll equality",
    "Aggregate and worst-club financial effect","Revenue equality and media pooling",
    "Hard cap and floor proposal alignment"),Weight=ow,Score=owner)
  result$mlbpa_components <- tibble(Component=c("Modeled player compensation",
    "Labor-market freedom and CBT exposure","Modeled early-career compensation",
    "Arbitration, free agency and qualifying offer rights","Lower-revenue club spending and media pooling"),
    Weight=uw,Score=union)
  result$benchmark_components <- tibble(Indicator=names(raw),Current=as.numeric(ref$features["current",]),
    MLB=as.numeric(ref$features["MLB",]),MLBPA=as.numeric(ref$features["MLBPA",]),
    Selected=as.numeric(raw),Normalized=as.numeric(z))
  result
}
