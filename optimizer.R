# Proposal-inspired screening ranges, not legal limits or calibrated acceptance estimates.
# Unrestricted manual scenarios remain available.
recommendation_checks <- function(p) {
  p <- complete_policy(p)
  inside <- function(x,lo,hi) length(x)==1 && is.finite(x) && x>=lo && x<=hi
  issues <- character()
  if(!p$upper_rule %in% c("cap","cbt")) issues <- c(issues,"No upper-payroll restriction")
  if(p$upper_rule=="cap") {
    if(!inside(p$upper_value,220,300)) issues <- c(issues,"Cap outside $220–300M")
    if(p$cap_start<2028) issues <- c(issues,"Cap transition before 2028")
    if(p$lower_rule!="floor") issues <- c(issues,"Cap without floor")
    if(p$lower_rule=="floor" && (!is.finite(p$lower_value)||p$upper_value-p$lower_value<50||p$lower_value>0.8*p$upper_value)) issues <- c(issues,"Cap and floor too close")
  }
  if(p$upper_rule=="cbt" && (!inside(p$upper_value,220,325)||!inside(p$upper_tax_rate,10,60))) issues <- c(issues,"CBT outside $220–325M or 10–60%")
  if(p$lower_rule!="none" && !inside(p$lower_value,120,190)) issues <- c(issues,"Lower threshold outside $120–190M")
  if(p$lower_rule=="tax" && !inside(p$lower_tax_rate,10,60)) issues <- c(issues,"Low-spending tax outside 10–60%")
  if(p$upper_rule=="cbt" && p$lower_rule!="none" && p$upper_value-p$lower_value<50) issues <- c(issues,"Upper and lower thresholds too close")
  if(!inside(p$revenue_sharing,10,35)||!inside(p$required_spending,0,75)) issues <- c(issues,"Sharing outside 10–35% / 0–75%")
  if(!p$central_media || p$media_share_pct!=100 || !p$media_start %in% c(2027,2028)) issues <- c(issues,"Preserve equal media pooling in principle")
  if(!inside(p$player_revenue_share,45,55)) issues <- c(issues,"Player revenue share outside 45–55%")
  if(!inside(p$minimum_salary_under2,.78,1.5)||!inside(p$minimum_salary_2plus,.78,1.5)||p$minimum_salary_under2>p$minimum_salary_2plus) issues <- c(issues,"Minimum salary outside proposal-inspired ranges or reversed service tiers")
  if(!inside(p$prearb_pool,50,180)||p$prearb_pool<30*p$prearb_min_bonus*p$prearb_eligible) issues <- c(issues,"Bonus pool outside $50–180M or insufficient to fund minimum")
  if(p$arbitration!="3"||!inside(p$super_two_pct,22,44)||!inside(p$min_arb_salary,0,3)) issues <- c(issues,"Arbitration provisions beyond reported proposals")
  if(!inside(p$arbitration_bonus_pct,0,5)||!inside(p$veteran_bonus,0,.5)||!inside(p$prearb_min_bonus,0,.1)) issues <- c(issues,"Bonuses beyond proposal ranges")
  if(p$arbitration_bonus_pct>0&&p$arbitration_bonus_start<2028) issues <- c(issues,"Settlement bonus before proposed 2028 class")
  issues
}

# Central screening and robust sensitivity testing. Heuristic support, not ratification probabilities.
optimizer_grid <- function() {
  upper <- bind_rows(tibble(upper_rule="cap",upper_value=c(220,250,258.2,280),upper_tax_rate=0),
    crossing(upper_rule="cbt",upper_value=c(220,250,280,300),upper_tax_rate=c(20,40,60)),
    tibble(upper_rule="none",upper_value=NA_real_,upper_tax_rate=0))
  lower <- bind_rows(tibble(lower_rule="floor",lower_value=c(120,150,180,180.2),lower_tax_rate=0),
    crossing(lower_rule="tax",lower_value=c(120,150,180),lower_tax_rate=c(20,40,60)),
    tibble(lower_rule="none",lower_value=NA_real_,lower_tax_rate=0))
  crossing(upper,lower,revenue_sharing=c(10,20,30),required_spending=c(25,50,75)) %>%
    filter(!(upper_rule=="cap"&lower_rule=="floor"&upper_value<=lower_value))
}
optimize_cba <- function(terms,baseline,behavior=1,lock_player_terms=FALSE,
                        grid=optimizer_grid(),shortlist_size=40,progress=function(i,n,stage){}) {
  terms <- complete_policy(terms)
  # Maximize weaker-side support subject to economic and contractual feasibility.
  variants <- list(terms)
  if(!lock_player_terms) variants <- c(variants,list(
    modifyList(terms,list(minimum_salary_under2=1,minimum_salary_2plus=1,prearb_pool=65,central_media=TRUE,free_agency="5_age30",qualifying_offer=FALSE)),
    modifyList(terms,list(minimum_salary_under2=1.25,minimum_salary_2plus=1.25,prearb_pool=120,central_media=TRUE,free_agency="5_age30",qualifying_offer=FALSE,super_two_pct=35)),
    modifyList(terms,list(minimum_salary_under2=1.5,minimum_salary_2plus=1.5,prearb_pool=180,central_media=TRUE,free_agency="5_age30",qualifying_offer=FALSE,super_two_pct=44))))
  if(!lock_player_terms) variants <- c(variants,list(
    modifyList(terms,list(minimum_salary_under2=1.5,minimum_salary_2plus=1.5,prearb_pool=180,central_media=TRUE,free_agency="5_age30",qualifying_offer=FALSE,arbitration="3",super_two_pct=44)),
    modifyList(terms,list(minimum_salary_under2=1.5,minimum_salary_2plus=1.5,prearb_pool=150,central_media=TRUE,free_agency="5_age30",qualifying_offer=FALSE,super_two_pct=44,min_arb_salary=3))))
  # Media pooling is a shared bargaining principle, not a concession to optimize away.
  if(!lock_player_terms) variants <- lapply(variants,function(p) modifyList(p,list(central_media=TRUE,media_share_pct=100,media_start=2027,arbitration="3",super_two_pct=min(44,p$super_two_pct))))
  variants <- unique(variants)
  ids <- expand.grid(structure=seq_len(nrow(grid)),variant=seq_along(variants))
  grid_policy <- function(i) {
    row <- grid[ids$structure[i],]
    p <- variants[[ids$variant[i]]]
    for(nm in c("upper_rule","upper_value","upper_tax_rate","lower_rule","lower_value","lower_tax_rate","revenue_sharing","required_spending")) p[[nm]] <- row[[nm]][1]
    if(p$upper_rule=="cap") p$cap_start <- 2028
    p
  }
  evaluate <- function(p,coef,response) {
    m <- simulate_policy(p,payroll_coef=coef,behavior_multiplier=response,full=FALSE,baseline=baseline)$metrics
    s <- negotiation_scores(p,m,details=FALSE)
    c(balance=m$competitive_balance_change,player=m$player_comp_change,club=m$club_effect,pool_remaining=m$prearb_remaining,
      mlb=s$mlb,mlbpa=s$mlbpa,compromise=s$compromise,viability=s$viability,joint=s$joint_support,
      cost_share=m$club_effect/sum(baseline$Revenue_Millions),worst_club_share=m$worst_club_effect_share,
      recipient_efficiency=m$recipient_efficiency,marginal_wins=m$recipient_marginal_wins)
  }
  policies_all <- Filter(function(p) !length(recommendation_checks(p)),lapply(seq_len(nrow(ids)),grid_policy))
  if(!length(policies_all)) stop("No realistic packages satisfy the selected terms. Try unlocking player terms or using proposal-inspired settings.")
  # Include the exact user-selected settings; never lose a manually discovered package.
  # Include reference presets as candidates if they meet the same guards.
  preset_candidates <- lapply(list(current_cba_policy(),mlb_proposal_policy(),mlbpa_proposal_policy()),function(p) modifyList(complete_policy(p),terms[bargaining_assumptions]))
  if(!lock_player_terms) policies_all <- c(policies_all,Filter(function(p) !length(recommendation_checks(p)),preset_candidates))
  policies_all <- c(list(terms),policies_all)
  make_policy <- function(i) policies_all[[i]]
  screened <- as.data.frame(t(vapply(seq_along(policies_all),function(i) {
    if(i%%100==0||i==length(policies_all)) progress(i,length(policies_all),"screen")
    evaluate(make_policy(i),terms$payroll_coef,behavior)
  },numeric(13))))
  # Refine promising packages with actual negotiated slider values, not changed model assumptions.
  # The selected behavior, player population, media share of revenue and model coefficients stay fixed.
  key <- function(p) paste(unlist(p),collapse="|")
  seen <- new.env(hash=TRUE,parent=emptyenv())
  for(i in seq_along(policies_all)) assign(key(policies_all[[i]]),TRUE,envir=seen)
  domains <- list(upper_value=c(180,200,220,240,250,258.2,260,280,300,320,350,400),
    upper_tax_rate=seq(0,100,10),lower_value=c(50,75,100,120,140,150,160,175,180.2,190,200,220),
    lower_tax_rate=seq(0,100,10),revenue_sharing=seq(0,50,5),required_spending=seq(0,100,10))
  if(!lock_player_terms) domains <- c(domains,list(minimum_salary_under2=c(.78,1,1.25,1.5,1.75,2),
    minimum_salary_2plus=c(.78,1,1.25,1.5,1.75,2),prearb_pool=seq(50,200,25),
    media_start=c(2027,2028),
    arbitration=c("2","3"),super_two_pct=c(22,44,66,100),min_arb_salary=c(0,1.5,3,5),
    arbitration_bonus_pct=c(0,2.5,5),veteran_bonus=c(0,.25,.5),prearb_min_bonus=c(0,.05,.1),free_agency=c("6","5_age30"),qualifying_offer=c(FALSE,TRUE)))
  financial_guard <- function(d) d$pool_remaining>=0 & d$cost_share>=-.05 & d$worst_club_share>=-.15
  # Economic outcomes dominate; mutual support rewards the weaker side continuously.
  # Allocation is an imperfect team-level proxy, not a measured labor-market welfare effect.
  score_packages <- function(d) {
    d$core_economic <- with(d,pmin(rescale01(balance),rescale01(player),rescale01(club)))
    d$allocation <- with(d,(rescale01(recipient_efficiency)+rescale01(marginal_wins))/2)
    d$economic <- .875*d$core_economic+.125*d$allocation
    d$overall <- d$compromise/100
    d
  }
  seed_ids <- function(d) {
    valid <- financial_guard(d) & vapply(policies_all,function(p) !length(recommendation_checks(p)),logical(1))
    ids <- which(valid)
    if(!length(ids)) return(integer())
    z <- score_packages(d[ids,,drop=FALSE])
    good <- which(z$balance>=0 & z$player>=0)
    unique(c(head(ids[good[order(z$compromise[good],decreasing=TRUE)]],12),
      head(ids[order(z$compromise,decreasing=TRUE)],6),
      head(ids[order(z$economic,decreasing=TRUE)],4)))
  }
  seeds <- unique(c(1L,seed_ids(screened)))
  for(pass in seq_len(3)) {
    new_ids <- integer()
    for(seed in seeds) {
      p <- policies_all[[seed]]
      for(nm in names(domains)) {
        vals <- domains[[nm]]
        if(nm=="upper_value") { if(p$upper_rule=="none") next; if(p$upper_rule=="cbt") vals<-vals[vals<=350 & vals==round(vals)] }
        if(nm=="upper_tax_rate"&&p$upper_rule!="cbt") next
        if(nm=="lower_value") { if(p$lower_rule=="none") next; if(p$lower_rule=="tax") vals<-vals[vals==round(vals)] }
        if(nm=="lower_tax_rate"&&p$lower_rule!="tax") next
        if(nm=="media_share_pct"&&!p$central_media) next
        for(v in vals) {
          q <- p; q[[nm]] <- v
          if(length(recommendation_checks(q))) next
          k <- key(q); if(exists(k,envir=seen,inherits=FALSE)) next
          assign(k,TRUE,envir=seen)
          policies_all[[length(policies_all)+1L]] <- q
          new_ids <- c(new_ids,length(policies_all))
        }
      }
    }
    if(!length(new_ids)) break
    extra <- as.data.frame(t(vapply(new_ids,function(i) evaluate(make_policy(i),terms$payroll_coef,behavior),numeric(13))))
    screened <- bind_rows(screened,extra)
    seeds <- seed_ids(screened)
    progress(nrow(screened),nrow(screened),"screen")
  }
  starting_support <- min(screened$mlb[1],screened$mlbpa[1])
  screened$id <- seq_len(nrow(screened))
  screened$realistic <- vapply(policies_all,function(p) !length(recommendation_checks(p)),logical(1))
  eligible <- screened[screened$realistic & financial_guard(screened),,drop=FALSE]
  if(!nrow(eligible)) stop("No evaluated package meets the proposal-inspired ranges and modeled club-cost limits. No recommendation is available.")
  take <- function(d,key) head(d$id[order(d[[key]],decreasing=TRUE)],shortlist_size)
  eligible <- score_packages(eligible)
  feasible_central <- subset(eligible,balance>=0&player>=0)
  shortlist <- unique(c(take(feasible_central,"compromise"),take(eligible,"economic"),take(feasible_central,"economic"),
    take(eligible,"compromise"),take(eligible,"balance"),take(eligible,"player"),take(eligible,"club")))
  assumptions <- expand.grid(coef=unique(c(max(.001,terms$payroll_coef-payroll_effect_se),
    terms$payroll_coef,terms$payroll_coef+payroll_effect_se)),
    response=unique(c(max(.25,behavior*.75),behavior,min(2,behavior*1.25))))
  central_idx <- which(abs(assumptions$coef-terms$payroll_coef)<1e-10 & abs(assumptions$response-behavior)<1e-10)[1]
  # Audit candidates in descending central compromise order until one passes all guards.
  cache <- new.env(hash=TRUE,parent=emptyenv())
  sensitivity <- function(i) {
    k <- as.character(i)
    if(exists(k,envir=cache,inherits=FALSE)) return(get(k,envir=cache))
    p <- make_policy(i)
    d <- as.data.frame(t(vapply(seq_len(nrow(assumptions)),function(j)
      evaluate(p,assumptions$coef[j],assumptions$response[j]),numeric(13))))
    d$id <- i; d$assumption <- seq_len(nrow(assumptions))
    assign(k,d,envir=cache); d
  }
  robust_ok <- function(d) all(financial_guard(d)) && median(d$balance)>=0 && median(d$player)>=0 &&
    d$balance[central_idx]>=0 && d$player[central_idx]>=0
  ordered <- feasible_central$id[order(feasible_central$compromise,decreasing=TRUE)]
  checked <- integer(); verified_max <- NA_real_
  for(i in ordered) {
    checked <- c(checked,i)
    z <- sensitivity(i)
    if(robust_ok(z)) { verified_max <- z$compromise[central_idx]; break }
  }
  shortlist <- unique(c(checked,shortlist))
  all <- bind_rows(lapply(seq_along(shortlist),function(j) {
    progress(j,length(shortlist),"robust"); sensitivity(shortlist[j])
  }))
  viable_ids <- all %>% group_by(id) %>% summarise(valid=all(pool_remaining>=0 & cost_share>=-.05 & worst_club_share>=-.15),.groups="drop") %>% filter(valid) %>% pull(id)
  if(!length(viable_ids)) stop("No shortlisted package meets the modeled club-cost limits across sensitivity cases.")
  all <- all %>% filter(id %in% viable_ids) %>% group_by(assumption) %>% group_modify(~score_packages(.x)) %>% ungroup()
  summaries <- all %>% group_by(id) %>% summarise(
    median_overall=median(overall),median_economic=median(economic),median_core_economic=median(core_economic),median_allocation=median(allocation),
    median_balance=median(balance),pool_feasible=all(pool_remaining>=0),
    finance_feasible=all(cost_share>=-.05 & worst_club_share>=-.15),
    median_player_comp=median(player),median_club_effect=median(club),
    median_mlb_support=median(mlb),median_mlbpa_support=median(mlbpa),
    median_compromise=median(compromise),median_viability=median(viability),
    median_joint_support=median(joint),
    balance_positive_share=mean(balance>=0),player_positive_share=mean(player>=0),
    central_mlb=mlb[assumption==central_idx][1],central_mlbpa=mlbpa[assumption==central_idx][1],
    central_compromise=compromise[assumption==central_idx][1],central_balance=balance[assumption==central_idx][1],central_player=player[assumption==central_idx][1],.groups="drop")
  summaries$qualified <- with(summaries,pool_feasible&finance_feasible&
    central_balance>=0&central_player>=0&median_balance>=0&median_player_comp>=0)
  summaries <- summaries[summaries$pool_feasible & summaries$finance_feasible,,drop=FALSE]
  if(!nrow(summaries)) stop("No shortlisted package meets the modeled club-cost limits across sensitivity cases.")
  feasible <- summaries[summaries$qualified,]
  qualified <- nrow(feasible)>0
  ranked <- if(qualified) feasible else summaries
  best <- ranked %>% arrange(desc(central_compromise),desc(median_compromise),desc(median_economic),desc(median_joint_support)) %>% slice(1)
  choices <- bind_rows(
    best %>% mutate(Objective=if(qualified)"Best overall compromise" else "Best overall tradeoff (economic guardrail not met)"),
    summaries %>% arrange(desc(central_compromise),desc(median_compromise)) %>% slice(1) %>% mutate(Objective="Highest support (financial screen)"),
    summaries %>% arrange(desc(median_economic)) %>% slice(1) %>% mutate(Objective="Best economic result"),
    summaries %>% arrange(desc(median_balance)) %>% slice(1) %>% mutate(Objective="Competitive balance"),
    summaries %>% arrange(desc(median_player_comp)) %>% slice(1) %>% mutate(Objective="Player compensation"))
  policies <- lapply(choices$id,make_policy)
  recommended <- policies[[1]]
  sim <- simulate_policy(recommended,payroll_coef=terms$payroll_coef,behavior_multiplier=behavior,baseline=baseline)
  scores <- negotiation_scores(recommended,sim$metrics)
  stopifnot(best$central_compromise>=max(ranked$central_compromise)-1e-10)
  if(qualified) stopifnot(is.finite(verified_max),abs(best$central_compromise-verified_max)<1e-8)
  list(best=best,policy=recommended,simulation=sim,scores=scores,qualified=qualified,
    screened=nrow(screened),tested=length(shortlist),assumptions=nrow(assumptions),ranked_packages=ranked,
    starting_support=starting_support,best_central_support=max(pmin(eligible$mlb,eligible$mlbpa)),
    realistic_screened=nrow(eligible),verified_max=verified_max,checked_for_max=length(checked),
    alternatives=choices,alternative_policies=policies,locked=lock_player_terms,
    behavior=behavior,terms=terms)
}
