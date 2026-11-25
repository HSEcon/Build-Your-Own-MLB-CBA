# Reproduce Models 1 and 2 from the original research datasets and chronological holdouts.
research_validation <- function(payroll,revenue) {
  p <- payroll %>% filter(!Shortened_Season_2020,!is.na(Prior_Win_Pct),is.finite(Log_Payroll_Index))
  r <- revenue %>% filter(!Year %in% c(2020,2021),is.finite(Adjusted_Revenue),is.finite(Prior_Revenue_Millions))
  fits <- list("Model 1: Payroll and winning"=lm(Win_Pct~Log_Payroll_Index+Prior_Win_Pct+factor(Team_ID)+factor(Year),data=p),
    "Model 2: Winning and revenue"=lm(Adjusted_Revenue~Win_Pct+Prior_Revenue_Millions+factor(Team_ID)+factor(Year),data=r))
  details <- bind_rows(lapply(names(fits),function(nm) {
    s <- summary(fits[[nm]]); c <- coef(s)
    keep <- !grepl("^factor",rownames(c))
    data.frame(Model=nm,Term=rownames(c)[keep],Estimate=c[keep,1],SE=c[keep,2],
      T=c[keep,3],P=c[keep,4],R_squared=s$r.squared,Adjusted_R_squared=s$adj.r.squared,Observations=nobs(fits[[nm]]),row.names=NULL)
  }))
  rmse <- function(a,b) sqrt(mean((a-b)^2))
  pt <- p %>% filter(Year>=2019); pr <- p %>% filter(Year<=2018)
  pfit <- lm(Win_Pct~Log_Payroll_Index+Prior_Win_Pct+factor(Team_ID),data=pr)
  rt <- r %>% filter(Year>=2023); rr <- r %>% filter(Year<=2022)
  rfit <- lm(Adjusted_Revenue~Win_Pct+Prior_Revenue_Millions+factor(Team_ID),data=rr)
  rp <- lm(Adjusted_Revenue~Prior_Revenue_Millions,data=rr)
  errors <- data.frame(Model=c(rep("Model 1",3),rep("Model 2",3)),
    Method=c("Always .500","Prior-year wins","Payroll regression","League-average revenue","Prior revenue regression","Winning and revenue regression"),
    Holdout=c(rep("2019–2025 (2020 excluded)",3),rep("2023–2025",3)),
    RMSE=c(rmse(pt$Win_Pct,.5),rmse(pt$Win_Pct,pt$Prior_Win_Pct),rmse(pt$Win_Pct,predict(pfit,pt)),
      rmse(rt$Adjusted_Revenue,0),rmse(rt$Adjusted_Revenue,predict(rp,rt)),rmse(rt$Adjusted_Revenue,predict(rfit,rt))))
  list(coefficients=details,validation=errors)
}
