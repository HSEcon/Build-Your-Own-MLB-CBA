# Trade accounting is separate from provider payroll snapshots.
# Do not assume a reported salary is a CBT charge or that unknown values are zero.
trade_cash_transfer <- function(salary, retained) {
  stopifnot(length(salary) == length(retained),
    all(is.na(salary) | (is.finite(salary) & salary >= 0)),
    all(is.na(retained) | (is.finite(retained) & retained >= 0)),
    all(is.na(salary) | is.na(retained) | retained <= salary))
  salary - retained
}

read_trade_ledger <- function(path = "data/trade_history.csv",
                              teams_path = "data/mlb_2027_payroll_base.csv") {
  d <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                na.strings = "", check.names = FALSE, fileEncoding = "UTF-8")
  required <- c("Trade_ID", "Movement_ID", "Payroll_Season", "Transaction_Date",
    "Player", "From_Team_ID", "To_Team_ID", "Status", "Salary_Before_Trade_Millions",
    "Salary_Retained_By_From_Club_Millions", "From_Club_CBT_Change_Millions",
    "To_Club_CBT_Change_Millions", "Snapshot_Reconciliation", "Deal_Notes", "Source", "Source_URL")
  stopifnot(all(required %in% names(d)), !anyDuplicated(names(d)))
  d <- d[, required]
  essential <- c(required[1:8], "Snapshot_Reconciliation", "Source", "Source_URL")
  stopifnot(!anyNA(d[, essential]), all(nzchar(trimws(as.matrix(d[, essential])))),
    !anyDuplicated(d$Movement_ID), !anyDuplicated(paste(d$Trade_ID, d$Player)),
    all(grepl("^https://", d$Source_URL)),
    all(d$Status %in% c("Official trade", "Reported trade", "Withdrawn")),
    all(d$Snapshot_Reconciliation %in% c("Unreconciled", "Included", "Not included")))
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)$Team_ID
  stopifnot(all(d$From_Team_ID %in% teams), all(d$To_Team_ID %in% teams),
    all(d$From_Team_ID != d$To_Team_ID))
  dates <- as.Date(d$Transaction_Date, format = "%Y-%m-%d")
  stopifnot(!anyNA(dates), all(format(dates, "%Y-%m-%d") == d$Transaction_Date),
    all(dates <= Sys.Date()))
  stopifnot(all(grepl("^[0-9]{4}$", d$Payroll_Season)),
    all(as.integer(d$Payroll_Season) %in% 1900:2100))
  for (col in required[9:12]) {
    raw <- d[[col]]
    d[[col]] <- suppressWarnings(as.numeric(raw))
    stopifnot(all(is.na(raw) | is.finite(d[[col]])))
  }
  amount <- trade_cash_transfer(d$Salary_Before_Trade_Millions,
                                 d$Salary_Retained_By_From_Club_Millions)
  d$From_Club_Salary_Change_Millions <- -amount
  d$To_Club_Salary_Change_Millions <- amount
  d$From_Club_Salary_Change_Millions[d$Status != "Official trade"] <- NA_real_
  d$To_Club_Salary_Change_Millions[d$Status != "Official trade"] <- NA_real_
  stopifnot(all(is.na(d$From_Club_Salary_Change_Millions) |
    abs(d$From_Club_Salary_Change_Millions + d$To_Club_Salary_Change_Millions) < 1e-8))
  d[order(d$Transaction_Date, d$Trade_ID, d$Movement_ID, decreasing = TRUE), ]
}
