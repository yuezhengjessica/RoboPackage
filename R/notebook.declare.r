#' Part6: Rebalancing Functions
#'
#' These functions allow you --
#' @param tests ------------
#' @keywords tests
#' @export
#' @examples
#' connect.and.query = function(query, type = "get")
#' GET.value.lookup = function(table = "scaling", col2lookup = "Scaling_Ratio", col.name = "Date", col.value)
#' GET.n.until = function(column, table, n, DATE)
#' GET.column = function(table, column)
#' GET.rt.dateRange = function(first_day, last_day, tickers = slec.tickers, table = "daily_return")
#' GET.rt = function(tickers = "SHV,SHY,IEI,AGZ,STIP,TIP", table, DATE, days.all = 504, delim = ",")
#' excludeSheet
#' applyPreference = function(features, excludeSheet, riskTolerance="Low", investmentHorizon="Short")
#' check.and.print = function(x)
#' check.dateRange = function(dataset, column = "Date")
#' check.valid.tickers = function(tickers, first_day, last_day, lookback.days)
#' as.Date.n = function(x)
#' SD.year2day = function(SD_year, is.percent = FALSE)
#' SD.day2year = function(SD_day)
#' rt.quater2year = function(rt_quater)
#' rt.year2quater = function(rt_year)
#' percent = function(x){x / 100}
#' YTM2adjust_rt = function(YTM)
#' shorten.rts = function(dataset, exclude = "Date")
#' adjusted_rt2proj_rt = function(adjusted_rt, daily_rt, DATE, days.lookBackMmt=105, days.excludeMmt=21)
#' cov.period = function(DATE, days.lookBackSD = 504, daily_rt)
#' cov.matrix2 = function(cov_period, SR)
#' opti.wets.donlp2 = function(SD_1year.max=3.5, rt_1quater.min=-3, proj_rt, cov_matrix2, stress_rt, stress_drawdown=15)
#' opti.wets = function(daily_rt, forward_rt, DATE, days.lookBackSD=504, days.lookBackMmt=105, days.excludeMmt=21, SD_1year.max=3.5, rt_1quater.min=-3,forward_rt_type = "adjusted",stress_rt, stress_drawdown)
#' portfolio.SD = function(weights, cov_matrix2)
#' portfolio.rt = function(weights, rt_)
#' global.bounds = function(SD, limit)
#' notebook.declare = function(daily_rt)
#' compound = function(notebook, thisDay, cov_period.thisDay, returns.thisDay)
#' check.if.rebalance = function(SD_thisDay, lowerBound, upperBound)




notebook.declare = function(daily_rt){
  # declaration of a chronological notebook based on date from daily return
  notebook.init = daily_rt %>%
    select(Date) %>%
    arrange(Date) %>%
    tbl_df() %>%
    mutate(Date = as.Date(Date))

  rows = nrow(daily_rt)

  note = notebook.init %>%
    mutate(weights = rep(list(NULL), rows),
           cov_matrix2 = rep(list(NULL), rows),
           #daily_rt = rep(list(NULL), rows),
           actual_rt = NA,
           cum_rt = NA,
           SD = NA,
           rt = NA,
           status = NA)

  return(note)
}
