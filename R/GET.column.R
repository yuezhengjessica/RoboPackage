#' Part1 : Get Dataset from MySQL to R (Database Connetion Function: connect.and.query/  	GET.value.lookup/GET.n.until/  GET.column/  GET.rt.dateRange/   GET.rt)
#'
#' These functions allow you get column from table
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




GET.column = function(table, column){
  statement = paste0("SELECT ", column, " FROM ", table)
  connect.and.query(statement, type = "get")
}

GET.rt.dateRange = function(first_day, last_day, tickers = slec.tickers, table = "daily_return"){
  # ad-hoc function
  message("Getting data range from return data on MySQL......")

  day.diff = last_day - first_day
  statement = paste0("SELECT Date,", tickers, " FROM ", table, " WHERE Date BETWEEN \'", first_day, "\' AND \'", last_day, "\'")
  a = connect.and.query(statement) %>%
    arrange(Date) %>%
    mutate_at(vars(-Date), percent)

  cat(day.diff);cat(" days");cat("\n")
  cat(nrow(a)-1); cat(" trading days")
  message("DONE")

  return(a)
}

GET.rt = function(tickers = "SHV,SHY,IEI,AGZ,STIP,TIP", table,
                  DATE, days.all = 504, delim = ","){
  message("Getting daily or forward return from MySQL......")
  if(table == "proj_return" & days.all == 504) {days.all = 1}
  if(table == "proj_return"){DATE = DATE - 1}

  a = strsplit(tickers, split = delim)[[1]] %>%
    paste0(collapse = ",")
  d = paste0("Date,", a)

  daily_rt = GET.n.until(d, table, days.all, DATE) %>%
    mutate_at(vars(-Date), percent)

  cat(paste0("got ", days.all, " day(s) data from "))
  cat(table)

  if(nrow(daily_rt) < days.all){
    warning("less data rows than expected!!!")
    cat(paste("date range of the target dataset is from", min(daily_rt$Date), "to", max(daily_rt$Date)))
  }
  message("DONE")
  return(daily_rt)
}
