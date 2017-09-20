#' Part5: Portfolio Optimization Function(with drawdown constraints)
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



opti.wets.donlp2 = function(SD_1year.max=3.5, rt_1quater.min=-3, proj_rt, cov_matrix2, stress_rt, stress_drawdown=15){
  # This function has already had nothing to do with the date.
  # This is the inner most function.
  # SD_1year.max       # positive decimal number eg. 3.5
  # rt_1quater.min     # negative decimal number eg. -3
  # proj_rt            # a horizontal 1×(n+1) tibble, n equals to number of assets, 1 row for Date
  # cov_matrix2        # n×n labeled matrix filled with small decimal numbers, labels are asset tickers

  tickers = names(proj_rt[ ,-1])

  proj_rt = proj_rt[ ,-1] %>%
    as.matrix %>%
    t()

  needs(Rdonlp2)
  # Convert SD and return to day format
  SD_1day.max = SD.year2day(percent(SD_1year.max))       # originally year
  rt_1year.min = rt.quater2year(percent(rt_1quater.min))  # originally quater

  # *[n] Number of assets (tickers)
  n = ncol(cov_matrix2)

  # [fn] Objective function to be minimized
  fn=function(par){-par%*%proj_rt}

  #________________________________________________________________________________________
  # [par] Initial value
  P = rep(1/n, n)

  # [par.upper & par.lower] -> par
  par.u = rep(0.3,n)
  par.l = rep(0,n)

  # [A] the matrix object that represents linear constraints
  A=rbind(t(c(rep(1,n))), t(proj_rt), t(stress_rt))

  # [lin.upper $ lin.lower] -> A
  lin.u=c(1, +Inf, +Inf)
  lin.l=c(1, rt_1year.min, stress_drawdown)

  # [nlin] list of functions that represents nonlinear constraints
  cov_cons = function(par){t(par)%*%cov_matrix2%*%par}
  nlin = list(cov_cons)

  # [nlin.upper & nlin.lower] -> nlin
  nlin.u=c(SD_1day.max)
  nlin.l=c(0)

  # Check criteria
  if(!TRUE){
    cat("------------------Check variables------------------\n")
    message("par"); print(P)
    message("fn"); print(fn)
    message("par.upper"); print(par.u)
    message("par.lower"); print(par.l); cat("\n")

    message("A"); print(A)
    message("lin.upper"); print(lin.u)

  }

  #_____________________________________________________________________
  # donlp2 core
  result = donlp2(par = P,
                  fn,
                  par.upper = par.u,
                  par.lower = par.l,

                  A,
                  lin.upper = lin.u,
                  lin.lower = lin.l,

                  nlin,
                  nlin.upper = nlin.u,
                  nlin.lower = nlin.l,

                  name = NULL)

  end_result = result$par
  names(end_result) = tickers
  return(end_result)
}
