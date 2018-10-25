#' Decompostion into exchange rate and non exchange rate effect
#'
#' This function allows you to do strip out exchange effects (e.g. investment portfolio, revenue)
#' Idea is to first reverse all the SGD denominated value over time into foreign currency value. Then apply a constant exchange rate at beginning of the period over time
#' @param sp_exch_rate_pair exchange rate pair. e.g "USDSGD=X". "<Foreign_currency><local_currency>=X"
#' @param ap_start_date starting date of portfolio e.g. 1950-01-01
#' @param ap_end_date ending date of portfolio e.g. 2020-10-01. If you include a date beyond current date, the function will use the current date instead
#' @param np_mthly_yearly #Decomposition at monthly or yearly level
#' @param dp_dates_investment_value #data frame of date and investment values
#' @keywords exchangeRate
#' @export
#' @examples

exchange_rate_decomposition <- function(sp_exch_rate_pair, ap_start_date, ap_end_date, np_mthly_yearly, dp_dates_investment_value){

  thisEnv <- environment()

  #Initializing parameters here
  sp_exch_rate_pair = sp_exch_rate_pair

  ap_start_date = ap_start_date
  ap_end_date = ap_end_date

  # ap_freq = ap_freq
  np_mthly_yearly = np_mthly_yearly
  dp_dates_investment_value = dp_dates_investment_value

  #Initialized transformed array
  ap_exch_rate = NULL
  d_exchRate_portfolio = NULL


  ## Create the list used to represent an
  ## object for this class
  methd <- list(

    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,

    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },

    get_sp_exch_rate_pair = function()
    {
      return(get("sp_exch_rate_pair",thisEnv))
    },

    get_ap_start_date = function()
    {
      return(get("ap_start_date",thisEnv))
    },

    get_ap_end_date = function()
    {
      return(get("ap_start_date",thisEnv))
    },

    get_np_mthly_yearly = function()
    {
      return(get("np_mthly_yearly",thisEnv))
    },

    get_dp_dates_investment_value = function()
    {
      return(get("dp_dates_investment_value",thisEnv))
    },

    set_sp_exch_rate_pair = function(value)
    {
      return(assign("sp_exch_rate_pair",value,thisEnv))
    },

    set_ap_start_date = function(value)
    {
      return(assign("ap_start_date",value,thisEnv))
    },

    set_ap_end_date = function(value)
    {
      return(assign("ap_end_date",value,thisEnv))
    },

    # set_ap_freq = function(value)
    # {
    #   return(assign("ap_freq",value,thisEnv))
    # },

    set_np_mthly_yearly = function(value)
    {
      return(assign("np_mthly_yearly", value, thisEnv))
    },

    set_dp_dates_investment_value = function(value)
    {
      return(assign("dp_dates_investment_value",value,thisEnv))
    },

    #Get exchange rate with date range. Starting date and ending date
    get_sp_exch_rate  = function(){

      quantmod::getSymbols(sp_exch_rate_pair, from = ap_start_date, to = ap_end_date)
      sp_exch_rate_pair = gsub("\\^", "", sp_exch_rate_pair)
      ap_exch_rate = xts::as.xts(get(sp_exch_rate_pair))

      return(assign("ap_exch_rate", ap_exch_rate, thisEnv))
    },

    #Transform exchange rate into monthly or yearly frequency
    transform_exchange_rate = function(){

      methd$get_sp_exch_rate()

      if(np_mthly_yearly == "monthly"){
        ap_exch_rate = xts::to.monthly(ap_exch_rate)
      }else if(np_mthly_yearly == "yearly"){
        ap_exch_rate = xts::to.yearly(ap_exch_rate)
      }

      ap_exch_rate = ap_exch_rate[, 6]
      names(ap_exch_rate)[1] = "Adj_Close"

      return(assign("ap_exch_rate", ap_exch_rate, thisEnv))
    },

    # https://stackoverflow.com/questions/29046311/how-to-convert-data-frame-into-time-series-in-r
    transform_portfolio = function(){
      if(np_mthly_yearly == "monthly"){
        dp_dates_investment_value = xts::xts(dp_dates_investment_value[,-1], order.by = as.Date(dp_dates_investment_value[,1], "%Y-%m-%d"))
        dp_dates_investment_value = xts::to.monthly(dp_dates_investment_value)

      }else if(np_mthly_yearly == "yearly"){
        dp_dates_investment_value = xts::xts(dp_dates_investment_value[,-1], order.by = as.Date(dp_dates_investment_value[,1], "%Y-%m-%d"))
        dp_dates_investment_value = xts::to.yearly(dp_dates_investment_value)
      }

      dp_dates_investment_value = dp_dates_investment_value[, 4]
      names(dp_dates_investment_value) = "value"
      return(assign("dp_dates_investment_value", dp_dates_investment_value, thisEnv))
    },

    #Merge exchange rate info into the investment dataframe. Either using month_year or year
    integrate_exchRate_portfolio = function(){
      methd$transform_exchange_rate()
      methd$transform_portfolio()
      #Merging portfolio with exchange rate
      d_exchRate_portfolio = merge(dp_dates_investment_value, ap_exch_rate, join='left')
      return(assign("d_exchRate_portfolio", d_exchRate_portfolio, thisEnv))
    },

    #Convert data into foreign currency
    convert_to_fgn_currency = function(){
     methd$integrate_exchRate_portfolio()
     d_exchRate_portfolio$fgn_value = d_exchRate_portfolio$value / d_exchRate_portfolio$Adj_Close
     return(assign("d_exchRate_portfolio", d_exchRate_portfolio, thisEnv))
    },

    #Fix portfolio at a exchange rate
    fix_exch_rate = function(portfolio, year, month = NULL){
      methd$convert_to_fgn_currency()
      d_exchRate_portfolio$local_static_value = d_exchRate_portfolio$fgn_value * as.numeric(d_exchRate_portfolio$Adj_Close[1])
      d_exchRate_portfolio$exch_rate_impact = d_exchRate_portfolio$value - d_exchRate_portfolio$local_static_value
      return(assign("d_exchRate_portfolio", d_exchRate_portfolio, thisEnv))
    },

    get_portfolio = function(){
      methd$fix_exch_rate()
      return(get("d_exchRate_portfolio", d_exchRate_portfolio, thisEnv))
    },

    get_diff_portfolio_value = function(){

      if(is.null(d_exchRate_portfolio)){
        methd$get_portfolio()
      }

       return(as.numeric(d_exchRate_portfolio$local_static_value[nrow(d_exchRate_portfolio)]) -
               as.numeric(d_exchRate_portfolio$value[nrow(d_exchRate_portfolio)]))
    }


  )

  ## Define the value of the list within the current environment.
  assign('this', exchange_rate_decomposition, envir = thisEnv)

  ## Set the name for the class
  class(methd) <- append(class(methd),"exchange_rate_decomposition")
  return(methd)
}


# ###########################Demonstration of OOP version for exchange rate effects#######################
# sp_exch_rate_pair = "USDSGD=X"
# ap_start_date <- as.Date("1950-01-01")
# ap_end_date <- as.Date("2020-10-01")
# np_mthly_yearly = "monthly"  #alternatively this could be yearly
# data(tsla)
# dp_dates_investment_value = tsla
#
# o_exchRate_effect <- exchange_rate_decomposition(sp_exch_rate_pair, ap_start_date, ap_end_date, np_mthly_yearly, dp_dates_investment_value)
# o_exchRate_effect$get_portfolio()
# o_exchRate_effect$get_diff_portfolio_value()
