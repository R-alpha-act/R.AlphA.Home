#' @title Times steps - v3 with lubridate functions, and only stores time, not durations
#' @description initialisation of a tmr_table
#' @param ... any other specification. Choose name for column and value for row
#' @return a new tmr table with 1 line of date time and the ... cols
#' @examples
#' new_tmr <- tmr_init(step = 0)

#' @importFrom lubridate now
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export

# init --------------------------------------------------------------------
tmr_init <- function(...){
	data.table(tmrDateTime = now(), ...)
}

