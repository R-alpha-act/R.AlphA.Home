#' @title Times steps - v3 with lubridate functions, and only stores time, not durations
#' @description initialisation of a tmr_table
#' @param tmTbl an existing time table
#' @param message print the timer
#' @param ... any other specification. Choose name for column and value for row
#' @return the tmr table with 1 line added at the end
#' @examples
#' new_tmr <- tmr_init(step = 0)
#' new_tmr <- tmr(new_tmr, step = 1)

#' @importFrom lubridate now
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export

# tmr ---------------------------------------------------------------------
tmr <- function(tmTbl, message = F, ...){
	time_interm <- data.table(tmrDateTime = now(), ...)
	if(message) print(time_interm)
	rbind(tmTbl, time_interm, fill = TRUE)
}
