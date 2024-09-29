#' @title Times steps - v3 with lubridate functions, and only stores time, not durations
#' @description computing columns in a tmr table
#' @param tmTbl an existing time table
#' @param ends do timers correspond to ends of steps
#' @return the tmr table sorted and with computed columns
#' @examples
#' new_tmr <- tmr_init(step = 0)
#' new_tmr <- tmr(new_tmr, step = 1)
#' full_tmr <- tmr_compute(new_tmr)

#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export

# compute -----------------------------------------------------------------
tmr_compute <- function(tmTbl, ends = TRUE){
	# tmTbl <- tmr_init() %>% tmr %>% tmr
	# ends <- TRUE

	tmTbl %>%
		arrange(tmrDateTime) %>%
		mutate(
			tmrPrevTime = lag(tmrDateTime)
			, tmrNextTime = lead(tmrDateTime)
			, tmrDtToPrev = tmrDateTime - tmrPrevTime
			, tmrDtToNext = tmrNextTime - tmrDateTime
			, tmrDt = if(ends) tmrDtToPrev else tmrDtToNext
		) %>%
		select(
			- tmrPrevTime, -tmrDtToNext, -tmrDtToPrev, -tmrNextTime
		)
}
