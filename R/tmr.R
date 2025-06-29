#' @title Timer with Static List
#' @description A timer function that uses an internal static list to track
#' timestamps. This behavior allows very fast execution for intermediate steps
#' (a few microseconds), limiting interference in time measurements.
#' @param start Logical. If `TRUE`, reinitializes a new timer table. Default is `FALSE`.
#' @param end Logical. If `TRUE`, returns the timer table with calculated time differences. Default is `FALSE`.
#' @param fill Logical. If `TRUE`, fills missing values when combining rows. Default is `TRUE`.
#' @param ... Additional specifications. Use named arguments to define columns
#' and values for documentation of the current step.
#' @return A data.table with timestamps and optionally calculated time differences.
#' The table contains:
#'   \itemize{
#'     \item `current_time`: Sys.time() (`POSIXct`).
#'     \item `current_time_num`: current_time converted to numeric (added when end=TRUE).
#'     \item `elapsed_time_num`: The time difference in seconds between consecutive rows (added when end=TRUE).
#'     \item `elapsed_time`: The formatted time difference with milliseconds (added when end=TRUE).
#'     \item Additional columns for any information provided via `...`.
#'   }
#' @examples
#' tmr(start = TRUE)      # initialization
#'
#' tmr("step 1")          # step 1 starting
#' Sys.sleep(0.3)         # ...some code for step 1...
#'
#' tmr("step 2")          # step 2 starting
#' Sys.sleep(0.5)         # ...some code for step 2...
#'
#' print(tmr(end = TRUE)) # End - print results
#'
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export

tmr <- local({
	timer_list <- list()

	function(..., start = FALSE, end=FALSE, fill = TRUE) {
		if(start) timer_list <<- list() # first call : initialize
		time_inter <- list(current_time = Sys.time()) # new timeStamp
		extra_args <- list(...)
		if (length(extra_args) > 0) {
			noName <- is.null(names(extra_args)) || names(extra_args)[1] == ""
			if (noName) names(extra_args)[1] <- "step"
			time_inter <- c(time_inter, extra_args)
		} # include additional arguments, default first one to "step"
		timer_list <<- append(timer_list, list(time_inter))
		if (end) {
			timer_table <- rbindlist(timer_list, fill = fill)
			timer_table[, current_time_num := as.numeric(current_time)]
			timer_table[, elapsed_time_num := c(diff(current_time_num), 0)]
			timer_table[, elapsed_time := sprintf(
				'%0d.%03d'
				, trunc(elapsed_time_num)
				, trunc((elapsed_time_num %% 1) * 1000)
			)]
			timer_table[, total_time := cumsum(elapsed_time_num)]
			timer_table[nrow(timer_table), end := TRUE]
			return(invisible(timer_table))
		} # last call : compute time diffs
		return(invisible(timer_list)) # return table for display purposes
	}
})
