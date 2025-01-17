#' @title Track Time Steps Inside a Data.table
#' @description The `timer` function allows you to append timeStamps to a data.table,
#' and include additional metadata provided as arguments.
#' Then calculate time differences between timeStamps.
#' @param timer_table A data.table containing the timer log to continue from.
#' Defaults to an empty `data.table().
#' @param end A logical, inidicating the end of the timer, defaulted to FALSE.
#' 'timer()' calls must be placed at the beginning of each part :
#' therefore, this step is necessary.
#' the time differences between timeStamps are calculated only at the end.
#' @param ... Additional specifications. Use named arguments to define columns
#' and values for rows.
#' @return A `data.table` containing the original data, plus one new timeStamp,
#' and optionally computed time differences, the printed table includes:
#'   \itemize{
#'     \item `timeStamp`: The current timeStamp (`POSIXct`).
#'     \item `datetime`: The numeric representation of the timeStamp.
#'     \item `time_diff`: The time difference in seconds between consecutive rows as a `difftime` object.
#'     \item `time_diff_secs`: The formatted time difference in seconds with milliseconds as a character string.
#'     \item Additional columns for any metadata provided via `...`.
#'   }
#' @examples
#' library(data.table)
#' # Initialize an empty data.table
#' timer_table <- data.table()
#'
#' # Add the first timeStamp
#' timer_table <- timer(timer_table, description = "Start")
#'
#' # Add another timeStamp and compute time differences
#' timer_table <- timer(timer_table, end = TRUE, description = "Event 2")
#'
#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
timer <- function(timer_table = data.table(), end = FALSE, ...) {

	{
		# R.AlphA_manualRun_start
		manualrun <- T
		manualrun <- F
		if (manualrun) {
			warning("! parameters manually defined inside function 'timer' for tests. Do not use results !")
			timer_table <- data.table()
			time_inter <- data.table(timeStamp = Sys.time())
			timer_table <- rbind(timer_table, time_inter, fill = TRUE)
			time_inter <- data.table(timeStamp = Sys.time())
			timer_table <- rbind(timer_table, time_inter, fill = TRUE)
			time_inter <- data.table(timeStamp = Sys.time())
			timer_table <- rbind(timer_table, time_inter, fill = TRUE)


			end <- T
		} # manualrun - for debug purposes
	} # R.AlphA_manualRun
	timeFormatted <- function(rawTime) {
		if (is.na(rawTime)) return(NA) # Handle NA values
		sprintf('%0d.%03d', trunc(rawTime), trunc((rawTime %% 1) * 1000))
	} # a simple function to format times in readable text
	extra_args <- list(...)
	if (length(extra_args) > 0) {
		for (name in names(extra_args)) {
			time_inter[, (name) := extra_args[[name]]]
		} # for each additional arg, add it to the table
	} # if additional args provided, add corresponding columns
	{
		timer_table <- rbind(
			fill = TRUE
			, timer_table
			, data.table(timeStamp = Sys.time())
		)

	} # Add a new line of timeStamp to the timer table
	if (end == TRUE && nrow(timer_table) > 1) {
		timer_table[, timeStamp_num := as.numeric(timeStamp)]
		timer_table[, ":=" (
			time_diff = c(NA, diff(timeStamp_num))
			, time_diff_secs = c(NA, sapply(diff(timeStamp_num), timeFormatted))
		)]
	} # Compute time differences if `end = TRUE`
	return(timer_table)
}
