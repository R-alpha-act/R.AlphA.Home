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
#'     \item `timeStamp_num`: timeStamp converted to numeric, useful for intermediary calculations.
#'     \item `dt_num`: The time difference in seconds between consecutive rows as a numeric value.
#'     \item `dt_text`: The formatted time difference in seconds with milliseconds as a character string.
#'     \item Additional columns for any information provided by the user via `...`. It allows documentation about the current step running, substeps, which version is being tested, ...
#'   }
#' @examples
#' library(data.table)
#' # Initialize an empty data.table
#' tmr <- data.table()
#'
#' # Add the first timeStamp
#' tmr <- timer(tmr, description = "calculation")
#'
#' # small calculation
#' for (i in 1:1000) {sqrt(2)}
#'
#' # Add another timeStamp and compute time differences
#' tmr <- timer(tmr, end = TRUE, description = "End")
#' print(tmr)
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
timer <- function(timer_table = data.table(), end = FALSE, ...) {
	formatTime <- function(time_numeric) {
		ifelse(
			is.na(time_numeric)
			, NA
			, sprintf(
				'%0d.%03d'
				, trunc(time_numeric)
				, trunc((time_numeric %% 1) * 1000)
			)
		)
	} # a simple function to format times in readable text
	time_inter <- data.table(timeStamp = Sys.time())
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
			, time_inter
		)
	} # Add a new line of timeStamp to the timer table
	if (end == TRUE && nrow(timer_table) > 1) {
		timer_table[ , timeStamp_num   := as.numeric(timeStamp)]
		timer_table[ , dt_num          := c(diff(timeStamp_num), 0)]
		timer_table[ , dt_text         := formatTime(dt_num)]
	} # Compute time differences if `end = TRUE`
	return(timer_table)
}
