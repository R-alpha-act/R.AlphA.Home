#' @title Track Time Steps with Lubridate Functions
#' @description Tracks the time spent on different steps of your code,
#' storing only timestamps without durations.
#' @param start Logical. If `TRUE`, initializes a new time tracking table.
#' Default is `FALSE`.
#' @param timer_table A data.table containing the timer log to continue from.
#' Ignored if `start = TRUE`.
#' @param message Logical. If `TRUE`, prints the updated timer table after
#' adding the new entry. Default is `FALSE`.
#' @param ... Additional specifications. Use named arguments to define columns
#' and values for rows.
#' @return A data.table with the existing `timer_table` plus one new
#' entry containing the timestamp.
#' @examples
#' # Initialize a new timer table
#' tt_tests <- timer(start = TRUE)
#'
#' # Add a new row with the current timestamp
#' tt_tests <- timer(timer_table = tt_tests)
#'
#' # Add a new row with custom specifications
#' tt_tests <- timer(timer_table = tt_tests, stepName = "step x", anyVariableName = "any value")
#' @import lubridate
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
# readable period ---------------------------------------------------------
readable_period <- function(periodObj){
	if_else(
		is.na(periodObj)
		, "NA"
		, sprintf(
			fmt = paste0(
				NULL
				, ifelse(periodObj@day, '%d days ', "%.d")
				, ifelse(periodObj@hour, '%dh:', "%.d")
				, ifelse(periodObj@minute, '%02dm:', "%.d")
				# , '%02dm:'
				, '%02ds'
				, ifelse(periodObj@day, "", '.%03dms')
			)
			, periodObj@day
			, periodObj@hour
			, periodObj@minute
			, lubridate::second(periodObj) %>% trunc
			, (periodObj@.Data%%1 * 1000) %>% trunc
		)
	)
}

# testPeriod <- c(testPeriod, NA)
# readable_period(testPeriod)
