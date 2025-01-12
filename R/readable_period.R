#' @title Convert Period Object to Human-Readable Text
#' @description Converts a `lubridate` period object into a human-readable
#' string representation.
#' @param periodObj A `period` object from the `lubridate` package to format.
#' @return A character string representing the period in a human-readable format.
#' @importFrom lubridate second
#' @export
#'
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
