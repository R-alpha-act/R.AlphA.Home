#' @title Count Series Based on Start/Stop Markers
#'
#' @description This function identifies and counts series in a vector based on
#' specified start and stop markers. It returns a vector of the same length,
#' indicating the series count or 0 when the element is outside a series
#' (e.g., after a stop marker and before the next start marker).
#'
#' @param data A data frame containing the column to process.
#' @param colNm A string specifying the column name in `data` to evaluate.
#' @param sttMark A value indicating the start of a series.
#' @param endMark A value indicating the end of a series.
#' @param includeStt Logical. Should the start marker be included as part of
#' the series? Default is `TRUE`.
#' @param includeEnd Logical. Should the end marker be included as part of the
#' series? Default is `TRUE`.
#'
#' @return A modified version of the input data frame with additional columns including:
#' \itemize{
#'   \item `stepStr`: The column specified by `colNm`.
#'   \item `findStt`: Logical flag indicating the occurrence of the start marker.
#'   \item `findEnd`: Logical flag indicating the occurrence of the end marker.
#'   \item `catLvl`: The current series level calculated as the difference between the cumulative counts of start and end markers.
#'   \item `inc1`, `inc2`, `inc3`: Logical flags for detecting nested series at different levels.
#'   \item `check1`, `check2`, `check3`: Intermediate variables used for series counting.
#'   \item `tst1`, `tst2`, `tst3`: Cumulative series counts at different nesting levels.
#'   \item `ret1`, `ret2`, `ret3`: Final series counts returned for each respective level.
#' }
#'
#' @importFrom stats runif
#' @export
#'
countSwitches <- function(data, colNm, sttMark, endMark,
							  includeStt = TRUE, includeEnd = TRUE){
	fill_in <- function(prev, new, check) {
		if_else(check == -1, 0, prev + check)
	}

	grpTable <- data %>%
		mutate(stepStr = get(colNm)) %>%
		as_tibble %>%
		mutate(findStt = stepStr == sttMark) %>%
		mutate(findEnd = stepStr == endMark) %>%
		mutate(nbStt = cumsum(findStt %>% replace_na(0))) %>%
		mutate(nbEnd = cumsum(findEnd %>% replace_na(0))) %>%
		mutate(catLvl = nbStt - nbEnd) %>%
		select(-nbStt, -nbEnd) %>%
		# spot starts
		mutate(inc1 = findStt & catLvl == 1) %>%
		mutate(inc2 = findStt & catLvl == 2) %>%
		mutate(inc3 = findStt & catLvl == 3) %>%
		# spot when under category to reset
		mutate(check1 = ifelse(catLvl >= (1-1), inc1, -1)) %>%
		mutate(check2 = ifelse(catLvl >= (2-1), inc2, -1)) %>%
		mutate(check3 = ifelse(catLvl >= (3-1), inc3, -1)) %>%
		select(-opBr, -clBr, anyBr) %>%
		mutate(tst1 = cumsum(inc1)) %>%
		group_by(tst1) %>% mutate(tst2 = cumsum(inc2)) %>%
		group_by(tst1, tst2) %>% mutate(tst3 = cumsum(inc3)) %>%
		mutate(ret1 = ifelse(catLvl >= 1, tst1, 0)) %>%
		mutate(ret2 = ifelse(catLvl >= 2, tst2, 0)) %>%
		mutate(ret3 = ifelse(catLvl >= 3, tst3, 0)) %>%
		identity()

	return(grpTable)
}
