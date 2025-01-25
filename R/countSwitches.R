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
#' @return A modified version of the input data frame with additional
#' columns for series identification and counts.
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
