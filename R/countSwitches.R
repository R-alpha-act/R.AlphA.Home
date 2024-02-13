#' get the number, from a vector of starts/stops-----------------------------
#' @description this function takes as input a vector of values in which
#' starts and stops are indicated by specific values, identifying each time a
#' new series. It returns a vector of the same length with the series count or
#' 0 when the element is outside a series (after a stop and before a start)
#' @param sttEnd vector of on/off, start/end, or alike switches
#' @param sttMark the starting value
#' @param endMark the ending value
#' @param includeStt should the starting value be counted as part of the series
#' @param includeEnd should the ending value be counted as part of the series
#' @importFrom stats runif
#' @export

countSwitches <- function(sttEnd, sttMark, endMark,
							  includeStt = TRUE, includeEnd = TRUE){
	# sttMark <- "start"
	# endMark <- "end"
	# includeStt <- TRUE
	# includeEnd <- TRUE
	grpTable <- sttEnd %>%
		as.data.table %>%
		rename(stepStr = ".") %>%
		mutate(findStt = stepStr == sttMark) %>%
		mutate(findEnd = stepStr == endMark) %>%
		mutate(nbStt = cumsum(findStt %>% replace_na(0))) %>%
		mutate(nbEnd = cumsum(findEnd %>% replace_na(0))) %>%
		# mutate(nbEnd = cumsum(stepStr == endMark) %>% replace_na(0)) %>%
		mutate(grp = ifelse(nbStt!=nbEnd, nbStt, 0)) %>%
		mutate(grpRetStt = ifelse(stepStr==sttMark & includeStt, nbStt,0)) %>%
		mutate(grpRetEnd = ifelse(stepStr==endMark & includeEnd, nbStt,0)) %>%
		# identity %>% print %>%
		identity
	grpTable[stepStr==sttMark, grp:= grpRetStt]
	grpTable[stepStr==endMark, grp:= grpRetEnd]
	# print(grpTable)

	return(grpTable$grp)
}

	# sttEnd <- c(
	# 	""
	# 	, "start"
	# 	, ""
	# 	, ""
	# 	, ""
	# 	, ""
	# 	, "end"
	# 	, ""
	# 	, ""
	# 	, "start"
	# 	, "end"
	# 	, ""
	# 	, ""
	# 	, "start"
	# 	, "end"
	# 	, ""
	# 	, ""
	# )
	# countSwitches(sttEnd, "start", "end")
	# sttMark <- "start"
	# endMark <- "end"
	# rm(includeEnd, includeStt, sttMark, endMark)
