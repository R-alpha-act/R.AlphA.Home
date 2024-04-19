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

countSwitches <- function(data, colNm, sttMark, endMark,
							  includeStt = TRUE, includeEnd = TRUE){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! parameters manually defined inside function 'countSwitches' for tests. Do not use results !")
		sttMark <- "+"
		endMark <- "-"
		includeStt <- TRUE
		includeEnd <- TRUE
		# sttEnd <- docContentRet$brTag
		data <- docContentRet
		colNm <- "brTag"
	}

	fill_in <- function(prev, new, check) {
		if_else(check == -1, 0, prev + check)
	}

	# dataX <- data
	# dataX <- dataX %>% bind_rows(dataX, dataX, dataX, dataX, dataX) %>% print
	# data <- dataX %>% slice(1:1E4)
	# data %>%
	# 	slice(1:10) %>%
	# 	mutate(test = get(colNm))

	grpTable <- data %>%
		# mutate(stepStr = colValues) %>%
		mutate(stepStr = get(colNm)) %>%
		as_tibble %>%
		# as.data.table %>%
		# rename(stepStr = ".") %>%
		# mutate(stepStr = brTag) %>%
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
		# # initialize brut
		# mutate(brut1 = inc1) %>%
		# mutate(brut2 = inc2) %>%
		# mutate(brut3 = inc3) %>%
		# # slowest part : accumulate
		# mutate(brut1 = accumulate2(brut1, check1 %>% tail(-1), fill_in)) %>%
		# mutate(brut2 = accumulate2(brut2, check2 %>% tail(-1), fill_in)) %>%
		# mutate(brut3 = accumulate2(brut3, check3 %>% tail(-1), fill_in)) %>%
		# mutate(ret1 = ifelse(catLvl >= 1, brut1, 0)) %>%
		# mutate(ret2 = ifelse(catLvl >= 2, brut2, 0)) %>%
		# mutate(ret3 = ifelse(catLvl >= 3, brut3, 0)) %>%
		#

		mutate(tst1 = cumsum(inc1)) %>%
		group_by(tst1) %>% mutate(tst2 = cumsum(inc2)) %>%
		group_by(tst1, tst2) %>% mutate(tst3 = cumsum(inc3)) %>%
		mutate(ret1 = ifelse(catLvl >= 1, tst1, 0)) %>%
		mutate(ret2 = ifelse(catLvl >= 2, tst2, 0)) %>%
		mutate(ret3 = ifelse(catLvl >= 3, tst3, 0)) %>%
		# select(-content) %>%
		identity


	# grpTable %>%
	# 	filter(rTs1 != ret1|rTs2 != ret2|rTs3 != ret3)
	# 	filter(tst1 != ret1)

		# mutate(inc3 = findStt & catLvl == 3) %>%
		# mutate(inc4 = findStt & catLvl == 4) %>%
		# mutate(nbEnd = cumsum(stepStr == endMark) %>% replace_na(0)) %>%
		# mutate(grp = ifelse(nbStt!=nbEnd, nbStt, 0)) %>%
		# mutate(grpRetStt = ifelse(stepStr==sttMark & includeStt, nbStt,0)) %>%
		# mutate(grpRetEnd = ifelse(stepStr==endMark & includeEnd, nbStt,0)) %>%
		# as_tibble %>% print(n = 130)
		# identity
	# grpTable[stepStr==sttMark, grp:= grpRetStt]
	# grpTable[stepStr==endMark, grp:= grpRetEnd]
	# print(grpTable)

	return(grpTable)
}
data.table(x = c(rep(1,1E7), rep(2, 3))) %>%
	mutate(across(x, as.numeric)) %>%
	group_by(x) %>%
	mutate(tst = cumsum(x))
	# mutate(tst = ifelse(x == 10, 0, cumsum(x)))
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
