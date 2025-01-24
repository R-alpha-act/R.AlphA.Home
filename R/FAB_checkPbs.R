#' @title for debug purposes - check problems for FoldAllBr function
#' @description gives a glimpse of lines which could cause problems in FoldAllBr
#'
#' @param linesBefore number of lines before the problem to display
#' @param linesAfter number of lines after the problem to display
#'
#' @return a table showing the problems, if any. Else : no return value
#' @export
#'
FAB_checkPbs <- function(linesBefore = 0, linesAfter = 10){
	test <- foldAllBr(debug_getTbl = 1)
	firstPb <- test %>% filter(checkCat < 0) %>% slice_min(rowid)
	if(nrow(firstPb)==0) {
		message("no indent problem found")
		return(NULL)
	} else {
		rowsRange <- (firstPb$rowid - linesBefore):(firstPb$rowid + linesAfter)
	} #
	test %>%
		dplyr::slice(rowsRange) %>%
		relocate(rowid, checkCat, content) %>%
		select(-opBrPN)
} # FAB_checkPbs
