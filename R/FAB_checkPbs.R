# rm(list = ls())
# fold brackets --------
#' @title FAB_checkPbs
#' @description gives a glimpse of lines which could cause pbs in FAB
#' @param linesBefore nb of lines before the pb to display
#' @param linesAfter nb of lines after the pb to display
#' @export
FAB_checkPbs <- function(linesBefore = 0, linesAfter = 10){
	test <- foldAllBr(debug_getTbl = 1)
	firstPb <- test %>% filter(checkCat < 0) %>% slice_min(rowid)
	rowsRange <- (firstPb$rowid - linesBefore):(firstPb$rowid + linesAfter)
	test %>%
		slice(rowsRange) %>%
		relocate(rowid, checkCat, content) %>%
		select(-opBrPN)
} # FAB_checkPbs
