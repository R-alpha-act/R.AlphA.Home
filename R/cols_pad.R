#' @title Add Variables to Facilitate Data Usage in a Pivot Table
#' @description Adds dummy columns to reach the number specified by the user.
#' @param data The data frame to which dummy columns will be added.
#' @param nCols An integer specifying the total number of columns desired.
#' @param colPrefix A string used as the prefix for the names of dummy columns.
#' @return A data frame with the specified total number of columns.
#' @export

cols_pad <- function(data, nCols = 100, colPrefix = "x_"){
	# nCols <- 100
	# data <- AVRecap
	# colPrefix <- "x_"

	nColsData <- ncol(data)
	colsToAdd <- nCols - nColsData
	if(colsToAdd < 0){
		stop("data already has ", nColsData, " cols, >", nCols)
	}
	dummyAdd <- matrix(ncol = colsToAdd, nrow = nrow(data)) %>%
		as.data.frame %>%
		rename_with(~str_replace(.,"V", colPrefix))

	data %>% cbind(dummyAdd)
}
