#' @title add variables so that data can be easily used in a pivot
#' @description will simply add dummy columns up to the number defined by the
#' user
#' @param data the data to add columns to
#' @param nCols number of columns to define
#' @param colPrefix for the dummy columns
#' @return the table with the chosen number of columns
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
