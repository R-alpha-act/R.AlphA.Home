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



# root <- dirname(rstudioapi::getSourceEditorContext()$path)
# workr_root <- sub("WorkR.*", "WorkR", root)
# list.files(file.path(workr_root, "Tests_xlsx"))
# setwd(root)
# a <- importAll(
# 	path = file.path(workr_root, "Tests_xlsx")
# 	# , pattern = "impall.*csv"
# 	, pattern = "impall"
# 	, ignore.case = T
# )
#
# testfun<- as.matrix(fread)
# getwd()
# # for (fSuffix in c(1,10,5,20,30,200)){
# # 	# fSuffix <- 10
# # 	write.csv(
# # 		data.table(
# # 			suffix = fSuffix
# # 			, b = "slfj"
# # 			, c = ISOdate(2020,1,1)
# # 			, d = runif(n = 10, min = 1*fSuffix, max = 10*fSuffix)
# # 		)
# # 		, file = file.path(workr_root, "Tests_xlsx", paste0("testImpAll_", fSuffix, ".csv"))
# # 		, row.names = FALSE
# # 	)
# # }
