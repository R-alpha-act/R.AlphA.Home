#' @title List Unique Values of Columns Across Tables
#' @description Counts the unique values of columns across one or more
#' data.tables, optionally filtered by a pattern.
#' @param ... One or more data.tables, or a list of data.tables, to process.
#' @param pattern An optional regular expression pattern to filter column names.
#' @return A data.table summarizing the number of unique values for each column
#' in each table.
#' @examples
#' @export
#'
listVarsUniques <- function(..., pattern = ""){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		warning("! Parameters manually defined inside function for tests. ",
				"Do not use results !")
		DT1 <- data.table(a = 1:10)
		DT2 <- data.table(b = 2:10)
		DT3 <- data.table(b = 2:10, c = 3:11)
		tablesList <- list(DT1 = DT1, DT2 = DT2, DT3 = DT3)
	} else {
		if (inherits(..., "list")){
			tablesList <- (...)
		} else {
			tablesList <- list(...)
		}
	}
	countVals <- data.table()
	for (i in 1:length(tablesList)) {
		tb <- tablesList[[i]] %>% as.data.table
		# tb <- tablesList[[1]]
		countVals_int <- data.table()
		tbVarNames <- names(tb)
		tbVarNames <- grep(pattern, names(tb), value = TRUE)
		for (tbVarName in tbVarNames) {
			# tbVarName <- tbVarNames[1]
			tbVarLength <- length(unique(tb[, get(tbVarName)]))
			countVals_int[ , (tbVarName) :=  tbVarLength]
		}
		countVals_int[, tbName := names(tablesList)[i]]
		countVals <- rbind(countVals, countVals_int, fill = TRUE)
	}
	return(countVals)
}
