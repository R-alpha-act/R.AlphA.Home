#' @title Initialize Missing Columns in Data Table
#' @description Adds missing columns to a data.table and initializes them
#' with a default value.
#' @param data A data.table to modify. Converts to data.table if necessary.
#' @param cols Character vector of column names to check and initialize.
#' @param value Default value to assign to missing columns.
#' @return The modified data.table, updated by reference.
#' @export
#'
initializeCols <- function(data, cols, value){
	presentNames <- data %>% names
	if(!is.data.table(data)) setDT(data)
	toAdd <- setdiff(cols, presentNames)
	if (length(toAdd)) {
		data[, (toAdd) := value]
		message(
			"column(s) ", paste(toAdd, collapse = ",")
			, " initialized to ", value
		)
	}
}

