#' @title initialize columns
#' @description check if columns are present - if not create them with dft value
#' @param data table where you need to init cols
#' @param cols col names
#' @param value the value you want to initialize if column doesn't exist yet
#' @return directly change data by reference (DT way, maybe to be changed)
#' @export

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

