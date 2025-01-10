#' @title Quick Look at the First Rows of a Table
#' @description Displays the first rows of a table with vertically aligned
#' values, offering a clearer view than `str()`.
#' @param x Data frame or table. The input table to inspect.
#' @param n Integer. Number of rows to display. Default is 3.
#' @return None. The function prints the formatted rows to the console.
#' @export
#'
slicetr <- function(x, n = 3){
	x %>% ungroup %>% slice(1:n) %>% t %>% print
	return(NULL)
}
