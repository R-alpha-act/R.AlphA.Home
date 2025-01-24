#' @title Quick Look at the First Rows of a Table
#' @description Displays the first rows of a table with vertically aligned
#' values, offering a clearer view than `str()`.
#'
#' @param x Data frame or table. The input table to inspect.
#' @param n Integer. Number of rows to display. Default is 3.
#'
#' @return None. The function prints the formatted rows to the console.
#' @export
#'
#' @examples
#' # Create a data frame
#' table <- data.frame(
#'   Value = (1:10)**2,
#'   Category = ifelse(1:10 %% 2 == 0, "Even", "Odd")
#'   )
#' slicetr(table)
#'
#' slicetr(table, n = 5)
#'
slicetr <- function(x, n = 3){
	x %>% ungroup %>% slice(1:n) %>% t %>% print
}
