#' @title quick look at first rows
#' @description same doal as str but with vertically aligned values
#' @param x table
#' @param n number of rows to display
#' @return nothing
#' @export
#'
slicetr <- function(x, n = 3){
	x %>% ungroup %>% slice(1:n) %>% t %>% print
	return(NULL)
}
