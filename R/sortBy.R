#' @title Sort a Data Frame by a Specified Column
#' @description Sorts a data frame by a specified column, in ascending or
#' descending order.
#'
#' @param df Data frame. The data frame to be sorted.
#' @param col Character. The name of the column by which to sort the data frame.
#' @param desc Logical. Should the sorting be in descending order?
#' Default is `FALSE` (ascending order).
#'
#' @return A data frame sorted by the specified column.
#' @export
#'
#' @examples
#' # Create a sample data frame
#' subject <- c(1:10)
#' height <- round(rnorm(10, mean = 66, sd = 3), 1)
#' d <- data.frame(subject, height)
#'
#' # Sort by height in ascending order
#' d2 <- sortby(d, "height")
#' print(d2)
#'
#' # Sort by height in descending order
#' d3 <- sortby(d, "height", desc = TRUE)
#' print(d3)
#'
sortby <- function(df, col, desc = FALSE) {
	df[order(
		eval(
			str2expression(paste0("df$", col)) # allows us to dynamically create a df$col
		),
		decreasing = desc
	),]
}
