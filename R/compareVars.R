#' @title Compare Table Variables
#' @description Compares column names in two tables based on a given pattern.
#' Provides information about which columns are present in which tables.
#' @param x A data frame representing the first table.
#' @param y A data frame representing the second table.
#' @param pattern A string pattern used to filter and compare only a subset of
#' variables (column names).
#' @return A list containing:
#' \itemize{
#'   \item \code{all}: All column names from both tables.
#'   \item \code{common}: Column names found in both tables.
#'   \item \code{onlyX}: Column names found only in the first table (\code{x}).
#'   \item \code{onlyY}: Column names found only in the second table (\code{y}).
#'   \item \code{exclusive}: Column names found in only one of the two tables.
#' }
#' @examples
#' # Example tables
#' table1 <- data.frame(a = 1:5, b = 6:10, c_test = 11:15)
#' table2 <- data.frame(b = 16:20, c_test = 21:25, d = 26:30)
#'
#' # Compare tables
#' comparison <- compareVars(table1, table2, pattern = "c_")
#'
#' # Print the comparison result
#' print(comparison)
#' @export
#
compareVars <- function(x, y, pattern = ""){
	result <- list()
	xVars <- grep(names(x), pattern = pattern, value = T)
	yVars <- grep(names(y), pattern = pattern, value = T)
	result$xVars <- xVars
	result$yVars <- yVars
	result$all <- union(xVars, yVars)
	result$common <- intersect(xVars, yVars)
	result$onlyX <- setdiff(xVars, yVars)
	result$onlyY <- setdiff(yVars, xVars)
	result$exclusive <- union(result$onlyX, result$onlyY)
	return(result)
}



