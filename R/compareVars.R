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



