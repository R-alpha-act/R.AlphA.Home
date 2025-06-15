#' @title Conditionally Print an Object
#'
#' @description This function prints `x` if `show` is `TRUE`;
#' otherwise, it returns `x` unchanged.
#'
#' @param x Any object.
#' @param show A logical value indicating whether to print `x` (default: `FALSE`).
#'
#' @return The object `x`, possibly printed if `show` is `TRUE`.
#' @export
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   filter(mpg > 20) %>%
#'   summarise(mean_hp = mean(hp)) %>%
#'   printif(1)

printif <- function(x, show = FALSE, ...){
	if (missing(x)) {
		stop("printif() must be used within a pipeline or with an explicit object.", call. = FALSE)
	}
	if(show) print(x, ...) else identity(x)
}
