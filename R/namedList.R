#' @title Create a Named List from a Character Vector
#' @description Converts a character vector into a named list where each
#' element's name is its own value.
#' @param vec A character vector.
#' @return A named list where each element is named after its value.
#' @examples
#' namedList(c("Apple", "Banana", "Cherry"))
#' @export
#'
namedList <- function(vec) {
	setNames(as.list(vec), vec)
}
