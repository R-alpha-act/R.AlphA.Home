#' @title Get Root Directory of Current Source File
#' @description Returns the directory path where the current source code file
#' is located.
#' @return A character string representing the absolute path of the directory
#' containing the current source file.
#' @export
#'
root <- function(){
	sourceLoc <- rstudioapi::getSourceEditorContext()$path
	rootDirname <- dirname(sourceLoc)
	return(rootDirname)
}
