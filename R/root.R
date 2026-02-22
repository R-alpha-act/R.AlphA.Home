#' @title Get Root Directory of Current Source File
#' @description Returns the directory path where the current source code file
#' is located, optionally the full file path, or builds a path relative to it.
#'
#' This function is especially useful when the same source code is used by multiple users,
#' each using their own environment with different file paths. It helps avoid writing
#' full paths in raw text inside source codes by dynamically retrieving the location
#' of the currently active source file in RStudio.
#'
#' When RStudio is not available (e.g. Rscript, terminal R, CI), falls back to
#' \code{getwd()} and silently forces \code{includeFName = FALSE}.
#'
#' @param ... Path components to append to the root directory. If empty, returns only
#' the directory path. If provided, builds a path using \code{file.path()}.
#' @param includeFName Logical. If \code{TRUE}, returns the full file path including
#' the filename instead of just the directory. Ignored if \code{...} is provided.
#' Silently ignored when RStudio is not available.
#'
#' @return A character string representing either:
#' \itemize{
#'   \item The absolute path of the directory containing the current source file (default)
#'   \item The full absolute path including the filename (if \code{includeFName = TRUE})
#'   \item A path built from the root directory and the provided components (if \code{...} given)
#'   \item The working directory via \code{getwd()} when outside RStudio
#' }
#'
#' @examples
#' \dontrun{
#' # Get only the directory path of the current source file
#' my_dir <- root()
#' print(my_dir)
#' # Example output: "/home/user/my_project/R"
#'
#' # Get the full path including filename
#' my_file <- root(includeFName = TRUE)
#' print(my_file)
#' # Example output: "/home/user/my_project/R/my_script.R"
#'
#' # Build a path relative to root
#' data_path <- root("data", "input.csv")
#' print(data_path)
#' # Example output: "/home/user/my_project/R/data/input.csv"
#' }
#'
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#' @export

root <- function(..., includeFName = FALSE){
	if (rstudioapi::isAvailable()) {
		sourceLoc <- rstudioapi::getSourceEditorContext()$path
		rootDirname <- dirname(sourceLoc)
	} else {
		sourceLoc <- getwd()
		rootDirname <- getwd()
		includeFName <- FALSE
	}
	if (length(list(...)) == 0) return(if (includeFName) sourceLoc else rootDirname)
	file.path(rootDirname, ...)
}
