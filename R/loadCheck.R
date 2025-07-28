#' @title Load and Install Package if Necessary
#' @description This function checks if a specified package is available in the
#' current R environment. If the package is not installed, it automatically
#' installs it with dependencies and then loads it. #' The function suppresses
#' startup messages to provide a clean loading experience.
#' @param package_name A character string specifying the name of the package to
#' install (if necessary), and load.
#' @return No return value.
#' @examples
#' # Load a commonly used package
#' loadCheck("dplyr")
#'
#' # Load a package that might not be installed
#' loadCheck("ggplot2")
#' @import utils
#' @export
#'
loadCheck <- function(package_name) {
	if (!requireNamespace(package_name, quietly = TRUE)) {
		message("Install required for package: ", package_name)
		install.packages(package_name, dependencies = TRUE, quiet = TRUE)
	}
	suppressPackageStartupMessages(library(package_name, character.only = TRUE, quietly = TRUE))
}
