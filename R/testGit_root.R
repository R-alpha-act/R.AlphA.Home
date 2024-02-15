# rm(list = ls())
# fonction de base pour avoir le dossier ds lequel se trouve le code --------
#' @title test if this fun is uploaded to git, and not locally
#' @description root : dirname of the source's location
# #' @importFrom stats runif
#' @export

testGit_root <- function(){
	sourceLoc <- rstudioapi::getSourceEditorContext()$path
	rootDirname <- dirname(sourceLoc)
	message("ok uploaded")
	return(rootDirname)
}
