#' @title Save File in an "Old" Directory, prefixing it with current date
#' @description Saves a file with the current date in its name in an "old"
#' directory located in the same directory as the original file.
#'
#' @param sav_filepath Path of the file to save.
#' Defaults to the current source file.
#' @param sav_fileNote An optional custom note to append to the file name
#' for the save, allowing to keep track of why this save has been done.
#' @param overwrite Logical. Should an existing save with the same name
#' be overwritten? Default is `FALSE`.
#'
#' @return the output value of the function used to copy file
#' @import ggplot2
#' @importFrom R.utils copyDirectory
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom rstudioapi getSourceEditorContext
#' @export
#'
save_in_old <- function(sav_filepath = NULL, sav_fileNote = NULL, overwrite = F){
	{
		if (is.null(sav_filepath)) {
			sav_filepath <- rstudioapi::getSourceEditorContext()$path
		} # get file path
		sav_filename <- sav_filepath %>% basename # local fileName
		sav_dirname <- dirname(sav_filepath) # directory name
		sav_olddirname <- file.path(sav_dirname, "old")
	} # paths
	if(!dir.exists(sav_olddirname)) {
		print(paste0("creating old directory : ", sav_olddirname))
		dir.create(sav_olddirname)
	} # create "old" dir if not already here
	if (!is.null(sav_fileNote)) {
		sav_fileExt <- tools::file_ext(sav_filename) # extension only
		sav_fileNameNoExt <- tools::file_path_sans_ext(sav_filename) # wo ext
		sav_filename <- paste0(
			sav_fileNameNoExt, " (", sav_fileNote, ")", ".", sav_fileExt
		)
	} # add an optional note after file name

	sav_savename <- paste0(Sys.Date(), " ", sav_filename)
	sav_savepath <- file.path(sav_olddirname, sav_savename)

	message("saving under : ", sav_savepath)
	if(file.exists(sav_savepath)){
		if(overwrite) message("file already exists, will be overwritten")
		if(!overwrite) message("file already exists : remove first")
	} # warn if a file already exists with the name chosen

	isDir <- dir.exists(sav_filepath) # if path to save is a directory
	if(isDir){
		message("(copying a directory)")
		R.utils::copyDirectory(sav_filepath, sav_savepath, overwrite = overwrite)
	} else {
		file.copy(from = sav_filepath, to = sav_savepath, overwrite = overwrite)
	} #
}
