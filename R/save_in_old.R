#' @title fonction de sauvegarde dans un dossier old
#' @description This function saves the project with date in its name in an "old" dir
#' @return nothing, only saves
#' @param sav_filepath file path of the file to save, default to current source
#' @param sav_fileNote note to be added in the save name
#' @param overwrite should an existing save with same name be overwritten
#' @export
#' @import ggplot2
#' @importFrom R.utils copyDirectory
#' @importFrom tools file_path_sans_ext file_ext

save_in_old <- function(sav_filepath = NULL, sav_fileNote = NULL, overwrite = F){
	requireNamespace("rstudioapi")
	requireNamespace("stringr")
	requireNamespace("tools")
	if (is.null(sav_filepath)) {
		sav_filepath <- rstudioapi::getSourceEditorContext()$path #chemin complet fichier
	}
	# sav_fileNote <- "test" # manual
	sav_filename <- sav_filepath %>% basename #uniquement le nom de fichier
	# sav_filename <- stringr::str_remove(sav_filepath, ".*/") #uniquement le nom de fichier
	sav_dirname <- dirname(sav_filepath) # nom du dossier
	sav_olddirname <- file.path(sav_dirname, "old")
	if(!dir.exists(sav_olddirname)) {
		print(paste0("creating old dir : ", sav_olddirname))
		dir.create(sav_olddirname)
	}

	# ajout note apres nom de fichier
	if (!is.null(sav_fileNote)) {
		sav_fileExt <- tools::file_ext(sav_filename) # extension
		sav_fileNameNoExt <- tools::file_path_sans_ext(sav_filename) # flexion
		sav_filename <- paste0(
			sav_fileNameNoExt, " (", sav_fileNote, ")", ".", sav_fileExt
		)
	}

	isDir <- dir.exists(sav_filepath)
	sav_savename <- paste0(Sys.Date(), " ", sav_filename)
	sav_savepath <- file.path(sav_olddirname, sav_savename)

	message("saving under : ", sav_savepath)
	if(file.exists(sav_savepath)){
		if(overwrite) message("file already exists, will be overwritten")
		if(!overwrite) message("file already exists : remove first")
	}

	if(isDir){
		message("(copying a dir)")
		R.utils::copyDirectory(sav_filepath, sav_savepath, overwrite = overwrite)
	} else {
		file.copy(from = sav_filepath, to = sav_savepath, overwrite = overwrite)
	}
}
