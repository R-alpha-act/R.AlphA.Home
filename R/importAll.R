#' @title Function to Import and Concatenate Multiple Files
#' @description Imports all selected files, concatenates them into a single
#' table, and adds an `fName` variable.
#' @param path Path to the directory, passed to `list.files`.
#' @param pattern Pattern to match file names, passed to `list.files`.
#' @param ignore.case Logical. If `TRUE`, ignores case when matching file names.
#' Passed to `list.files`.
#' @param importFunction A custom function for importing files. If not set, the
#' function automatically selects an import method based on the file extension.
#' @param fill Logical. Passed to `rbind` to allow filling missing columns.
#' @param fileList A character vector of file names to import
#' (used instead of `pattern`).
#' @return A data frame containing the concatenated table.
#' @importFrom openxlsx read.xlsx
#' @export
#
importAll <- function(
	path = "."
	, pattern = ""
	, ignore.case = FALSE
	, importFunction = NULL
	, fill = F
	, fileList = NULL
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message("! parameters manually defined inside function for tests. ",
				"Do not use results !")
		root <- dirname(rstudioapi::getSourceEditorContext()$path)
		workr_root <- sub("WorkR.*", "WorkR", root)
		setwd(root)
		path = file.path(workr_root, "Tests_xlsx")
		pattern = "impall"
		ignore.case = TRUE
		importFunction = openxlsx::read.xlsx
		# lire des rds ?
		path <- workr_root %>%
			file.path(
				"Datacamp"
				, "Competitions"
				, "Abalone"
				, "Results"
				, "datas"
			# ) %>% list.files()
			) %>% print
		pattern <- "seed_99(1)*\\.rds"
		importFunction <- NULL
		fileList <- c(
			NULL
			, "/Users/Raphael/Google Drive/WorkR/Datacamp/Competitions/Abalone/Results/datas/compareModels_seed_99.rds"
			, "/Users/Raphael/Google Drive/WorkR/Datacamp/Competitions/Abalone/Results/datas/compareModels_seed_991.rds"
		)
	}

	if (missing(fileList)) {
		# with a pattern
		filePaths <- data.table(
			NULL,
			locPath = list.files(
				path = path,
				pattern = pattern,
				ignore.case = ignore.case,
				full.names = FALSE
			),
			fulPath = list.files(
				path = path,
				pattern = pattern,
				ignore.case = ignore.case,
				full.names = TRUE
			)
		)
	} else {
		# with a file list
		filePaths <- data.table(fulPath = file.path(path, fileList)) %>%
			mutate(locPath = fulPath %>% str_remove(".*/")) %>%
			as.data.table
	}
	# choosing import function depending on extensions
	if (missing(importFunction)) {
		if(manualrun) print ("importFunction missing")
		filePaths[, ext := gsub(".*\\.", "", locPath)]
		importFunsList <- do.call(rbind,list(NULL
			, data.table(ext = "xlsx"	, fun = function(x) as.data.table(openxlsx::read.xlsx(x)))
			, data.table(ext = "csv"	, fun = fread)
			, data.table(ext = "rds"	, fun = readRDS)
		))
		filePaths <- merge(
			filePaths, importFunsList
			, by = "ext"
		)
	} else {
		if(manualrun) print ("importFunction provided")
		testnames <- names(filePaths)
		# filePaths[, .(fun = importFunction), by = .(locPath, fulPath)]
		filePaths[, cst := T]
		importFunsList <- do.call(rbind,list(NULL
			, data.table(cst = T, fun = importFunction)
		))
		filePaths <- merge(
			filePaths, importFunsList
			, by = "cst"
		)[, cst := NULL]
	}
	if (length(unique(filePaths$fun)) > 1) {
		message("Warning: More than one type of file detected. ",
				"This might cause issues with column types (very risky).")
	}
	importsList <- mapply(
		FUN = function(ful_path, loc_path, importFunction){
			import <- importFunction(ful_path) %>% as.data.table
			import[, fName := loc_path]
		}
		, ful_path = filePaths$fulPath
		, loc_path = filePaths$locPath
		, importFunction = filePaths$fun
		, SIMPLIFY = F
	)
	concatenation <- do.call(
		function(...) rbind(..., fill = fill)
		, importsList
	)
}

