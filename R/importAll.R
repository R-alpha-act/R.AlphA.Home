#' @title Function to Import and Concatenate Multiple data files
#' @description Imports multiple files into a list, concatenates them into a single
#' table, and adds an `fName` variable.
#'
#' The files can be selected either by giving a file list (character vector), or
#' by specifying a pattern.
#'
#' @param path Path to the directory, passed to `list.files`.
#' @param pattern Pattern to match file names, passed to `list.files`.
#' @param ignore.case Logical. If `TRUE`, ignores case when matching file names.
#' Passed to `list.files`. Default behavior is case-sensitive (`FALSE`)
#' @param importFunction A custom function for importing files. If not set, the
#' function selects an import method based on the file extension.
#' @param fill Logical. Passed to `rbind` to allow filling missing columns.
#' @param fileList A character vector of file names to import
#' (used instead of `pattern`).
#'
#' @return A data frame containing the concatenated table with the fName column
#' @importFrom openxlsx read.xlsx
#' @importFrom data.table fread
#' @importFrom arrow read_parquet read_feather
#' @export
#'
#' @examples
#' # Directory containing test files
#' test_path <- tempdir()
#'
#' # Create test files
#' write.csv( data.frame(a = 1:3, b = 4:6)    , file.path(test_path, "file1.csv"))
#' write.csv( data.frame(a = 7:9, b = 10:12)  , file.path(test_path, "file2.csv"))
#' write.csv( data.frame(a = 3:5, b = 8:10)   , file.path(test_path, "file3.csv"))
#' saveRDS(   data.frame(a = 1:5, b = 6:10)   , file.path(test_path, "file1.rds"))
#' saveRDS(   data.frame(a = 11:15, b = 16:20), file.path(test_path, "file2.rds"))
#'
#' # Example 1 : Import all csv files
#' result <- importAll(path = test_path, pattern = "\\.csv$")
#' print(result)
#'
#' # Example 2: Import only selected files
#' file_list <- c("file1.csv", "file2.csv")
#' result <- importAll(path = test_path, fileList = file_list)
#' print(result)
#'
#' # Example 3: Import all .rds files
#' result <- importAll(path = test_path, pattern = "\\.rds$")
#' print(result)
#'
#' # Example 4: Use a custom import function
#' custom_import <- function(file) {
#'   data <- read.csv(file, stringsAsFactors = FALSE)
#'   return(data)
#' }
#' result <- importAll(path = test_path, pattern = "\\.csv$", importFunction = custom_import)
#' print(result)
#'
importAll <- function(
		path = "."
		, pattern = ""
		, ignore.case = FALSE
		, importFunction = NULL
		, fill = FALSE
		, fileList = NULL
){

	is_absolute_path <- function(path) {
		if (length(path) == 0) return(FALSE)
		# R.utils function
		if (requireNamespace("R.utils", quietly = TRUE)) {
			return(R.utils::isAbsolutePath(path))
		}
		# If R.utils not available : starting with / or ~ (Unix/Mac),
		# "letter:" (Windows), or \\ (UNC)
		grepl("^(/|~|[A-Za-z]:|\\\\)", path)
	} # check if a path is absolute
	{
		if (missing(fileList)) {
			# with a pattern
			fullPaths_vec <- list.files(
				path = path,
				pattern = pattern,
				ignore.case = ignore.case,
				full.names = TRUE
			)
			filePaths <- data.table(fulPath = fullPaths_vec) %>%
				mutate(locPath = fulPath %>% basename) %>%
				as.data.table
		} else {
			filePaths <- data.table(locPath = fileList) %>%
				mutate(
					fulPath = ifelse(
						sapply(locPath, is_absolute_path),
						locPath,  # Absolute path - we keep it as is
						file.path(path, locPath)  # Relative path - we combine it with 'path'
					)
				) %>%
				as.data.table
		} # list file paths : either with pattern, or with fileList
		files_exist <- file.exists(filePaths$fulPath)
		if (!all(files_exist)) {
			missing_files <- filePaths$fulPath[!files_exist]
			warning(paste("The following files do not exist and will be ignored:",
						  paste(missing_files, collapse = ", ")))
			filePaths <- filePaths[files_exist, ]
		} # check files existence
		if (nrow(filePaths) == 0) {
			stop("No files found or all specified files are missing")
		}
	} # get paths either with pattern, or with fileList --> filePaths
	{
		# choosing import function depending on extensions
		if (missing(importFunction)) {
			filePaths[, ext := gsub(".*\\.", "", locPath)]
			importFunsList <- tribble(
				~ext       , ~fun
				, "xlsx"   , function(x) as.data.table(openxlsx::read.xlsx(x))
				, "csv"    , fread
				, "rds"    , readRDS
				, "parquet", read_parquet
				, "feather", read_feather
			) %>%
				as.data.table

			filePaths <- merge(filePaths, importFunsList, by = "ext", all.x = TRUE)

			# unsupported extensions
			if (any(is.na(filePaths$fun))) {
				unsupported_ext <- unique(filePaths[is.na(fun), ext])
				stop(paste(
					"following extension(s) are not supported by importAll:"
					, paste(unsupported_ext, collapse = ", ")
					,"\nSupported extensions: xlsx, csv, rds"
				))
			}
		} else {
			filePaths[, cst := TRUE]
			importFunsList <- tribble(
				~cst     , ~fun
				, TRUE   , importFunction
			) %>%
				as.data.table

			filePaths <- merge(filePaths, importFunsList, by = "cst")[, cst := NULL]
		}
	} # add the import function --> filePaths$fun
	{
		multipleFileTypes <- length(unique(filePaths$fun)) > 1
		warningMessage <- paste(
			sep = "\n"
			, "Different file types detected"
			, "columns will be automatically harmonized"

		)
		if (multipleFileTypes) {
			warning(warningMessage)
			harmonize_types <- TRUE
		} else {
			harmonize_types <- FALSE
		}
	} # warning when multiple file types
	{
		importsList <- mapply(
			FUN = function(ful_path, loc_path, importFunction){
				import <- importFunction(ful_path) %>% as.data.table
				import[, fName := loc_path]

				# Harmonization of types if necessary
				if (harmonize_types) {
					import <- import[, lapply(.SD, function(x) {
						if (is.factor(x)) as.character(x)
						else if (is.logical(x) && all(is.na(x))) as.character(x)
						else x
					})]
					import[, fName := loc_path]  # Restore fName apfter lapply
				}

				return(import)
			}
			, ful_path = filePaths$fulPath
			, loc_path = filePaths$locPath
			, importFunction = filePaths$fun
			, SIMPLIFY = FALSE
		)
	} # import all files --> importsList
	{
		concatenation <- do.call(
			function(...) rbind(..., fill = fill)
			, importsList
		)
		return(concatenation)
	} # concatenate all imports --> concatenation
} # import and concatenate multiple files
