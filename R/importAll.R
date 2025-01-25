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
#' @examples
#' # Directory containing test files
#' test_path <- tempdir()
#'
#' # Create test files
#' write.csv(data.frame(a = 1:3, b = 4:6), file.path(test_path, "file1.csv"))
#' write.csv(data.frame(a = 7:9, b = 10:12), file.path(test_path, "file2.csv"))
#'
#' # Example 1 : Import and concatenate the files
#' result <- importAll(path = test_path, pattern = "\\.csv$")
#' print(result)
#'
#' # Example 2: Import files using fileList
#' file_list <- c("file1.csv", "file2.csv")
#' result <- importAll(path = test_path, fileList = file_list)
#' print(result)
#'
#' # Example 3: Import .rds files
#' saveRDS(data.frame(a = 1:5, b = 6:10), file.path(test_path, "file1.rds"))
#' saveRDS(data.frame(a = 11:15, b = 16:20), file.path(test_path, "file2.rds"))
#' result <- importAll(path = test_path, pattern = "\\.rds$")
#' print(result)
#'
#' # Example 4: Custom import function
#' custom_import <- function(file) {
#'   data <- read.csv(file, stringsAsFactors = FALSE)
#'   return(data)
#' }
#' result <- importAll(path = test_path, pattern = "\\.csv$", importFunction = custom_import)
#' print(result)
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
		#if(manualrun) print ("importFunction missing")
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
		#if(manualrun) print ("importFunction provided")
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

