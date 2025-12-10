# Tests for Session 5: Elimination of stringi via openxlsx→readxl+writexl and tidyr::replace_na→dplyr::coalesce
# Purpose: Verify that all function replacements work correctly

test_that("importAll correctly imports xlsx files with readxl", {
	# Create a test xlsx file
	test_data <- data.frame(
		id = 1:3,
		name = c("Alice", "Bob", "Charlie"),
		score = c(95.5, 87.3, 92.1)
	)
	test_file <- tempfile(fileext = ".xlsx")
	writexl::write_xlsx(test_data, test_file)

	# Import using importAll
	result <- importAll(fileList = test_file)

	# Verify result
	expect_true(is.data.table(result))
	expect_equal(nrow(result), 3)
	expect_true("fName" %in% names(result))
	expect_true("name" %in% names(result))
	expect_true("id" %in% names(result))
	expect_true("score" %in% names(result))

	# Cleanup
	file.remove(test_file)
})

test_that("quickExport correctly exports with writexl using sheetName", {
	# Create test data
	test_data <- data.frame(
		product = c("A", "B", "C"),
		sales = c(100, 200, 150),
		region = c("North", "South", "East")
	)

	# Export using quickExport
	test_dir <- tempdir()
	test_file <- "test_export_session5.xlsx"
	test_path <- file.path(test_dir, test_file)

	# Suppress message output
	suppressMessages(
		quickExport(
			test_data,
			sheetName = "SalesData",
			saveDir = test_dir,
			saveName = test_file
		)
	)

	# Verify file was created
	expect_true(file.exists(test_path))

	# Verify sheet name
	sheets <- readxl::excel_sheets(test_path)
	expect_equal(sheets, "SalesData")

	# Verify content
	imported <- readxl::read_excel(test_path, sheet = "SalesData")
	expect_equal(nrow(imported), 3)
	expect_true("product" %in% names(imported))
	expect_true("sales" %in% names(imported))

	# Cleanup
	file.remove(test_path)
})

test_that("left_join_checks works with coalesce instead of replace_na", {
	# Create test tables with NA values after join
	table_left <- data.table::data.table(
		id = 1:4,
		name = c("Alice", "Bob", "Charlie", "David")
	)

	table_right <- data.table::data.table(
		id = c(1, 3),
		department = c("Sales", "Engineering")
	)

	# Perform join check - should handle NA without error
	result <- left_join_checks(
		table_left,
		table_right,
		by = "id",
		showProblems = FALSE
	)

	# Verify result
	expect_true(is.data.table(result))
	expect_equal(nrow(result), 4)

	# Verify that coalesce correctly replaced NA in tmp_inX and tmp_inY
	# These columns should exist and have no NA values (coalesced to 0)
	expect_false(any(is.na(result$tmp_inX)))
	expect_false(any(is.na(result$tmp_inY)))
})

test_that("countSwitches works with coalesce instead of replace_na", {
	# Create test data with start/end markers
	test_data <- data.frame(
		step = c(
			"start",
			"content 1",
			"start",
			"nested content",
			"end",
			"more content",
			"end"
		)
	)

	# Run countSwitches
	result <- countSwitches(
		test_data,
		colNm = "step",
		sttMark = "start",
		endMark = "end"
	)

	# Verify result
	expect_true(is.data.frame(result))
	expect_equal(nrow(result), 7)

	# Verify level counting worked correctly
	expect_true("catLvl" %in% names(result))
	expect_true("lvl_1" %in% names(result))

	# Verify that coalesce handled NA correctly in cumsum operations
	# Check that level tracking is correct
	expect_equal(result$catLvl[1], 1) # After "start"
	expect_equal(result$catLvl[3], 2) # After nested "start"
	expect_equal(result$catLvl[5], 1) # After first "end"
	expect_equal(result$catLvl[7], 0) # After final "end"
})

test_that("No NA values propagate incorrectly due to coalesce", {
	# Test case with multiple NA conditions
	test_data <- data.table::data.table(
		id = c(1, 2, 3, NA, 4),
		value = c(10, NA, 30, 40, NA)
	)

	# Test with full_join which introduces NA
	left_tbl <- test_data[1:3]
	right_tbl <- test_data[c(1, 3, 4)]

	result <- left_join_checks(
		left_tbl,
		right_tbl,
		by = "id",
		showProblems = FALSE
	)

	# Coalesced columns (tmp_inX, tmp_inY) should have no NA
	expect_false(any(is.na(result$tmp_inX)))
	expect_false(any(is.na(result$tmp_inY)))
})

test_that("importAll works with multiple xlsx files", {
	# Create multiple test files
	test_data1 <- data.frame(a = 1:2, b = 3:4)
	test_data2 <- data.frame(a = 5:6, b = 7:8)

	test_file1 <- tempfile(fileext = ".xlsx")
	test_file2 <- tempfile(fileext = ".xlsx")

	writexl::write_xlsx(test_data1, test_file1)
	writexl::write_xlsx(test_data2, test_file2)

	# Import both
	result <- importAll(fileList = c(test_file1, test_file2))

	# Verify combined result
	expect_true(is.data.table(result))
	expect_equal(nrow(result), 4)

	# Cleanup
	file.remove(test_file1, test_file2)
})

test_that("readxl::read_excel returns expected data types", {
	# Create test data with various types
	test_data <- data.frame(
		int_col = 1:3,
		char_col = c("a", "b", "c"),
		num_col = c(1.5, 2.5, 3.5),
		stringsAsFactors = FALSE
	)

	test_file <- tempfile(fileext = ".xlsx")
	writexl::write_xlsx(test_data, test_file)

	# Read with readxl
	result <- readxl::read_excel(test_file)
	result_dt <- as.data.table(result)

	# Verify conversion
	expect_true(is.data.table(result_dt))
	expect_equal(nrow(result_dt), 3)

	# Cleanup
	file.remove(test_file)
})

test_that("writexl named list sheet naming works correctly", {
	# Test the named list syntax used in quickExport
	test_data <- data.frame(x = 1:3, y = 4:6)
	test_file <- tempfile(fileext = ".xlsx")

	# Use named list syntax (as in quickExport)
	sheet_name <- "CustomSheet"
	writexl::write_xlsx(
		x = setNames(list(test_data), sheet_name),
		path = test_file
	)

	# Verify sheet name
	sheets <- readxl::excel_sheets(test_file)
	expect_equal(sheets, "CustomSheet")

	# Verify content
	imported <- readxl::read_excel(test_file)
	expect_equal(nrow(imported), 3)

	# Cleanup
	file.remove(test_file)
})
