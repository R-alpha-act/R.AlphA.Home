# Tests for compareVars function
# Purpose: Compare column names between two tables

test_that("compareVars identifies common columns correctly", {
	table1 <- data.frame(id = 1:3, name = letters[1:3], value = 1:3)
	table2 <- data.frame(id = 4:6, name = letters[4:6], category = c("A", "B", "C"))

	result <- compareVars(table1, table2)

	expect_equal(sort(result$common), c("id", "name"))
	expect_equal(result$onlyX, "value")
	expect_equal(result$onlyY, "category")
})

test_that("compareVars identifies exclusive columns correctly", {
	table1 <- data.frame(a = 1, b = 2, c = 3)
	table2 <- data.frame(c = 4, d = 5, e = 6)

	result <- compareVars(table1, table2)

	expect_equal(sort(result$onlyX), c("a", "b"))
	expect_equal(sort(result$onlyY), c("d", "e"))
	expect_equal(sort(result$exclusive), c("a", "b", "d", "e"))
})

test_that("compareVars handles identical tables", {
	table1 <- data.frame(x = 1, y = 2, z = 3)
	table2 <- data.frame(x = 4, y = 5, z = 6)

	result <- compareVars(table1, table2)

	expect_equal(sort(result$common), c("x", "y", "z"))
	expect_length(result$onlyX, 0)
	expect_length(result$onlyY, 0)
	expect_length(result$exclusive, 0)
})

test_that("compareVars handles completely different tables", {
	table1 <- data.frame(a = 1, b = 2)
	table2 <- data.frame(c = 3, d = 4)

	result <- compareVars(table1, table2)

	expect_length(result$common, 0)
	expect_equal(sort(result$onlyX), c("a", "b"))
	expect_equal(sort(result$onlyY), c("c", "d"))
})

test_that("compareVars filters by pattern correctly", {
	table1 <- data.frame(var_a = 1, var_b = 2, other = 3)
	table2 <- data.frame(var_a = 4, var_c = 5, extra = 6)

	result <- compareVars(table1, table2, pattern = "var_")

	expect_equal(sort(result$all), c("var_a", "var_b", "var_c"))
	expect_equal(result$common, "var_a")
	expect_equal(result$onlyX, "var_b")
	expect_equal(result$onlyY, "var_c")
})

test_that("compareVars returns correct structure", {
	table1 <- data.frame(x = 1)
	table2 <- data.frame(y = 1)

	result <- compareVars(table1, table2)

	expect_type(result, "list")
	expect_true(all(c("all", "common", "onlyX", "onlyY", "exclusive") %in% names(result)))
})

test_that("compareVars handles empty pattern (all columns)", {
	table1 <- data.frame(col1 = 1, col2 = 2)
	table2 <- data.frame(col2 = 3, col3 = 4)

	result <- compareVars(table1, table2, pattern = "")

	expect_equal(sort(result$all), c("col1", "col2", "col3"))
})
