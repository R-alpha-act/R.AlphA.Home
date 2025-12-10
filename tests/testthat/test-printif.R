# Tests for printif function
# Purpose: Conditionally print an object

test_that("printif returns object unchanged when show = FALSE", {
	input <- 42
	result <- printif(input, show = FALSE)

	expect_equal(result, input)
})

test_that("printif returns object when show = TRUE", {
	input <- "hello"
	result <- printif(input, show = TRUE)

	expect_equal(result, input)
})

test_that("printif accepts numeric 0/1 for show parameter", {
	input <- data.frame(a = 1:3)

	result_0 <- printif(input, 0)
	result_1 <- printif(input, 1)

	expect_equal(result_0, input)
	expect_equal(result_1, input)
})

test_that("printif works with different object types", {
	# Vector
	vec <- c(1, 2, 3)
	expect_equal(printif(vec, 0), vec)

	# List
	lst <- list(a = 1, b = 2)
	expect_equal(printif(lst, 0), lst)

	# Data frame
	df <- data.frame(x = 1:3, y = 4:6)
	expect_equal(printif(df, 0), df)

	# Matrix
	mat <- matrix(1:9, nrow = 3)
	expect_equal(printif(mat, 0), mat)
})

test_that("printif default show is FALSE", {
	input <- "test"
	result <- printif(input)

	expect_equal(result, input)
})

test_that("printif errors when called without argument", {
	expect_error(printif(), "must be used within a pipeline")
})

test_that("printif works in pipeline", {
	skip_if_not_installed("dplyr")

	result <- mtcars %>%
		head(3) %>%
		printif(0)

	expect_equal(nrow(result), 3)
})

test_that("printif handles NULL", {
	result <- printif(NULL, 0)
	expect_null(result)
})

test_that("printif handles NA", {
	result <- printif(NA, 0)
	expect_true(is.na(result))
})
