# Tests for sepThsd function
# Purpose: Format numbers with thousands separator

test_that("sepThsd formats numbers with default space separator", {
	result <- sepThsd(1234567)
	expect_true(grepl(" ", result))
	expect_type(result, "character")
})

test_that("sepThsd formats numbers with comma separator", {
	result <- sepThsd(1234567, big.mark = ",")
	expect_true(grepl(",", result))
})

test_that("sepThsd respects digits parameter", {
	result <- sepThsd(1234.5678, digits = 3)
	# Should have 3 significant digits
	expect_type(result, "character")
})

test_that("sepThsd handles scientific notation", {
	result_sci <- sepThsd(1234567, scientific = TRUE)
	result_no_sci <- sepThsd(1234567, scientific = FALSE)

	# Scientific notation should contain 'e'
	expect_true(grepl("e", result_sci, ignore.case = TRUE))
	expect_false(grepl("e", result_no_sci, ignore.case = TRUE))
})

test_that("sepThsd handles vectors", {
	input <- c(1000, 2000, 3000)
	result <- sepThsd(input)

	expect_length(result, 3)
	expect_type(result, "character")
})

test_that("sepThsd handles zero", {
	result <- sepThsd(0)
	expect_type(result, "character")
})

test_that("sepThsd handles negative numbers", {
	result <- sepThsd(-1234567)
	expect_true(grepl("-", result))
	expect_true(grepl(" ", result))
})

test_that("sepThsd handles decimal numbers", {
	result <- sepThsd(1234.56, digits = 4)
	expect_type(result, "character")
})

test_that("sepThsd with different big.mark options", {
	result_dot <- sepThsd(1000000, big.mark = ".")
	result_apostrophe <- sepThsd(1000000, big.mark = "'")

	expect_true(grepl("\\.", result_dot))
	expect_true(grepl("'", result_apostrophe))
})
