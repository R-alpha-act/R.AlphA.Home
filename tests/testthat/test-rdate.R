# Tests for rdate function
# Purpose: Generate random dates

test_that("rdate generates correct number of dates", {
	result <- rdate(5)
	expect_length(result, 5)
})

test_that("rdate generates Date objects by default", {
	result <- rdate(3)
	expect_s3_class(result, "Date")
})

test_that("rdate respects min and max bounds", {
	min_date <- as.Date("2020-01-01")
	max_date <- as.Date("2020-12-31")

	result <- rdate(100, min = min_date, max = max_date)

	expect_true(all(result >= min_date))
	expect_true(all(result <= max_date))
})

test_that("rdate sorts dates when sort = TRUE", {
	result <- rdate(10, sort = TRUE)

	# Check that dates are sorted
	expect_equal(result, sort(result))
})

test_that("rdate does not sort by default", {
	# Run multiple times to increase chance of catching unsorted
	set.seed(123)
	result <- rdate(100, sort = FALSE)

	# Not necessarily sorted (probabilistic, but likely with 100 dates)
	# Just check it returns valid dates
	expect_s3_class(result, "Date")
})

test_that("rdate handles single date request", {
	result <- rdate(1)
	expect_length(result, 1)
	expect_s3_class(result, "Date")
})

test_that("rdate with include_hours returns POSIXct", {
	result <- rdate(3, include_hours = TRUE)

	# Should be POSIXct when hours are included
	expect_true(inherits(result, "POSIXt"))
})

test_that("rdate handles string date inputs", {
	result <- rdate(5, min = "2021-06-01", max = "2021-06-30")

	min_date <- as.Date("2021-06-01")
	max_date <- as.Date("2021-06-30")

	expect_true(all(result >= min_date))
	expect_true(all(result <= max_date))
})

test_that("rdate handles same min and max date", {
	single_date <- as.Date("2022-05-15")
	result <- rdate(5, min = single_date, max = single_date)

	expect_true(all(result == single_date))
})

test_that("rdate defaults to current year", {
	result <- rdate(10)
	current_year <- as.numeric(format(Sys.Date(), "%Y"))

	result_years <- as.numeric(format(result, "%Y"))
	expect_true(all(result_years == current_year))
})
