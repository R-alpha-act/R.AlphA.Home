# Tests for Rtimer R6 class
# Purpose: Performance measurement timer

test_that("Rtimer can be instantiated", {
	tmr <- Rtimer$new()

	expect_s3_class(tmr, "Timer")
	expect_s3_class(tmr, "R6")
})

test_that("Rtimer$add() adds timestamps", {
	tmr <- Rtimer$new()
	tmr$add("step1")
	tmr$add("step2")

	result <- tmr$get()

	expect_equal(nrow(result), 2)
})
test_that("Rtimer$get() returns data.table", {
	tmr <- Rtimer$new()
	tmr$add()

	result <- tmr$get()

	expect_s3_class(result, "data.table")
})

test_that("Rtimer$get() returns empty data.table when no timestamps", {
	tmr <- Rtimer$new()
	result <- tmr$get()

	expect_s3_class(result, "data.table")
	expect_equal(nrow(result), 0)
})

test_that("Rtimer tracks time differences", {
	tmr <- Rtimer$new()
	tmr$add("start")
	Sys.sleep(0.1)  # Small delay
	tmr$add("end")

	result <- tmr$get()

	# diffTime should be positive for first entry
	expect_true(result$diffTime[1] >= 0)
})

test_that("Rtimer stores step labels", {
	tmr <- Rtimer$new()
	tmr$add("first_step")
	tmr$add("second_step")

	result <- tmr$get()

	expect_true("step" %in% names(result))
	expect_equal(result$step[1], "first_step")
	expect_equal(result$step[2], "second_step")
})

test_that("Rtimer$add() returns self for chaining", {
	tmr <- Rtimer$new()

	result <- tmr$add("test")

	expect_identical(result, tmr)
})

test_that("Rtimer can chain add() calls", {
	tmr <- Rtimer$new()

	tmr$add("a")$add("b")$add("c")

	result <- tmr$get()
	expect_equal(nrow(result), 3)
})

test_that("Rtimer calculates totalTime correctly", {
	tmr <- Rtimer$new()
	tmr$add("start")
	Sys.sleep(0.05)
	tmr$add("mid")
	Sys.sleep(0.05)
	tmr$add("end")

	result <- tmr$get()

	# totalTime should be cumulative
	expect_true("totalTime" %in% names(result))
	expect_true(result$totalTime[2] >= result$totalTime[1])
})

test_that("Rtimer handles extra named arguments", {
	tmr <- Rtimer$new()
	tmr$add(step = "custom", extra = "value")

	result <- tmr$get()

	expect_true("extra" %in% names(result))
})

test_that("Multiple Rtimer instances are independent", {
	tmr1 <- Rtimer$new()
	tmr2 <- Rtimer$new()

	tmr1$add("from_tmr1")
	tmr2$add("from_tmr2")
	tmr2$add("another")

	result1 <- tmr1$get()
	result2 <- tmr2$get()

	expect_equal(nrow(result1), 1)
	expect_equal(nrow(result2), 2)
})
