# Tests for ret_lum function
# Purpose: Adjust brightness of hex colors

test_that("ret_lum returns a hex color string", {
	result <- ret_lum("#FF5733", 1.0)

	expect_type(result, "character")
	expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
})

test_that("ret_lum with factor 1.0 returns similar color", {
	input <- "#808080"  # Gray
	result <- ret_lum(input, 1.0)

	# Should be very close to original (allowing for rounding)
	expect_equal(result, input, tolerance = 0.01)
})

test_that("ret_lum darkens color with factor < 1", {
	input <- "#FFFFFF"  # White
	result <- ret_lum(input, 0.5)

	# Result should be darker (gray)
	expect_true(result != "#FFFFFF")

	# Convert to RGB to verify darkening
	input_rgb <- grDevices::col2rgb(input)
	result_rgb <- grDevices::col2rgb(result)

	expect_true(all(result_rgb <= input_rgb))
})

test_that("ret_lum lightens color with factor > 1", {
	input <- "#808080"  # Gray
	result <- ret_lum(input, 1.5)

	# Result should be lighter
	input_rgb <- grDevices::col2rgb(input)
	result_rgb <- grDevices::col2rgb(result)

	expect_true(all(result_rgb >= input_rgb))
})

test_that("ret_lum clamps values to [0, 1]", {
	# Black with high factor should not exceed white
	result <- ret_lum("#808080", 10)
	expect_equal(result, "#FFFFFF")

	# Any color with factor 0 should be black
	result_black <- ret_lum("#FF5733", 0)
	expect_equal(result_black, "#000000")
})

test_that("ret_lum handles pure colors", {
	# Pure red
	result_red <- ret_lum("#FF0000", 0.5)
	result_rgb <- grDevices::col2rgb(result_red)

	expect_equal(as.numeric(result_rgb[2, 1]), 0)  # Green should stay 0
	expect_equal(as.numeric(result_rgb[3, 1]), 0)  # Blue should stay 0
})

test_that("ret_lum handles different color formats", {
	# Should work with lowercase hex
	result <- ret_lum("#ff5733", 1.0)
	expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
})

test_that("ret_lum preserves relative color ratios", {
	input <- "#FF8040"  # Orange-ish
	factor <- 0.5
	result <- ret_lum(input, factor)

	input_rgb <- grDevices::col2rgb(input) / 255
	result_rgb <- grDevices::col2rgb(result) / 255

	# Ratios should be preserved (with some tolerance for rounding)
	# R:G ratio should be similar
	if (input_rgb[2, 1] > 0) {
		input_ratio <- input_rgb[1, 1] / input_rgb[2, 1]
		result_ratio <- result_rgb[1, 1] / result_rgb[2, 1]
		expect_equal(input_ratio, result_ratio, tolerance = 0.1)
	}
})
