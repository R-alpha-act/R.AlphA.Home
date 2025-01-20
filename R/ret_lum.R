#' @title Adjust the Brightness of a Hex Color
#' @description Modifies the brightness of a color by multiplying its RGB
#' components by a specified factor.
#' @param hexCol Character. The color to adjust, specified in
#' hexadecimal format (e.g., "#FF5733").
#' @param rgbFact Numeric. The factor by which to multiply the RGB components.
#' Values greater than 1 increase brightness, while values between 0 and 1
#' decrease it. Ensure the result remain within the valid range (0-255).
#' @return A modified hex color in hexadecimal format.
#' @importFrom grDevices rgb
#' @importFrom grDevices col2rgb
#' @examples
#' # Example 1: Lightening a color
#' ret_lum("#FF5733", 1.5)  # Returns a lighter version of the input color
#'
#' # Example 2: Darkening a color
#' ret_lum("#FF5733", 0.7)  # Returns a darker version of the input color
#' @export
#'
ret_lum <- function(hexCol, rgbFact) {
	# Convert hex color to RGB (normalized to [0, 1])
	back2rgb <- grDevices::col2rgb(hexCol) / 255

	# Adjust brightness by the factor and clamp values to [0, 1]
	retLum <- pmax(0, pmin(back2rgb * rgbFact, 1))

	# Convert back to hex format
	back2Hex <- grDevices::rgb(retLum[1], retLum[2], retLum[3])
	return(back2Hex)
}

