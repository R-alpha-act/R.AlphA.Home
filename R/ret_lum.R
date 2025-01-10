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
#' @export
#'
ret_lum <- function(hexCol, rgbFact){
	# hexCol <- rgb(1,1,1)
	# rgbFact <- 0.7
	back2rgb <- grDevices::col2rgb(hexCol) / 255
	retLum <- back2rgb*rgbFact
	back2Hex <- grDevices::rgb(retLum[1], retLum[2], retLum[3])
	return(back2Hex)
}
