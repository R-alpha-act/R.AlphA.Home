#' @title Adjust the Brightness of the Graphics Window for confortable viewing
#' @description Modifies the brightness level of the active graphics window by
#' adjusting its background color.
#'
#' This is especially useful when using dark RStudio themes, where a 100% white
#' graphic window creates an unconfortable contrast.
#'
#' @param lum Numeric. Brightness level, ranging from 0 (completely dark)
#' to 100 (maximum brightness).
#' @param reset Logical. If TRUE, resets to default settings.
#' @return no return value : only apply the theme_set() function
#' @importFrom grDevices rgb
#' @importFrom graphics par plot
#' @import ggplot2
#' @export
#'
lum_0_100 <- functionfunction(lum = NULL, reset = FALSE) {
	if (reset) {
		par(bg = "white")
		theme_set(theme_gray())
		return(invisible(NULL))
	}
	if (!is.numeric(lum) || lum < 0 || lum > 100) {
		stop("lum must be a numeric value between 0 and 100.")
	}
	# for the plot() function
	lum_pc <- lum/100; par(bg = rgb(lum_pc, lum_pc, lum_pc))

	# for ggplot()
	hex_leg <- rgb(lum_pc,lum_pc,lum_pc)
	dark_adjustments <- theme(
		plot.background = element_rect(fill = ret_lum(hex_leg,0.7))
		, legend.background = element_rect(fill = hex_leg)
		, panel.background = element_rect(fill = hex_leg)
		, panel.grid.major = element_line(size = rel(0.5), color = ret_lum(hex_leg, 0.7))
		, panel.grid.minor = element_line(size = rel(0.25), color = ret_lum(hex_leg, 0.7))
		, axis.text = element_text(color = ret_lum(hex_leg,0.4))
		, legend.key = element_rect(fill = ret_lum(hex_leg,0.8))
	)
	theme_set(theme_get()+dark_adjustments)
}


