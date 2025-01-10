#' @title Set Shiny Background Color to a Chosen Shade of Grey
#' @description Adjusts the background color of a Shiny app's sidebar based
#' on a specified luminosity.
#' @param lum Numeric. Luminosity level, ranging from 0 (black) to 100 (white).
#' @return An HTML tag representing the background color for the sidebar.
#' @export
#'
SBBGColor_lum_0_100 <- function(lum){
	lum_pc <- lum/100
	hex_lum <- rgb(lum_pc, lum_pc, lum_pc)
	HTMLText <- paste0(
		""
		,'#sidebar {background-color: ', hex_lum,';}'
		# , 'body, label, input, button, select {font-family: "Arial";}'
	)
	tags$head(tags$style(HTML(HTMLText)))
}
