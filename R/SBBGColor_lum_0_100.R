#' @title set the Shiny BG color to chosen shade of grey
#' @description choose luminosity between 0 and 100
#' @param lum luminosity between 0 and 100
#' @return the HTML tag for sidebar color
#' @export

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
