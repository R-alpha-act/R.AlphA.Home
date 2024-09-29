#' @title set the Shiny BG color to chosen shade of grey
#' @description choose luminosity between 0 and 100
#' @param lum luminosity between 0 and 100
#' @return the different HTML tags for background, sidebar, (+ more ?)
#' @importFrom shinyWidgets setBackgroundColor
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' @export


shiny_lum_0_100 <- function(lum){
	lum_pc <- lum/100
	return(c(
		R.AlphA.Base::SBBGColor_lum_0_100((lum * 1.2) %>% min(100))
		, shinyWidgets::setBackgroundColor(rgb(lum_pc, lum_pc, lum_pc))

	))
}
