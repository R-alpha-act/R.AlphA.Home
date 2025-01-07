#' @title Library Loader
#' @description Manually loads all specified libraries required by the user.
#' @return A message indicating the result of the library loading process.
#' @export
#'
getLibsR.AlphA <- function(){
	library(dplyr)
	library(data.table)
	library(lubridate)
	library(tidyr)
	# library(tidyverse)
	library(R.AlphA.Base)
	library(R.AlphA.Life)
	library(rstudioapi)
	library(forcats) # instead of tidyverse
	library(readr, exclude = "read_rds")   # instead of tidyverse
	library(ggplot2) # instead of tidyverse
	library(stringr) # instead of tidyverse
	library(purrr)   # instead of tidyverse
	library(tibble)  # instead of tidyverse
	library(arrow)
}
