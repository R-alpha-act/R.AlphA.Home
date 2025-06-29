#' @title Sets a global option from a named list element
#'
#' @description This function takes an element from a named list as an argument
#' and sets a global option where the option name is the list's name and the
#' value is the corresponding string of the selected element.
#'
#' @param optionList An element from a named list, specified as `myList$element`.
#'
#' @details
#' - The function automatically extracts the list name from the argument.
#' - The option is then dynamically set using `options(list_name = element)`.
#'
#' @return The function does not return anything but sets an option that can be retrieved
#' using `getOption(list_name)`.
#'
#' @examples
#' myList <- list(model1 = "model_1", model2 = "model_2", model3 = "model_3")
#' setOption(myList$model1)
#' getOption("myList")  # Returns "model_1"
#'
#' @export

setOption <- function(optionList) {
	# Récupérer l'environnement parent pour trouver le nom de la liste
	optionList_name <- deparse(substitute(optionList)) # Récupère "myList$optionList1"

	# Extraire le nom de la liste et la clé du modèle
	match <- regexpr("^[^$]+", optionList_name)  # Prend la partie avant '$'
	list_name <- regmatches(optionList_name, match)  # Nom de la liste

	# Créer une liste nommée pour options()
	option_value <- list(optionList)
	names(option_value) <- list_name

	# Appliquer les options
	do.call(options, option_value)
}
