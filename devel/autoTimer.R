#' @title Track Time Steps with Lubridate Functions
#' @description Tracks the time spent on different steps of your code,
#' storing only timestamps without durations.
#' @param start Logical. If `TRUE`, initializes a new time tracking table.
#' Default is `FALSE`.
#' @param timer_table A data.table containing the timer log to continue from.
#' Ignored if `start = TRUE`.
#' @param message Logical. If `TRUE`, prints the updated timer table after
#' adding the new entry. Default is `FALSE`.
#' @param ... Additional specifications. Use named arguments to define columns
#' and values for rows.
#' @return A data.table with the existing `timer_table` plus one new
#' entry containing the timestamp.
#' @examples
#' # Initialize a new timer table
#' tt_tests <- timer(start = TRUE)
#'
#' # Add a new row with the current timestamp
#' tt_tests <- timer(timer_table = tt_tests)
#'
#' # Add a new row with custom specifications
#' tt_tests <- timer(timer_table = tt_tests, stepName = "step x", anyVariableName = "any value")
#'
#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
library(data.table)
library(dplyr)

# Définition de la fonction avec gestion interne de timer_table
timer <- local({
	timer_table <- data.table::data.table()  # Variable locale persistante

	function(..., reset = FALSE, print = FALSE) {
		# Réinitialiser le tableau si demandé
		if (reset) {
			timer_table <<- data.table::data.table()  # Réinitialisation
			return(invisible(timer_table))
		}

		# Si "print" est demandé, afficher le tableau
		if (print) {
			print(timer_table)
			return(invisible(timer_table))
		}

		# Enregistrer l'heure actuelle
		t2 <- Sys.time()

		# Si le tableau est vide, initialiser avec un premier horodatage fictif
		if (nrow(timer_table) == 0) {
			t1 <- t2
			dt <- NA
			dt_seconds <- NA
		} else {
			t1 <- max(timer_table$heure, na.rm = TRUE)
			t1_s <- as.numeric(t1)
			t2_s <- as.numeric(t2)
			dt_seconds <- t2_s - t1_s
			dt <- sprintf('%0d.%03d', trunc(dt_seconds), trunc((dt_seconds %% 1) * 1000))
		}

		# Création de la nouvelle ligne
		time_inter <- data.table::data.table(
			heure = t2,
			dt = dt,
			heure_seconds = as.numeric(t2),
			dt_seconds = dt_seconds
		)

		# Ajout des éventuelles colonnes supplémentaires
		x <- list(...)
		names_values <- data.table::data.table(
			name = names(x),
			value = unlist(x, use.names = FALSE)
		)
		if (nrow(names_values) > 0) {
			for (arg in 1:nrow(names_values)) {
				time_inter[, (names_values$name[arg]) := names_values$value[arg]]
			}
		}

		# Ajouter la nouvelle ligne au tableau interne
		timer_table <<- data.table::rbindlist(list(timer_table, time_inter), fill = TRUE)

		# Retourner le tableau interne (invisible pour éviter l'affichage systématique)
		invisible(timer_table)
	}
})

# Initialisation et ajout des étapes
timer(reset = T)  # Réinitialisation du timer
timer(comment = "Début")  # Étape 1
Sys.sleep(1)
timer(comment = "Étape 1")  # Étape 2
Sys.sleep(2)
timer(comment = "Étape 2")  # Étape 3
timer(print=T)
