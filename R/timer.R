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
timer <- function(timer_table = data.table(),start = FALSE, message = F, ...){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		message = T
		start = T
		warning("! parameters manually defined inside function for tests.",
		"Do not use results !")
		timer_table = data.table()
	}
	# t1, t2
		if(start) {
			t1 <- Sys.time()
		} else {
			t1 <- max(timer_table$heure)
		}
		t2 <- Sys.time()
		t1_s <- as.numeric(t1)
		t2_s <- as.numeric(t2)
	# dt : difference entre h_fin et h_debut	et remise en forme
		dt_seconds <- t2_s - t1_s
		# message("dt_seconds :", round(dt_seconds, 3))
		# dt_period <- as.period(dt_interval)
		dt_ms_part     <- trunc(dt_seconds %% 1 * 1000)
		dt_second_part <- trunc(dt_seconds / (1))          %% 60
		dt_min_part    <- trunc(dt_seconds / (1*60))       %% 60
		dt_hour_part   <- trunc(dt_seconds / (1*60*60))    %% 24
		dt_day_part    <- trunc(dt_seconds / (1*60*60*24))
		if (dt_seconds<60) {
			# print("seconds only")
			dt <- sprintf('%0d.%03d', dt_second_part, dt_ms_part)
		} else if (dt_seconds<60*60){
			# print("max : minutes")
			dt <- sprintf('%0d:%02d.%03d', dt_min_part, dt_second_part, dt_ms_part)
		} else if (dt_seconds<60*60*24){
			# print("max : hours")
			dt <- sprintf('%0d:%02d:%02d.%03d',dt_hour_part, dt_min_part, dt_second_part, dt_ms_part)
		} else {
			# print("max : more than hours anyway")
			dt <- sprintf('%0d:%02d:%02d:%02d.%03d',dt_day_part, dt_hour_part, dt_min_part, dt_second_part, dt_ms_part)
		}
	# time_inter : remet les resultats sous forme de table
		time_inter <- data.table(heure=t2, dt=dt,heure_seconds = t2_s, dt_seconds=dt_seconds)
	if (start) timer_table <- time_inter[0]
	# ajout des eventuelles colonnes supplementaires
		x = list(...)
		names_values <- data.table(
			name = names(x)
			, value = eval(x)
		)
		if (nrow(names_values)>0) {
			for (arg in 1:nrow(names_values)) {
				time_inter[, (names_values$name[arg]) := names_values$value[arg]]
			}
		}
	# timer_table :
		timer_table_final <- rbind(timer_table,time_inter, fill = TRUE)
		# if (message) print(paste0(
		# 	"time : ", time_inter$heure, " - step : ", time_inter$step
		# ))
		if (message) print(time_inter)
		return(timer_table_final)
}
