#' @title Track Time Steps Inside a Data.table
#' @description The `timer` function allows you to append timestamps to a data.table,
#' and include additional metadata provided as arguments.
#' Then calculate time differences between timestamps.
#' @param timer_table A data.table containing the timer log to continue from.
#' Defaults to an empty `data.table().
#' @param end A logical. If `TRUE`, the time differences between timestamps
#' are calculated and the table printed.
#' @param ... Additional specifications. Use named arguments to define columns
#' and values for rows.
#' @return A `data.table` containing the original data, plus one new timestamp,
#' and optionally computed time differences, the printed table includes:
#'   \itemize{
#'     \item `timestamp`: The current timestamp (`POSIXct`).
#'     \item `datetime`: The numeric representation of the timestamp.
#'     \item `time_diff`: The time difference in seconds between consecutive rows as a `difftime` object.
#'     \item `time_diff_seconds`: The formatted time difference in seconds with milliseconds as a character string.
#'     \item Additional columns for any metadata provided via `...`.
#'   }
#' @examples
#' library(data.table)
#' # Initialize an empty data.table
#' timer_table <- data.table()
#'
#' # Add the first timestamp
#' timer_table <- timer(timer_table, description = "Start")
#'
#' # Add another timestamp and compute time differences
#' timer_table <- timer(timer_table, end = TRUE, description = "Event 2")
#'
#' @import lubridate
#' @rawNamespace import(data.table, except =  c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek))
#' @export
#'
timer <- function(timer_table = data.table(), end = FALSE, ...) {
  curTime <- Sys.time()

  # Création de la nouvelle ligne avec heure et datetime
  time_inter <- data.table(timestamp = curTime, datetime = as.numeric(curTime))

  # Gestion des arguments supplémentaires via ...
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    # Ajouter les arguments supplémentaires comme colonnes dans time_inter
    for (name in names(extra_args)) {
      time_inter[, (name) := extra_args[[name]]]
    }
  }

  # Fusionner time_inter avec timer_table
  timer_table <- rbind(timer_table, time_inter, fill = TRUE)

  # Calcul des différences de temps si `end = TRUE`
  if (end == TRUE && nrow(timer_table) > 1) {
    # Calcul des différences formatées
    timer_table[, time_diff := c(NA, diff(timestamp))]
    timer_table[, time_diff_seconds := c(NA, sapply(diff(datetime), timeFormatted))]
  }

  return(timer_table)
}

timeFormatted <- function(rawTime) {
  if (is.na(rawTime)) return(NA) # Gérer les NA
  sprintf('%0d.%03d', trunc(rawTime), trunc((rawTime %% 1) * 1000))
}
