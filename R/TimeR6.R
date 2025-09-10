#' Timer Class for Performance Measurement
#'
#' An R6 class for measuring and tracking execution time of code segments.
#' Provides functionality to add timing checkpoints, calculate time differences,
#' and generate summary reports of performance metrics.
#'
#' @section Public Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize a new Timer instance.}
#'   \item{\code{add(...)}}{Add a timing checkpoint with optional labels.}
#'   \item{\code{results(fill = TRUE)}}{Generate timing results as data.table.}
#'   \item{\code{stop()}}{Add final checkpoint and return results.}
#'   \item{\code{reset()}}{Clear all timing data.}
#' }
#'
#' @examples
#' \dontrun{
#' tmr <- TimeR6$new()
#' tmr$add("start")
#' # some code
#' tmr$add("step1")
#' results <- tmr$results()
#' tmr$stop()
#' tmr$reset()
#' }
#'
#' @import R6
#' @importFrom data.table data.table rbindlist
#' @export
TimeR6 <- R6Class(
	"Timer",
	private = list(
		timer_list = list()
	),
	public = list(
		#' @description Create a new Timer instance
		#' @return A TimeR6 object
		initialize = function() {
			private$timer_list <- list()
		},
		#' @description Add a timestamp
		#' @param ... Optional named labels attached to the timestamp.
		#' @return The object itself (invisible) for chaining
		add = function(...) {
			extra_args <- list(...)
			if (length(extra_args) > 0) {
				noName <- is.null(names(extra_args)) || names(extra_args)[1] == ""
				if (noName) names(extra_args)[1] <- "step"
				new_entry <- c(list(ct_dtm = Sys.time(), p_ct_dtm = proc.time()[3]), extra_args)
			}
			else new_entry <- list(ct_dtm = Sys.time(), p_ct_dtm = proc.time()[3])
			private$timer_list <- append(private$timer_list, list(new_entry))
			invisible(self)
		},
		#' @description Return the collected timings as a \code{data.table}
		#' @param fill Logical; if \code{TRUE}, fill missing columns when combining entries
		#' @return A \code{data.table} containing timestamps and time differences
		results = function(fill = TRUE) {
			if (length(private$timer_list) == 0) return(data.table())
			timer_table <- rbindlist(private$timer_list, fill = fill)
			timer_table[, ct_num := as.numeric(ct_dtm)]
			timer_table[, dt_num := c(diff(ct_num), 0)]
			timer_table[, p_dt_num := c(diff(p_ct_dtm), 0)]
			# timer_table[, dt_num_formated := sprintf(
			# 	'%0d.%03d',
			# 	trunc(dt_num),
			# 	trunc((dt_num %% 1) * 1000)
			# )]
			timer_table[, tot_num := cumsum(dt_num)]
			return(timer_table)
		},
		#' @description Add a final checkpoint named "end", print and return the results
		#' @return A \code{data.table} of the timing results
		stop = function() {
			self$add("end")
			result <- self$results()
			print(result)
			return(result)
		},
		#' @description Reset/clear all stored timings
		#' @return The object itself (invisible) for chaining
		reset = function() {
			private$timer_list <- list()
			invisible(self)
		}
	)
)
