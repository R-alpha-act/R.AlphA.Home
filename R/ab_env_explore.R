#' @title List Objects in R Environment
#' @description Lists all objects in the user's R environment with their class,
#' dimensions, and memory size. Designed to be called by AlphABot tools
#' but usable standalone.
#'
#' @param envir The environment to list. Default: \code{globalenv()}.
#' @param max_objects Maximum number of objects to return. Default: 100.
#'
#' @return A data.frame with columns: \code{name}, \code{class}, \code{dim},
#' \code{size_bytes}, \code{size_human}. Sorted by size descending.
#' Returns an empty data.frame if the environment is empty.
#'
#' @export
#'
#' @examples
#' # List objects in current globalenv
#' ab_list_env()
#'
#' # List objects in a custom environment
#' env_test <- new.env()
#' env_test$x <- 1:100
#' env_test$df <- mtcars
#' ab_list_env(envir = env_test)
#'
ab_list_env <- function(envir = globalenv(), max_objects = 100) {
	obj_names <- ls(envir = envir)
	if (length(obj_names) == 0) {
		return(data.frame(
			name = character(0)
			, class = character(0)
			, dim = character(0)
			, size_bytes = numeric(0)
			, size_human = character(0)
			, stringsAsFactors = FALSE
		))
	} # empty env

	info_list <- lapply(obj_names, function(nm) {
		obj <- tryCatch(
			get(nm, envir = envir)
			, error = function(e) NULL
		)
		if (is.null(obj)) {
			return(data.frame(
				name = nm
				, class = "???"
				, dim = "—"
				, size_bytes = 0
				, size_human = "???"
				, stringsAsFactors = FALSE
			))
		}
		obj_class <- class(obj)[1]
		obj_dim <- if (!is.null(dim(obj))) {
			paste(dim(obj), collapse = " x ")
		} else if (is.atomic(obj) || is.list(obj)) {
			paste0("length ", length(obj))
		} else {
			"—"
		}
		obj_size <- as.numeric(object.size(obj))
		list(
			name = nm
			, class = obj_class
			, dim = obj_dim
			, size_bytes = obj_size
		)
	}) # collect info per object

	df_env <- data.frame(
		name = vapply(info_list, `[[`, character(1), "name")
		, class = vapply(info_list, `[[`, character(1), "class")
		, dim = vapply(info_list, `[[`, character(1), "dim")
		, size_bytes = vapply(info_list, `[[`, numeric(1), "size_bytes")
		, stringsAsFactors = FALSE
	)
	df_env$size_human <- vapply(df_env$size_bytes, .format_size, character(1))
	df_env <- df_env[order(-df_env$size_bytes), ]
	rownames(df_env) <- NULL

	if (nrow(df_env) > max_objects) {
		n_total <- nrow(df_env)
		df_env <- df_env[seq_len(max_objects), ]
		attr(df_env, "truncated") <- TRUE
		attr(df_env, "total_objects") <- n_total
	}
	df_env
}


#' @title Inspect a Specific R Object
#' @description Returns a detailed text description of an R object from the
#' user's environment. Adapts the output format to the object class:
#' data.frame/tibble, model, list, vector, function, etc.
#'
#' @param name Character. Name of the object to inspect.
#' @param envir The environment to look in. Default: \code{globalenv()}.
#' @param max_lines Maximum number of output lines. Default: 50.
#'
#' @return A single character string with the inspection output.
#'
#' @export
#'
#' @examples
#' ab_inspect_object("mtcars")
#'
ab_inspect_object <- function(name, envir = globalenv(), max_lines = 50) {
	if (!exists(name, envir = envir)) {
		return(paste0("Object '", name, "' not found in environment."))
	}
	obj <- get(name, envir = envir)
	obj_class <- class(obj)
	out_lines <- character(0)

	out_lines <- c(
		out_lines
		, paste0("=== ", name, " ===")
		, paste0("Class: ", paste(obj_class, collapse = ", "))
	)

	if (is.data.frame(obj)) {
		out_lines <- c(
			out_lines
			, paste0("Dim: ", nrow(obj), " rows x ", ncol(obj), " cols")
			, ""
			, "--- Column types ---"
			, .col_types_summary(obj)
			, ""
			, "--- head(6) ---"
			, capture.output(print(utils::head(obj, 6)))
			, ""
			, "--- summary ---"
			, capture.output(summary(obj))
		)
	} else if (inherits(obj, c("lm", "glm", "nls"))) {
		out_lines <- c(
			out_lines
			, ""
			, "--- summary ---"
			, capture.output(summary(obj))
		)
	} else if (is.list(obj)) {
		out_lines <- c(
			out_lines
			, paste0("Length: ", length(obj))
			, paste0("Names: ", paste(utils::head(names(obj), 20), collapse = ", "))
			, ""
			, "--- str (max.level = 2) ---"
			, capture.output(utils::str(obj, max.level = 2, give.attr = FALSE))
		)
	} else if (is.function(obj)) {
		formals_txt <- capture.output(print(formals(obj)))
		body_txt <- capture.output(print(body(obj)))
		out_lines <- c(
			out_lines
			, ""
			, "--- formals ---"
			, formals_txt
			, ""
			, "--- body ---"
			, body_txt
		)
	} else if (is.atomic(obj)) {
		out_lines <- c(
			out_lines
			, paste0("Length: ", length(obj))
			, paste0("Type: ", typeof(obj))
		)
		if (is.numeric(obj)) {
			out_lines <- c(
				out_lines
				, ""
				, "--- summary ---"
				, capture.output(summary(obj))
			)
		}
		if (length(obj) <= 20) {
			out_lines <- c(out_lines, "", "--- values ---", capture.output(print(obj)))
		} else {
			out_lines <- c(
				out_lines
				, ""
				, "--- head(20) ---"
				, capture.output(print(utils::head(obj, 20)))
			)
		}
	} else {
		out_lines <- c(
			out_lines
			, ""
			, "--- str ---"
			, capture.output(utils::str(obj, max.level = 2, give.attr = FALSE))
		)
	} # generic fallback

	if (length(out_lines) > max_lines) {
		out_lines <- c(
			out_lines[seq_len(max_lines)]
			, paste0("... (truncated, ", length(out_lines), " lines total)")
		)
	}
	paste(out_lines, collapse = "\n")
}


# ---- Internal helpers ----

#' Format bytes to human-readable size
#' @noRd
.format_size <- function(bytes) {
	if (bytes < 1024) return(paste0(bytes, " B"))
	if (bytes < 1024^2) return(paste0(round(bytes / 1024, 1), " KB"))
	if (bytes < 1024^3) return(paste0(round(bytes / 1024^2, 1), " MB"))
	paste0(round(bytes / 1024^3, 1), " GB")
}

#' Summarize column types for a data.frame
#' @noRd
.col_types_summary <- function(df) {
	col_classes <- vapply(df, function(x) class(x)[1], character(1))
	type_counts <- sort(table(col_classes), decreasing = TRUE)
	vapply(
		names(type_counts)
		, function(tp) paste0("  ", tp, ": ", type_counts[tp], " cols")
		, character(1)
	)
}
