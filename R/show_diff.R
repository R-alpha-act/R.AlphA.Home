#' Compare two texts or files with diffobj
#'
#' This function compares two inputs (files or text strings) and displays
#' the differences using the diffobj package with syntax highlighting.
#'
#' @param input1 A character string. Either a file path or text content to compare.
#' @param input2 A character string. Either a file path or text content to compare.
#'
#' @return A diffobj object containing the visual comparison of the two inputs.
#'
#' @examples
#' # Compare two text strings
#' show_diff("Hello\nWorld", "Hello\nR World")
#'
#' # Compare two files
#' \dontrun{
#' show_diff("file1.txt", "file2.txt")
#' }
#'
#' # Mix file and text
#' \dontrun{
#' show_diff("file.txt", "New content\nWith changes")
#' }
#'
#' @import diffobj
#' @export
show_diff <- function(input1, input2) {
	get_content <- function(input) {
		if (is.null(input) || length(input) == 0 || is.na(input) || nchar(input) == 0) {
			return(character(0))
		} else if (file.exists(input)) {
			return(readLines(input, warn = FALSE))
		} else {
			return(strsplit(input, "\n", fixed = TRUE)[[1]])
		}
	} # extrait le contenu d'une chaine ou d'un fichier
	{
		content1 <- get_content(input1)
		content2 <- get_content(input2)
		content1_clean <- trimws(content1)
		content2_clean <- trimws(content2)
	} # Supprimer les espaces en début/fin de ligne
	{
		# variables couleurs / constantes
		# TODO : rstudioapi::getThemeInfo()
		bg_body          <- "#1c2249"
		header_color     <- "#404c7d"
		header_text      <- "#cfe6ff"
		number_color     <- "#DDDDDD"
		text_color       <- "#d7e6f0"
		code_deleted_bg  <- "#f2525260"
		code_deleted_text<- "#ffdfe0"
		code_added_bg    <- "#3b755f60"
		code_added_text  <- "#dff2df"
		insert_bg        <- "#8fe6b3"
		insert_text      <- "#e6ffee"
		cell_border      <- "#DDDDDD"

		# Construction du CSS en utilisant les variables
		css <- sprintf(
			"body, html {
			  background-color: %s !important;
			  color: %s;
			}

			/* diffobj table layout */
			.diffobj-container table { width:100%%; border-collapse:collapse; }

			/* headers */
			.diffobj-container .banner {
			  background: %s;
			  color: %s;
			  padding: 8px 12px;
			  font-weight: 700;
			  text-align: center;
			  font-size: 15px;
			  letter-spacing: 0.2px;
			  border-bottom: 1px solid rgba(255,255,255,0.03);
			}

			/* line numbers */
			.diffobj-container .line-no {
			  background: transparent;
			  color: %s;
			  padding: 6px 8px;
			  text-align: right;
			  width: 40px;
			  font-size: 12px;
			}

			/* code cells */
			.diffobj-container pre {
			  padding: 8px 12px;
			  font-size: 13px;
			  line-height: 1.5;
			  white-space: pre-wrap;
			  color: %s;
			  background: transparent;
			  margin: 0;
			}

			/* deleted chunk: dark muted red with readable text */
			.diffobj-container .delete {
			  background: %s;
			  color: %s;
			  border-radius: 2px;
			}

			/* added chunk: dark muted green */
			.diffobj-container .insert {
			  background: %s;
			  color: %s;
			  border-radius: 2px;
			}

			/* diffobj word diffs */
			.diffobj-container .diffobj-word {
			  background: %s;
			  color: %s;
			  padding: 0 6px;
			  border-radius: 6px;
			  font-weight: 700;
			  margin-left: 6px;
			  box-shadow: inset 0 -1px 0 rgba(0,0,0,0.35);
			}

			/* subtle column separation */
			.diffobj-container table td { vertical-align: middle; }
			.diffobj-container table td.code { border-left: 1px solid %s; }
			",
			bg_body,
			text_color,
			header_color, header_text,
			number_color,
			text_color,
			code_deleted_bg, code_deleted_text,
			code_added_bg, code_added_text,
			insert_bg, insert_text,
			cell_border
		)
	} # Définir le CSS dark custom
	{
		diff_result <- diffobj::diffChr(
			target = content1_clean,
			current = content2_clean,
			mode = "sidebyside",
			tar.banner = "Original",
			cur.banner = "Modified",
			color.mode = "rgb",
			format = "html",
			style = list(
				html.output = "diff.w.style"
			)
		)
	} # Comparer avec diffobj
	{
		html_output <- as.character(diff_result)
		styled_output <- paste0(
			"<style>", css, "</style>",
			'<div class="diffobj-container">',
			html_output,
			'</div>'
		)
		class(styled_output) <- c("html", "character")
		styled_output
	} # Enrober le résultat avec le CSS dark
}
