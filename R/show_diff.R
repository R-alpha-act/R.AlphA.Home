#' Compare two texts or files with diffr
#'
#' This function compares two inputs (files or text strings) and displays
#' the differences using the diffr package with syntax highlighting.
#'
#' @param input1 A character string. Either a file path or text content to compare.
#' @param input2 A character string. Either a file path or text content to compare.
#'
#' @return A diffr object containing the visual comparison of the two inputs.
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
#' @import diffr
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
		temp1 <- tempfile(fileext = ".txt")
		temp2 <- tempfile(fileext = ".txt")
		writeLines(content1_clean, temp1)
		writeLines(content2_clean, temp2)
	} # Créer des fichiers temporaires nettoyés
	{
		widget <- diffr::diffr(
			temp1,
			temp2,
			contextSize = 2,
			minJumpSize = 5,
			wordWrap = TRUE,
			before = "Original",
			after = "Modified"
		)
	} # Utiliser diffr sur les fichiers nettoyés
	unlink(c(temp1, temp2))
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
			}

			/* table layout */
			.diffr table.diff { width:100%%; border-collapse:collapse; }

			/* headers */
			.diffr .diff-header {
			  background: %s;
			  color: %s;
			  padding: 8px 12px;
			  font-weight: 700;
			  text-align: center;
			  font-size: 15px;
			  letter-spacing: 0.2px;
			  border-bottom: 1px solid rgba(255,255,255,0.03);
			}

			/* small, compact line numbers */
			.diffr .line-no {
			  background: transparent;
			  color: %s;
			  padding: 6px 8px;
			  text-align: right;
			  width: 40px;
			  font-size: 12px;
			}

			/* code cells */
			.diffr .code {
			  padding: 8px 12px;
			  font-size: 13px;
			  line-height: 1.5;
			  white-space: pre-wrap;
			  color: %s;
			  background: transparent;
			}

			/* deleted chunk: dark muted red with readable text */
			.diffr .code.replace.before {
			  background: %s;
			  color: %s;
			  border-radius: 2px;
			}

			/* added chunk: dark muted green */
			.diffr .code.replace.after {
			  background: %s;
			  color: %s;
			  border-radius: 2px;
			}

			/* inserted characters: small pill */
			.diffr .char-insert {
			  background: %s;
			  color: %s;
			  padding: 0 6px;
			  border-radius: 6px;
			  font-weight: 700;
			  margin-left: 6px;
			  box-shadow: inset 0 -1px 0 rgba(0,0,0,0.35);
			}

			/* subtle column separation */
			.diffr table.diff td { vertical-align: middle; }
			.diffr table.diff td.code { border-left: 1px solid %s; }

			/* reduce visual noise for empty area */
			.diffr .diff { margin: 10px 6px; }
			",
			bg_body,
			header_color, header_text,
			number_color,
			text_color,
			code_deleted_bg, code_deleted_text,
			code_added_bg, code_added_text,
			insert_bg, insert_text,
			cell_border
		)

		# css est prêt — utilisation typique :
		# htmltools::browsable(htmltools::tagList(htmltools::HTML(css), result))
		# ou via onRender si tu préfères injecter après rendu :
		# widget2 <- htmlwidgets::onRender(widget, sprintf("function(el,x){var s=document.createElement('style');s.appendChild(document.createTextNode(%s));document.head.appendChild(s);} ", jsonlite::toJSON(css, auto_unbox=TRUE)))


		js <- sprintf("
	    function(el,x){
	      var css = %s;
	      var s = document.createElement('style'); s.type='text/css';
	      s.appendChild(document.createTextNode(css));
	      document.head.appendChild(s);
	    }", jsonlite::toJSON(css, auto_unbox = TRUE))

		htmlwidgets::onRender(widget, js)
	} # inject CSS after render
}
