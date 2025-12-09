#' @title Unfold Code Sections in RStudio
#' @description ralpha_fold() and ralpha_unfold() allow usage of the R.AlphA
#' code format that keeps long scripts easily readable.
#'
#' This format is based on identifying code parts with brackets, and an
#' optional but recommended comment at the end :
#'
#' \preformatted{
#' \{
#'   ...
#'   code from part 1
#'   ...
#' \} # part 1
#' \{
#'   ...
#' \} # part 2
#' }
#'
#'then appearing as
#' \preformatted{
#' \{...\} # part 1
#' \{...\} # part 2
#' }
#'
#' To stay easy to manipulate, this format requires shortcuts to easily open
#' or close the different sections.
#'
#' ralpha_fold() will fold the different code parts and go back to beginning
#' of current part
#'
#' ralpha_unfold() will unfold a code part and jump to the next braces when
#' relevant.
#'
#' both combined will provide a convenient way to manage what is displayed on
#' screen, ensuring a constant global overview of the document.
#'
#' Shortcuts required:
#' Here are the suggested shortcuts, both for Mac and Windows :
#' \itemize{
#'   \item ralpha_fold   : use ctrl+up
#'   \item ralpha_unfold : use ctrl+down
#' }
#'
#' @return NULL (invisibly).
#' This function performs actions only (cursor movement and unfolding)
#'
#' @import rstudioapi
#' @importFrom data.table data.table
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr filter mutate group_by slice ungroup slice_min pull
#' @importFrom stringr str_locate_all str_locate str_detect
#' @importFrom stringi stri_extract stri_count
#' @export

ralpha_unfold <- function(){
	colFact <- 1E-3
	caseMsg <- 0
	{
		getPos     <- function(){
			srcContext %>%
				primary_selection() %>%
				getElement("range")
		} # getPos : get pos
		PN_DP      <- function(posNum, colFact = 1E-3){
			row <- floor(posNum)
			col <- ((posNum - row) / colFact) %>% round(8)
			document_position(row, col)
		} # PN_DP : pos num to document position
		DP_PN      <- function(docPos){
			posNum <- (docPos[1] + docPos[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DP_PN : document position to pos num
		DR_PN      <- function(docPos){
			posNum <- (docPos$start[1] + docPos$start[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DR_PN : document range to pos num
		PN_DR      <- function(posNum_start, posNum_end = posNum_start){
			SRow <- floor(posNum_start)
			SCol <- ((posNum_start - SRow) / colFact) %>% round(8)
			ERow <- floor(posNum_end)
			ECol <- ((posNum_end - ERow) / colFact) %>% round(8)
			document_range(
				start = document_position(SRow, SCol)
				, end = document_position(ERow, ECol)
			)
		} # PN_DR : pos num to document range
		endLine    <- function(posNum) ceiling(posNum) - colFact # endLine : endLine of a posNum

		handle_case_99  <- function() {
			executeCommand("unfold")
		}  # 99 : no more brackets
		handle_case_1   <- function() {
			nextBrInfo <- textTable %>%
				filter(!is.na(BrAbsPN)) %>%
				slice_min(rowid) %>%
				printif(0)
			setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
			executeCommand("unfold")
		}  # 1: noBr (default)
		handle_case_2   <- function() {
			nextBrInfo <- textTable %>%
				filter(!is.na(BrAbsPN)) %>%
				slice_min(rowid) %>%
				printif(0)
			executeCommand("unfold")
			setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
			executeCommand("unfold")
		}  # 2: afterBr
		handle_case_3   <- function() {
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine, cursorCol + 1))
		}  # 3: beforeBR
		handle_case_4   <- function() {
			setCursorPosition(document_position(cursorLine, 999))
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine+1, 0))
		}  # 4: blockEnd
		handle_case_5.1 <- function() {
			nextBrInfo <- textTable %>%
				filter(!is.na(BrAbsPN)) %>%
				slice_min(rowid) %>%
				printif(0)
			executeCommand("unfold")
			setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
		}  # 5.1: cursor position | } {
		handle_case_5.2 <- function() {
			setCursorPosition(document_position(cursorLine, findLastClosing-1))
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine, findNextOpen-1))
		}  # 5.2: cursor position } | {
		handle_case_5.3 <- function() {
			setCursorPosition(document_position(cursorLine, findLastClosing-1))
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine, findLastOpen))
		}  # 5.3: cursor position } { |
		handle_case_6   <- function() {
			if(OpBrBefore){
				setCursorPosition(document_position(cursorLine, findLastOpen))
			} else {
				setCursorPosition(document_position(cursorLine, findNextOpen))
			}
			executeCommand("unfold")
		}  # 6: startEnd
		handle_default  <- function() {
			printif("case not found", caseMsg)
			executeCommand("unfold")
		}  # dft
	} # local funs
	srcContext <- rstudioapi::getSourceEditorContext()
	retainPos <- getPos();
	retainPN <- DR_PN(retainPos)
	{
		srcAllText <- srcContext %>% getElement("contents")
		cursorLine <- retainPos$start[1]
		cursorCol <- retainPos$start[2]

		textTable <- tibble(lineFull = srcAllText) %>%
			rowid_to_column %>%
			filter(rowid >= cursorLine) %>%
			mutate(hasBracket = str_detect(lineFull, "\\{")) %>%
			mutate(isCurLine = rowid == cursorLine) %>%
			filter(hasBracket|isCurLine) %>%
			group_by(isCurLine) %>% slice(1) %>% ungroup %>%
			mutate(scanStart = ifelse(isCurLine, cursorCol, 1)) %>%
			mutate(lineStart = substr(lineFull, 0, scanStart-1)) %>%
			mutate(lineEnd = substr(lineFull, scanStart, nchar(lineFull))) %>%
			mutate(BrRelPos = str_locate(lineEnd, "\\{")[,1]) %>%
			mutate(BrAbsPos = BrRelPos + scanStart-1) %>%
			mutate(BrAbsPN = (rowid + (BrAbsPos+1) * colFact) %>% round(8)) %>%
			printif(0)

		curLineInfo <- textTable %>%
			filter(isCurLine) %>%
			printif(0)

		noOprBrNextLines <- nrow(textTable %>% filter(!isCurLine)) == 0
		OpBrBefore <- curLineInfo$lineStart %>% str_detect("\\{")
		ClBrBefore <- curLineInfo$lineStart %>% str_detect("\\}")
		OpBrAfter <- curLineInfo$lineEnd %>% str_detect("\\{")
		ClBrAfter <- curLineInfo$lineEnd %>% str_detect("\\}")
		findLastClosing <- curLineInfo$lineStart %>%
			str_locate_all("\\}") %>%
			as.data.frame %>%
			tail(1) %>%
			pull(start) %>%
			printif(0)
		findLastOpen <- curLineInfo$lineStart %>%
			str_locate_all("\\{") %>%
			as.data.frame %>%
			tail(1) %>%
			pull(start) %>%
			printif(0)
		findNextOpen <- curLineInfo$lineEnd %>%
			str_locate_all("\\{") %>%
			as.data.frame %>%
			head(1) %>%
			mutate(absPos = start + cursorCol) %>%
			pull(absPos) %>%
			printif(0)
	} # analyze text

	# cases handling ===========================================================
	# Identify which case we're in (order matters - check specific patterns first)
	active_case <- dplyr::case_when(
		noOprBrNextLines & !OpBrAfter ~ "case_99",  # 99: no more brackets
		curLineInfo$lineStart %>% grepl(pattern = "\\{$") ~ "case_2",  # 2: afterBr
		curLineInfo$lineEnd %>% grepl(pattern = "^\\{") ~ "case_3",  # 3: beforeBR
		# case 5: endStart → line with } and { (cursor position varies)
		(curLineInfo$lineFull %>% grepl(pattern = "\\}.*\\{")) & !ClBrBefore & OpBrAfter ~ "case_5.1",  # 5.1: cursor | } {
		(curLineInfo$lineFull %>% grepl(pattern = "\\}.*\\{")) & ClBrBefore & OpBrAfter ~ "case_5.2",  # 5.2: cursor } | {
		(curLineInfo$lineFull %>% grepl(pattern = "\\}.*\\{")) & ClBrBefore & !OpBrAfter ~ "case_5.3",  # 5.3: cursor } { |
		curLineInfo$lineFull %>% grepl(pattern = "\\{.*\\}") ~ "case_6",  # 6: startEnd
		curLineInfo$lineFull %>% grepl(pattern = "\\}") ~ "case_4",  # 4: blockEnd
		TRUE ~ "case_1"  # 1: noBr (default)
	)

	printif(active_case, caseMsg)

	# Execute action based on case
	switch(active_case,
		"case_99"  = handle_case_99(),
		"case_1"   = handle_case_1(),
		"case_2"   = handle_case_2(),
		"case_3"   = handle_case_3(),
		"case_4"   = handle_case_4(),
		"case_5.1" = handle_case_5.1(),
		"case_5.2" = handle_case_5.2(),
		"case_5.3" = handle_case_5.3(),
		"case_6"   = handle_case_6(),
		handle_default()  # default case
	)

	if(active_case != "case_99") executeCommand("expandToMatching")
	invisible(NULL)
}
