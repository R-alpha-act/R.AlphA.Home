# rm(list = ls())
# fold brackets --------
#' @title _FoldAllBr
#' @description finds open brackets in current code and fold them
#' @param time get a timer on the function steps
#' @param debug_getTbl for debugging : return the docContent table with tags
#' @importFrom tibble rowid_to_column
#' @importFrom stringr str_detect str_remove_all str_extract str_remove
#' @import rstudioapi
#' @export
options(FAB_wait = 0.5)
foldAllBr <- function(time = F, debug_getTbl = F){

fnTmr <- timer(start = T, endOf = "start")
colFact <- 1E-2
{
	# foldBrLine : given a line, fold the bracket ending it ====================
	foldBrLine <- function(opLine, waitTime = 0){
		# opLine <- opBrList[2]
		setCursorPosition(document_position(opLine, 999))
		Sys.sleep(waitTime)
		executeCommand("expandToMatching")
		Sys.sleep(waitTime)
		executeCommand("fold")
		Sys.sleep(waitTime)
		# return(getPos())
	}


	# R.AlphA.Base::getLibsR.AlphA()
	# getPos : get pos =========================================================
	getPos <- function(){
		getSourceEditorContext() %>%
			primary_selection() %>%
			getElement("range")
	}

	# PN_DP : pos num to document pos ==========================================
	PN_DP <- function(posNum){
		row <- floor(posNum)
		col <- ((posNum - row) / colFact) %>% round(8)
		document_position(row, col)
	}
	# DP_PN : document pos to pos num ==========================================
	DP_PN <- function(docPos) {
		posNum <- docPos[1] + (docPos[2] * colFact) %>% round(8)
		as.numeric(posNum)
	}

	# DR_PN : document range to pos num ========================================
	DR_PN <- function(docPos) {
		posNum <- docPos$start[1] + (docPos$start[2] * colFact) %>% round(8)
		as.numeric(posNum)
	}

	PN_DR <- function(posNum_start, posNum_end = posNum_start){
		SRow <- floor(posNum_start)
		SCol <- ((posNum_start - SRow) / colFact) %>% round(8)
		ERow <- floor(posNum_end)
		ECol <- ((posNum_end - ERow) / colFact) %>% round(8)
		document_range(
			start = document_position(SRow, SCol)
			, end = document_position(ERow, ECol)
			)
	}

	# endLine : endLine of a posNum =========
	endLine <- function(posNum){
		ceiling(posNum) - colFact
	}


	# nchar for all three types : ------------------------------------------
	nchars <- function(x, ...){
		vapply(
			c("chars", "bytes", "width")
			, function(tp) nchar(x, tp, ...)
			, integer(length(x))
		)
	}

	# a <- nchars("\u200b")  # in R versions (>= 2015-09-xx):
	# a <- nchars("\u200b" %>% encodeString)  # in R versions (>= 2015-09-xx):
	# a <- nchars("\n")
	# a <- nchars("\n" %>% encodeString)
	# a <- nchars("\n1234567\n" %>% encodeString)
	# ## chars bytes width
	# ##     1     3     0


} # local funs
{
	retainPos <- getPos()
	# retainPos <- PN_DR(1.1) # for tests
	# setCursorPosition(1.1 %>% PN_DP)
	curPosNum <- retainPos$start %>% DP_PN
	retainStartRow <- retainPos$start[1]
	retainStartCol <- retainPos$start[2]
	docContent <- getSourceEditorContext()$contents %>%
		data.frame(content = .) %>%
		tibble::rowid_to_column()
	fnTmr <- timer(fnTmr, endOf = "read Content")
} # read content, get current position

fnTmr <- timer(fnTmr, endOf = "init, funs")
browseOption <- getOption("FAB_browse")
if(!is.null(browseOption)) if(browseOption == 1) browser()
{
	opName <- "+"
	clName <- "-"
	# opBrPatt <- "(?<!\t.{0,80})\\{$"
	# clBrPatt <- "^\\}"
	# opBrPatt <- "^[ \t]*(.*function\\(.*\\))\\ *{$" # new one
	opBrPatt <- "^[ \t]*(.*function\\(.*\\)) *\\{$" # correction
	opBrPatt <- "^[ \t]*(.*function\\(.*\\))? *\\{$" # REcorrection
	# clBrPatt <- "(^|\t)+\\}" # new one
	# clBrPatt <- "^\t*\\}" # again : only tabs before the bracket
	# clBrPatt <- "^\t*\\} #" # 2024.10.10 - only the commented ones
	clBrPatt <- "^\t*\\}(\\))? #" # 2024.11.03 - can also be "})"
	comPatt <- "^( |\t)*#.*"

	# for tests only - en fait pas tant que ca ?
	docContent_tags <- docContent %>%
		mutate(content = content %>% str_remove(comPatt)) %>%
		mutate(
			opBr = content %>% str_detect(opBrPatt)
			, clBr = content %>% str_detect(clBrPatt)
			, anyBr = pmax(opBr, clBr)
			, brTag = paste0(ifelse(opBr, opName, ""), ifelse(clBr, clName, ""))
		) %>%
		identity
	fnTmr <- timer(fnTmr, endOf = "tags")

	docContent_incs <- docContent_tags %>%
		countSwitches("brTag", opName, clName) %>%
		identity
	fnTmr <- timer(fnTmr, endOf = "countSwitches")

	docContentRet <- docContent_incs %>%
		# filter(anyBr == 1) %>%
		# mutate(brPairNb = countSwitches(brTag, opName, clName)) %>%
		# mutate(expected = ceiling(1:n() / 2)) %>%
		as_tibble %>%
		select(-matches("^(brut|inc|check|tst[0-9])|find(Stt|End)|stepstr")) %>%
		mutate(conCat = paste("0", ret1, ret2, ret3, sep = "_")) %>%
		mutate(conCatLim = conCat %>% str_remove_all("_0") %>% paste0("_")) %>%
		mutate(isCur = ifelse(retainStartRow == rowid, "=cur=", "_")) %>%
		# mutate(isCur = isCur * 100) %>%
		group_by(conCatLim) %>%
		mutate(isSecStart = rowid == min(rowid)) %>%
		ungroup %>%
		mutate(opBrPlace = content %>% str_extract(paste0(".*", opBrPatt)) %>% nchar) %>%
		# mutate(opBrPlace = ifelse(rowid == 1, 1, opBrPlace)) %>% # pourquoi ??
		mutate(opBrPN = rowid + opBrPlace * colFact) %>%
		select(-opBrPlace) %>%
		# mutate(opBrPN = opBrPN * 100) %>%
		# print %>%
		identity

	if(debug_getTbl) {
		interm_tbl_debug <- docContentRet %>%
			select(rowid, content, anyBr, brTag, catLvl,
				   conCatLim, isCur, isSecStart, opBrPN)
		return(interm_tbl_debug) # only for debugging
	}
	fnTmr <- timer(fnTmr, endOf = "ret : other treatments")
} # back to docContent normal

{

	curLine <- docContentRet %>% filter(isCur == "=cur=")
	curPosSec <- curLine$conCatLim # init before check
	curPosCat <- curLine$catLvl
	# message("curPosSec : ", curPosSec)
	noBracket <- !str_detect(curLine$content, opBrPatt)
	skipIf <- curPosCat == 0 & noBracket
	if(skipIf) message("noBr - 0")
	if(curLine$isSecStart & !skipIf){

		message("on a startSec line")
		# brPlace <- curLine$content %>%
		# 	str_extract(paste0(".*",opBrPatt)) %>%
		# 	nchar
		isBfSecStart <- curLine$opBrPN >= curPosNum
		isAtSecStart <- curLine$opBrPN == (curPosNum - colFact) %>% round(10)
		if(isBfSecStart){
			# message("sec not started yet")
			curPosSec <- curLine$conCatLim %>% str_remove("_[0-9]*_$")
			curPosCat <- curLine$catLvl - 1
		}
		if(isAtSecStart){
			executeCommand("expandToMatching")
			executeCommand("fold")
			setCursorPosition((curPosNum - colFact) %>% PN_DP)
			return(NULL)
		}
	}
	# message("curPosCat : ", curPosCat)

	# docContentRet %>% filter(rowid %in% c(300:400, 728:735)) %>% print(n = 150)
	curSection <- docContentRet %>%
		filter(conCatLim  %>% str_detect(paste0("^",curPosSec))) %>%
		identity
	subSections <- curSection %>%
		filter(catLvl == curPosCat + 1) %>%
		identity
	subSectionsStarts <- subSections %>%
		group_by(conCatLim) %>%
		slice_min(rowid) %>%
		arrange(rowid) %>%
		identity

	fnTmr <- timer(fnTmr, endOf = "docCont norm")

} # back to docContent normal
{
	onlyOneSec <- F
	docContentRet %>% count(ret1, ret2, ret3)
	if(max(docContentRet$ret1) == 1) onlyOneSec <- TRUE
	fnTmr <- timer(fnTmr, endOf = "check if 1 big")
} # check if only 1 big section
{
	sectionStartLine <- curSection %>%
		# filter(conCatLim == curPosSec) %>%
		slice_min(rowid)

	sectionStart_PN <- sectionStartLine %>%
		pull(opBrPN) %>%
		magrittr::add(colFact)
	if(skipIf) sectionStart_PN <- (1 + colFact) %>% round(8)
	fnTmr <- timer(fnTmr, endOf = "sectionStart line and PN")

} # secStart line and PN

waitOption <- getOption("FAB_wait") # pour decomposer ce que fait la fct
if(is.null(waitOption)) waitOption = 0

subSectionsStarts %>%
	pull(rowid) %>%
	lapply(foldBrLine, waitTime = waitOption) # fold lines
fnTmr <- timer(fnTmr, endOf = "fold")

sectionStart_DP <- sectionStart_PN %>% PN_DP
backToInit <- (onlyOneSec & curPosSec == "0_1")|curPosSec == 0
{
	# message("sectionStart_DP : ")
	# print(sectionStart_DP)
	# message("onlyOneSec : ", onlyOneSec)
	# message("curPosSec : ", curPosSec)
	# message("backToInit : ", backToInit)
} # messages
if(backToInit) {
	setCursorPosition(curPosNum %>% PN_DP)
} else {
	setCursorPosition(sectionStart_DP)
}
fnTmr <- timer(fnTmr, endOf = "put cursor back - end")

if(time){
	# popSize <- nrow(old_pop)
	# timePerM <- sum(fnTmr$dt_seconds/popSize*1E6) %>% round(2)
	timerPlot <- fnTmr %>%
		arrange(-heure_seconds) %>%
		mutate(endOf = factor(endOf, levels = endOf)) %>%
		# mutate(secsPerMLines = dt_seconds / popSize * 1E6) %>%
		# mutate(dt100 = (dt_seconds * 100) %>% floor) %>%
		ggplot(aes(endOf, dt_seconds)) +
		geom_col() +
		theme(axis.text = element_text(size = 12)) +
		geom_text(aes(
			label = dt_seconds %>% round(2)
			, y = pmin(dt_seconds + 0.06, 3)
		)) +
		coord_flip(ylim = c(0,3)) +
		ggtitle(
			paste0("function : ", "foldAllBr")
			, subtitle = paste0("nrows docContent : ", nrow(docContent))
		)
	lum_0_100(50)
	print(timerPlot)
} # timer plots
return(NULL)

}
