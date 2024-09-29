# rm(list = ls())
# fold brackets --------
#' @title _FoldAllBr
#' @description finds open brackets in current code and fold them
#' @importFrom tibble rowid_to_column
#' @importFrom stringr str_detect
#' @import rstudioapi
#' @export

foldAllBr <- function(){


fnTmr <- timer(start = T, endOf = "start")
# message("========================================")
# colDiv <- 1E2
colFact <- 1E-2
{
	# foldBr : given a line, fold the bracket ending it ========================
	foldBr <- function(posNum){
		# posNum <- 15.30
		# opLine <- opBrList[2]
		savePos <- getPos()
		# setCursorPosition(document_position(opLine, 999))
		setCursorPosition(posNum %>% PN_DP)
		executeCommand("expandToMatching")

		# checking if after expanding
		brRange <- getPos()
		brRangePN <- brRange %>% lapply(DP_PN)
		SPRangePN <- savePos %>% lapply(DP_PN)
		sttInside <- SPRangePN$start %>% between(brRangePN$start, brRangePN$end)
		endInside <- SPRangePN$end %>% between(brRangePN$start, brRangePN$end)

		executeCommand("fold")


		# si dedans : on remet au debut de la section qu'on a ferme
		finalPos <- savePos
		if(sttInside|endInside) {
			beforePosNum <- (posNum - colFact)
			finalPos <- beforePosNum %>% PN_DP
			message("sttInside : ", sttInside, "\t", "endInside : ", endInside)
			message("posNum : ", posNum)
			message("beforePosNum : ", beforePosNum)
		}
		message("finalPos : ", finalPos)
		setCursorPosition(finalPos)
		# return(getPos()) # inutile ce truc ?
	}


	# foldBrLine : given a line, fold the bracket ending it ====================
	foldBrLine <- function(opLine){
		# opLine <- opBrList[2]
		setCursorPosition(document_position(opLine, 999))
		executeCommand("expandToMatching")
		executeCommand("fold")
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
# tests en vrac ============================================================
retainPos <- getPos()
curPosNum <- retainPos$start %>% DP_PN
# message("folding BR. Curr pos : ",retainPos$start %>% paste(collapse = "-"))
# message("curr pos num : ", curPosNum)

{
	docContentTokenize <- rstudioapi::getSourceEditorContext()$path %>%
		sourcetools::tokenize_file() %>%
		countSwitches("value", "{", "}") %>%
		select(-matches("^(brut|inc|check|stepStr)")) %>%
		mutate(conCat = paste("0", ret1, ret2, ret3, sep = "_")) %>%
		mutate(conCatLim = conCat %>% str_remove_all("_0")) %>%
		# mutate(isCur = retainStartRow == rowid) %>%
		# as_tibble %>%
		# filter(row %in% 8:12)
		mutate(posNum = row + column * colFact) %>%
		mutate(dif = posNum - curPosNum) %>%
		mutate(rowDif = floor(posNum) - floor(curPosNum)) %>% # idem
		as_tibble %>%
		identity
} # docContentTokenize
{
	docContent <- getSourceEditorContext()$contents %>%
		data.frame(content = .) %>%
		tibble::rowid_to_column()


	retainStartRow <- retainPos$start[1]
	retainStartCol <- retainPos$start[2]
	opName <- "+"
	clName <- "-"
	# opBrPatt <- "(?<!\t.{0,80})\\{$"
	# clBrPatt <- "^\\}"
	opBrPatt <- "\\{$" # new one
	# clBrPatt <- "(^|\t)+\\}" # new one
	clBrPatt <- "^\t*\\}" # again : only tabs before the bracket
	# "		}" %>% str_detect(clBrPatt)
	# "m	}" %>% str_detect(clBrPatt)

	# for tests only - en fait pas tant que ca ?
	docContentRet <- docContent %>%
		mutate(
			opBr = content %>% str_detect(opBrPatt)
			, clBr = content %>% str_detect(clBrPatt)
			, anyBr = pmax(opBr, clBr)
			, brTag = paste0(ifelse(opBr, opName, ""), ifelse(clBr, clName, ""))
		) %>%
		countSwitches("brTag", opName, clName) %>%
		# filter(anyBr == 1) %>%
		# mutate(brPairNb = countSwitches(brTag, opName, clName)) %>%
		# mutate(expected = ceiling(1:n() / 2)) %>%
		as_tibble %>%
		select(-matches("^(brut|inc|check)")) %>%
		mutate(conCat = paste("0", ret1, ret2, ret3, sep = "_")) %>%
		mutate(conCatLim = conCat %>% str_remove_all("_0")) %>%
		mutate(isCur = ifelse(retainStartRow == rowid, "=cur=", "_")) %>%
		# mutate(isCur = isCur * 100) %>%
		group_by(conCatLim) %>%
		mutate(isSecStart = rowid == min(rowid)) %>%
		ungroup %>%
		mutate(opBrPlace = content %>% str_extract(paste0(".*", opBrPatt)) %>% nchar) %>%
		mutate(opBrPlace = ifelse(rowid == 1, 1, opBrPlace)) %>%
		mutate(opBrPN = rowid + opBrPlace * colFact) %>%
		# mutate(opBrPN = opBrPN * 100) %>%
		identity %>%
		identity


	curLine <- docContentRet %>% filter(isCur == "=cur=")
	curPosSec <- curLine$conCatLim # init before check
	curPosCat <- curLine$catLvl
	if(curLine$isSecStart){
		retainStartCol
		# brPlace <- curLine$content %>%
		# 	str_extract(paste0(".*",opBrPatt)) %>%
		# 	nchar
		isBfSecStart <- curLine$opBrPN >= curPosNum
		isAtSecStart <- curLine$opBrPN == (curPosNum - colFact) %>% round(10)
		if(isBfSecStart){
			# message("sec not started yet")
			curPosSec <- curLine$conCatLim %>% str_remove("_[0-9]*$")
			curPosCat <- curLine$catLvl - 1
		}
		if(isAtSecStart){
			executeCommand("expandToMatching")
			executeCommand("fold")
			setCursorPosition((curPosNum - colFact) %>% PN_DP)
			return(NULL)
		}
	}
	# message("curPosSec : ", curPosSec)
	# message("curPosCat : ", curPosCat)

	curSection <- docContentRet %>%
		filter(conCatLim  %>% str_detect(paste0("^",curPosSec))) %>%
		identity
	subSections <- curSection %>%
		filter(catLvl == curPosCat + 1) %>%
		identity
	subSectionsStarts <- subSections %>%
		group_by(conCatLim) %>%
		slice_min(rowid) %>%
		identity


} # back to docContent normal
onlyOneSec <- F
{
	docContentRet %>% count(ret1, ret2, ret3)
	if(max(docContentRet$ret1) == 1) onlyOneSec <- TRUE
} # check if only 1 big section
sectionStartLine <- docContentRet %>%
	filter(conCatLim == curPosSec) %>%
	slice_min(rowid)

sectionStart_PN <- sectionStartLine %>%
	pull(opBrPN) %>%
	magrittr::add(colFact)

# message("sectionStartLine")
print(sectionStartLine)

# message("sectionStart_PN : ", sectionStart_PN)


subSectionsStarts %>% pull(rowid) %>% lapply(foldBrLine)

sectionStart_DP <- sectionStart_PN %>% PN_DP
# message("sectionStart_DP : ")
# print(sectionStart_DP)
# message("onlyOneSec : ", onlyOneSec)
# message("curPosSec : ", curPosSec)
backToInit <- (onlyOneSec & curPosSec == "0_1")|curPosSec == 0
# message("backToInit : ", backToInit)
if(backToInit) {
	setCursorPosition(curPosNum %>% PN_DP)
} else {
	setCursorPosition(sectionStart_DP)
}

fnTmr <- timer(fnTmr, endOf = "end")
print(fnTmr)
# message("end.")
return(NULL)
message("should not be printed")
docContentTokenize %>% filter(row == 81)
curPosNum
curPlace <- docContentTokenize %>% filter(dif>=0) %>% slice_min(dif)
curSection_tok <- docContentTokenize %>%
	filter(conCatLim  %>% str_detect(paste0("^",curPlace$conCatLim)))
sectionStart_tok <- curSection_tok %>% slice_min(posNum)
areSubSections <- max(curSection_tok$catLvl) > curPlace$catLvl
isStartOfSection <- sectionStart_tok$posNum == curPlace$posNum

# close all subSect (lvl > current one)
if (areSubSections) {
	message("closing all subs")
	allSecStarts <- curSection_tok %>%
		filter(catLvl == curPlace$catLvl + 1) %>%
		group_by(conCatLim) %>%
		slice_min(posNum) %>%
		# pull(posNum) %>%
		# ceiling %>%
		# magrittr::add(-colFact) %>%
		identity
	allSecStarts %>% lapply(foldBr)
	# foldBr(15.99)
	setCursorPosition(sectionStart_tok$posNum %>% PN_DP)
} # close all subsections, cursor at start
if (isStartOfSection) {
	message("cursor is at start of section")
	setCursorPosition(sectionStart_tok$posNum %>% PN_DP)
	# realSecStart <- getPos()
	# SecStartRow <- realSecStart$start[1]
	# SecStartCol <- realSecStart$start[2]
	# beforeSecStart <- document_position(SecStartRow, SecStartCol-1)
	beforeSecStart <- sectionStart_tok$posNum - colFact
	setCursorPosition(beforeSecStart %>% PN_DP)
	# setCursorPosition(sectionStart_tok$posNum
	sectionStart_tok$posNum %>% foldBr()
	# curSection$rowid %>% min %>% foldBr()
} # si deja au debut : on ferme la section

}
