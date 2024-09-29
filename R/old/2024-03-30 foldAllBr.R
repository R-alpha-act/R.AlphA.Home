# rm(list = ls())
# fold brackets --------
#' @title _FoldAllBr
#' @description finds open brackets in current code and fold them
#' @importFrom tibble rowid_to_column
#' @importFrom stringr str_detect
#' @import rstudioapi
#' @export

foldAllBr <- function(){
	message("========================================")
	colDiv <- 1E2
# local funs

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
			beforePosNum <- (posNum - 1/colDiv)
			finalPos <- beforePosNum %>% PN_DP
			message("sttInside : ", sttInside, "\t", "endInside : ", endInside)
			message("posNum : ", posNum)
			message("beforePosNum : ", beforePosNum)
		}
		message("finalPos : ", finalPos)
		setCursorPosition(finalPos)
		# return(getPos()) # inutile ce truc ?
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
		col <- (posNum - row) * colDiv
		document_position(row, col)
	}
	# DP_PN : document pos to pos num ==========================================
	DP_PN <- function(docPos) {
		posNum <- docPos[1] + docPos[2] / colDiv
		as.numeric(posNum)
	}

	# DR_PN : document range to pos num ========================================
	DR_PN <- function(docPos) {
		posNum <- docPos$start[1] + docPos$start[2] / colDiv
		as.numeric(posNum)
	}


	# tests en vrac ============================================================
	retainPos <- getPos()
	message("folding BR. Curr pos : ",retainPos$start %>% paste(collapse = "-"))
{
	retainPos <- getPos()
} # test
	{
		retainPos <- getPos()
		{
			# any text
			# ...
			# ...
		} # lvl 3_1
		{
			# any text
			# ...
		} # lvl 3_2
		{
			# any text
			# ...
		} # lvl 3_3
	} # test # lvl 2
	# retainStartRow <- retainPos$start[1]
	# curPosNum <- retainPos$start[1] + retainPos$start[2] / colDiv
	curPosNum <- retainPos$start %>% DP_PN

	# docContent %>% filter(rowid %in% 39:42) %>% as_tibble
	# docContent <- getSourceEditorContext()$contents %>%
	# 	data.frame(content = .) %>%
	# 	tibble::rowid_to_column()
	#
	# opName <- "+"
	# clName <- "-"
	# opBrPatt <- "(?<!\t.{0,80})\\{$"
	# clBrPatt <- "^\\}"
	# opBrPatt <- "\\{$" # new one
	# clBrPatt <- "(^|\t)+\\}" # new one
	# version with tokenize
	docContentTokenize <- rstudioapi::getSourceEditorContext()$path %>%
		sourcetools::tokenize_file() %>%
		countSwitches("value", "{", "}") %>%
		select(-matches("^(brut|inc|check|stepStr)")) %>%
		mutate(conCat = paste("0", ret1, ret2, ret3, sep = "_")) %>%
		mutate(conCatLim = conCat %>% str_remove_all("_0")) %>%
		# mutate(isCur = retainStartRow == rowid) %>%
		# as_tibble %>%
		# filter(row %in% 8:12)
		mutate(posNum = row + column / colDiv) %>%
		mutate(dif = posNum - curPosNum) %>%
		as_tibble %>%
		identity


	# just to check
	# docContentTokenize %>%
	# 	filter(abs(posNum - curPosNum)< 2) %>%
	# 	print(n = 30)



	# # for tests only - en fait pas tant que ca ?
	# docContentRet <- docContent %>%
	# 	mutate(
	# 		opBr = content %>% str_detect("\\{$")
	# 		, clBr = content %>% str_detect("(^|\t)+\\}")
	# 		, anyBr = pmax(opBr, clBr)
	# 		, brTag = paste0(ifelse(opBr, opName, ""), ifelse(clBr, clName, ""))
	# 	) %>%
	# 	countSwitches("brTag", opName, clName) %>%
	# 	# filter(anyBr == 1) %>%
	# 	# mutate(brPairNb = countSwitches(brTag, opName, clName)) %>%
	# 	# mutate(expected = ceiling(1:n() / 2)) %>%
	# 	as_tibble %>%
	# 	select(-matches("^(brut|inc|check)")) %>%
	# 	mutate(conCat = paste(ret1, ret2, ret3, sep = "_")) %>%
	# 	mutate(conCatLim = conCat %>% str_remove_all("_0")) %>%
	# 	mutate(isCur = retainStartRow == rowid) %>%
	# 	identity

	# curLine <- docContentRet %>% filter(rowid == retainStartRow)
	curPlace <- docContentTokenize %>% filter(dif>=0) %>% slice_min(dif)

	# curSection <- docContentRet %>%
	# 	filter(conCatLim  %>% str_detect(paste0("^",curLine$conCatLim)))

	curSection_tok <- docContentTokenize %>%
		filter(conCatLim  %>% str_detect(paste0("^",curPlace$conCatLim)))
	sectionStart_tok <- curSection_tok %>% slice_min(posNum)
	# sectionStart <- curSection %>%
	# 	slice_min(rowid) %>%
	# 	pull(rowid) %>%
	# 	document_position(999)  # a corriger pour avoir le vrai

		# filter(abs(rowid - retainStartRow) < 5)

	areSubSections <- max(curSection_tok$catLvl) > curPlace$catLvl
	isStartOfSection <- sectionStart_tok$posNum == curPlace$posNum
	# areSubSections <- max(curSection$catLvl) > curLine$catLvl
	# isStartOfSection <- min(curSection$rowid) == curLine$rowid

	if (areSubSections) {
		message("there are subsections")
		allSecStarts <- curSection_tok %>%
			filter(catLvl == curPlace$catLvl + 1) %>%
			group_by(conCatLim) %>%
			slice_min(posNum) %>%
			pull(posNum) %>%
			ceiling %>%
			magrittr::add(-1/colDiv) %>%
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
		beforeSecStart <- sectionStart_tok$posNum - 1/colDiv
		setCursorPosition(beforeSecStart %>% PN_DP)
		# setCursorPosition(sectionStart_tok$posNum
		sectionStart_tok$posNum %>% foldBr()
		# curSection$rowid %>% min %>% foldBr()
	} # si deja au debut : on ferme la section




#
#
# 	opBrList <- docContent %>%
# 		filter(content %>% str_detect(opBrPatt)) %>%
# 		pull(rowid)
#
# 	clBrList <- docContent %>% filter(content %>% str_detect(clBrPatt)) # pas utilise ?
#
# 	BrList <- docContent %>%
# 		mutate(
# 			opBr = content %>% str_detect(opBrPatt)
# 			, clBr = content %>% str_detect(clBrPatt)
# 			, anyBr = pmax(opBr, clBr)
# 			, brTag = paste0(ifelse(opBr, "open", ""), ifelse(clBr, "close", ""))
# 		) %>%
# 		filter(anyBr == 1) %>%
# 		mutate(brPairNb = countSwitches(brTag, "open", "close")) %>%
# 		mutate(expected = ceiling(1:n() / 2)) %>%
# 		as_tibble %>%
# 		identity


	# # get the 2 surrounding br infos
	# surrndInfo <- BrList %>%
	# 	mutate(
	# 		posDist = rowid - retainStartRow
	# 		, absPosDist = abs(posDist)
	# 		, signPosDist = sign(posDist)
	# 	) %>%
	# 	group_by(signPosDist) %>%
	# 	slice_min(absPosDist)
	# retainPos
	# surrndInfo
	# surrndInfo$brTag %>% c("open", "close")
#
# 	lastBr <- BrList %>%
# 		mutate(posDist = rowid - retainStartRow) %>%
# 		filter(posDist <0) %>% slice_max(posDist)
#
#
#

#
# 	if (nrow(lastBr)) { if(lastBr$opBr) {
# 		foldBr(lastBr$rowid)
# 		message("this should be printed for single folding")
# 		return(NULL) # stop function if this is done
# 		message("this should not be printed")
# 	}}
#
# 	# if last br is not an opened one then close them all
# 	# fold Br and list their positions
# 	ClosedBr <- opBrList %>% lapply(foldBr)
#
# 	# fun to check if current range is in tested range
# 	isInRange <- function(current, tested){
# 		# tested <- ClosedBr[[1]]
# 		# current <- retainPos
# 		currentLine <- current$start[1]
# 		rowStart <- tested$start[1]
# 		rowEnd <- tested$end[1]
# 		isInside <- currentLine %>% between(rowStart, rowEnd)
# 		if (isInside) {
# 			return(rowStart)
# 		}
# 	}
#
# 	# get the intersections between current range and the closed Br
# 	intersections <- ClosedBr %>%
# 		lapply(isInRange, current = retainPos) %>%
# 		unlist
#
# 	# get cursor back to start (or adjust if there are intersections)
# 	if (intersections %>% is.null){
# 		setCursorPosition(retainPos) # back
# 	} else {
# 		newPos <- document_position(intersections %>% min, 1)
# 		setCursorPosition(newPos)
# 	}
}
