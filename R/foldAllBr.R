# rm(list = ls())
# fold brackets --------
#' @title _FoldAllBr
#' @description finds open brackets in current code and fold them
#' @importFrom tibble rowid_to_column
#' @importFrom stringr str_detect
#' @import rstudioapi
#' @export

foldAllBr <- function(){


	# R.AlphA::getLibsR.AlphA()
	getPos <- function(){
		getSourceEditorContext() %>%
			primary_selection() %>%
			getElement("range")
	}

	retainPos <- getPos()
{
	retainPos <- getPos()
} # test
	retainStartRow <- retainPos$start[1]


	docContent <- getSourceEditorContext()$contents %>%
		data.frame(content = .) %>%
		tibble::rowid_to_column()
	opBrList <- docContent %>%
		filter(content %>% str_detect("(?<!\t.{0,80})\\{$")) %>%
		# print %>%
		pull(rowid)

	clBrList <- docContent %>%
		filter(content %>% str_detect("^\\}"))

	BrList <- docContent %>%
		mutate(
			opBr = content %>% str_detect("(?<!\t.{0,80})\\{$")
			, clBr = content %>% str_detect("^\\}")
			, anyBr = pmax(opBr, clBr)
			, brTag = paste0(ifelse(opBr, "open", ""), ifelse(clBr, "close", ""))
		) %>%
		filter(anyBr == 1) %>%
		mutate(brPairNb = countSwitches(brTag, "open", "close")) %>%
		mutate(expected = ceiling(1:n() / 2)) %>%
		as_tibble %>%
		identity


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

	lastBr <- BrList %>%
		mutate(posDist = rowid - retainStartRow) %>%
		filter(posDist <0) %>% slice_max(posDist)




	foldBr <- function(opLine){
		# opLine <- opBrList[2]
		setCursorPosition(document_position(opLine, 999))
		executeCommand("expandToMatching")
		executeCommand("fold")
		return(getPos())
	}


	if (nrow(lastBr)) { if(lastBr$opBr) {
		foldBr(lastBr$rowid)
		# message("this should be printed for single folding")
		return(NULL) # stop function if this is done
		message("this should not be printed")
	}}

	# if last br is not an opened one then close them all
	# fold Br and list their positions
	ClosedBr <- opBrList %>% lapply(foldBr)

	# fun to check if current range is in tested range
	isInRange <- function(current, tested){
		# tested <- ClosedBr[[1]]
		# current <- retainPos
		currentLine <- current$start[1]
		rowStart <- tested$start[1]
		rowEnd <- tested$end[1]
		isInside <- currentLine %>% between(rowStart, rowEnd)
		if (isInside) {
			return(rowStart)
		}
	}

	# get the intersections between current range and the closed Br
	intersections <- ClosedBr %>%
		lapply(isInRange, current = retainPos) %>%
		unlist

	# get cursor back to start (or adjust if there are intersections)
	if (intersections %>% is.null){
		setCursorPosition(retainPos) # back
	} else {
		newPos <- document_position(intersections %>% min, 1)
		setCursorPosition(newPos)
	}
}
