#' @title Left Join with Validation Checks
#' @description Performs a left join and verifies that no unexpected duplicates
#' or mismatches occur.
#' @param x A data.table representing the left table.
#' @param y A data.table representing the right table.
#' @param ... Additional arguments passed to `dplyr::left_join`.
#' @param req_preserved_x Logical. Ensure that the number of rows in `x`
#' remains unchanged after the join. Default: TRUE.
#' @param req_xAllMatch Logical. Ensure that all rows in `x` find a match in `y`. Default: FALSE.
#' @param behavior Character. Specifies behavior if validation fails.
#' Options: `"warning"` or `"error"`. (default: `"warning"`)
#' @param showNotFound Logical. Show rows from `x` that did not match with `y`.
#' Default: FALSE.
#' @return A data.table containing the joined table.
#' @importFrom tibble rowid_to_column rownames_to_column
#' @importFrom tidyr replace_na
#' @importFrom stringr str_remove
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' @export
#'
left_join_checks <- function(
		x
		, y
		, ...
		, req_xAllMatch = 1
		, req_preserved_x = 1
		, behavior = "error"
		, showNotFound = F
		, time = F
){
	manualrun <- T
	manualrun <- F
	if (manualrun) {
		rm(list = ls())
		R.AlphA.Base::getLibsR.AlphA()
		warning("! parameters manually defined inside function 'left_join_checks'",
				"for tests. Do not use results !")
		workRRoot <- root() %>% str_extract(".*WorkR")
		tbls <- workRRoot %>%
			file.path("pop stats", "ex_working_folder", "INPUTS", "ret") %>%
			list.files(pattern = "IPTables.rds", full.names = T) %>%
			print %>%
			readRDS
		x <- data.table(a = 1, age = c(1:4)) %>% print
		y <- data.table(a = 1, age = c(2:4, 4), result = "ok") %>% print

		x <- generate_pop(2E5, age_min = 30, age_max = 36) %>%
			complete_pop
		y <- tbls$STMRes$t_vie
		req_xAllMatch = 1
		req_preserved_x = 1
		req_yNotFound = 0
		time = T
	}
	fnTmr <- timer(start = T, endOf = "Start --")
	# a voir plus tard - verif que pas de vars deja avec ljc_
	# R.AlphA::compareVars(x, y, pattern = "ljc_")

	# preparation pour merge
	xMerge <- x %>% rowid_to_column(var = "ljc_xID") %>% mutate(ljc_inX = 1)
	yMerge <- y %>% rowid_to_column(var = "ljc_yID") %>% mutate(ljc_inY = 1)
	fnTmr <- timer(fnTmr, endOf = "indexes - inX, inY")

	# merge
	joinXY <- left_join(
		xMerge
		, yMerge
		, ...
	) %>% replace_na(list(ljc_inX = 0, ljc_inY = 0))
	fnTmr <- timer(fnTmr, endOf = "join itself")

	# check
	xMerge;yMerge;joinXY
	fnTmr <- timer(fnTmr, endOf = "only calling")

	chk_preserved_x <- all.equal(joinXY$ljc_xID, xMerge$ljc_xID) %>% isTRUE
	fnTmr <- timer(fnTmr, endOf = "chk_preserved_x")
	chk_dups_x <- duplicated(joinXY$ljc_xID) %>% sum
	fnTmr <- timer(fnTmr, endOf = "chk_dups_x")
	chk_preserved_y <- all.equal(joinXY$ljc_yID, yMerge$ljc_yID) %>% isTRUE
	fnTmr <- timer(fnTmr, endOf = "chk_preserved_y")
	chk_dups_y <- duplicated(joinXY$ljc_yID) %>% sum
	fnTmr <- timer(fnTmr, endOf = "chk_dups_y")
	joinMatch_prep <- joinXY %>% count(ljc_inX, ljc_inY)
	fnTmr <- timer(fnTmr, endOf = "joinMatch_prep")
	joinMatch <- expand.grid(ljc_inX = 0:1, ljc_inY = 0:1) %>%
		left_join(joinMatch_prep) %>%
		suppressMessages %>%
		replace_na(list(n=0))
	chk_yNotFound <- joinMatch %>% filter(!ljc_inY) %>% pull(n) %>% sum
	chk_xAllMatch <- chk_yNotFound == 0
	fnTmr <- timer(fnTmr, endOf = "chk_*")

	chk_preserved_x
	chk_preserved_y
	chk_dups_x
	chk_dups_y
	chk_xAllMatch
	chk_yNotFound
	fnTmr <- timer(fnTmr, endOf = "more calls")

	valuesTable <- mget(ls(pattern = "^chk_")) %>%
		unlist %>%
		data.frame(value = .) %>%
		rownames_to_column("check") %>%
		mutate(key = str_remove(check, "^chk_|^req_"))

	reqsTable <- mget(ls(pattern = "^req_")) %>%
		unlist %>%
		data.frame(req = .) %>%
		rownames_to_column("check") %>%
		mutate(req = req %>% as.numeric) %>%
		mutate(key = str_remove(check, "^chk_|^req_"))

	checksTable <- full_join(valuesTable, reqsTable, by = "key") %>%
		mutate(is_problem = value != req) %>%
		select(key, value, req, is_problem) %>%
		mutate(is_problem = is_problem %>% as.numeric) %>%
		replace_na(list(is_problem = 0)) %>%
		arrange(is_problem %>% desc, req %>% desc)

	nbPbs <- sum(checksTable$is_problem, na.rm = T)

	if (nbPbs) {
		commonMsg <- paste0(nbPbs, " problem(s) during merge",
							"\nsee report for details")

		if (showNotFound & !chk_xAllMatch) {
			print(joinXY %>% filter(!ljc_inY))
		}

		if (behavior == "warning") {
			print(checksTable)
			warning(commonMsg)
		} else if (behavior == "error") {
			print(checksTable)
			stop(commonMsg)
		}
	}
	fnTmr <- timer(fnTmr, endOf = "counting problems")

	joinXY_select <- joinXY %>% select(-starts_with("ljc_"))
	fnTmr <- timer(fnTmr, endOf = "select")
	# timer plots
	if(time){
		xSize <- nrow(x)
		timePerM <- sum(fnTmr$dt_seconds/xSize*1E6) %>% round(2)
		timerPlot <- fnTmr %>%
			arrange(-heure_seconds) %>%
			mutate(endOf = factor(endOf, levels = endOf)) %>%
			mutate(secsPerMLines = dt_seconds / xSize * 1E6) %>%
			ggplot(aes(endOf, secsPerMLines)) +
			geom_col() +
			theme(axis.text = element_text(size = 12)) +
			geom_text(aes(
				label = secsPerMLines %>% round(2)
				, y = pmin(secsPerMLines + 0.06, 3)
			)) +
			coord_flip(ylim = c(0,3)) +
			ggtitle(
				paste0("function : ", "left_join_checks")
				, paste0(
					"total time per M: ", timePerM, " s"
					, "   -   "
					, "xSize : ", xSize %>% sepThsd
				)
			)
		lum_0_100(60)
		print(timerPlot)
	}

	# return joined table but the intermediary columns
	return(joinXY_select)
}
