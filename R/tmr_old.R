# tmr ---------------------------------------------------------------------
tmr <- function(tmTbl, message = F, ...){
	time_interm <- data.table(tmrDateTime = now(), ...)
	rbind(tmTbl, time_interm, fill = TRUE)
}


# compute -----------------------------------------------------------------
tmr_compute <- function(tmTbl, starts = TRUE){
	tmTbl %>%
		arrange(tmrDateTime) %>%
		mutate(
			tmrPrevTime = lag(tmrDateTime)
			, tmrNextTime = lead(tmrDateTime)
			, tmrDtToPrev = tmrDateTime - tmrPrevTime
			, tmrDtToNext = tmrNextTime - tmrDateTime
			, tmrDt = if(starts) tmrDtToNext else tmrDtToPrev
		) %>%
		select(
			- tmrPrevTime, -tmrDtToNext, -tmrDtToPrev, -tmrNextTime
		)
}


# vis ? -------------------------------------------------------------------
tmr_vis <- function(tmTblComputed){
	tmTblComputed %>%
		mutate(
			NULL
			, tmrDtPeriod = as.period(tmrDt)
			, tmrDate = date(tmrDateTime)
			, tmrHour2 = format(tmrDateTime, "%H:%M:%S")
			, test = readable_period(tmrDtPeriod)
		) %>%
		select(
			-tmrDtPeriod
			, - tmrDateTime
			, - tmrDt
			# , - tmrDtPeriod
			# , tmrDate
		)

}

# tmr_init() %>% tmr %>% tmr_compute %>% tmr_vis


# tests -------------------------------------------------------------------
#
# tmr_vis(tmTbl %>% tmr_compute)
# max(tmTbl$tmrDateTime) - min(tmTbl$tmrDateTime)
#
# tmTblComputed <- tmTbl %>% tmr_compute
# test <- tmTblComputed$tmrDt[1] * c(1E2,1E6, 1E8);test
# testPeriod <- as.period(test);testPeriod
# readable_period(testPeriod)
#
#
# for (step in 1:1E4) {
# 	if(step == 1) tmTblNew <- tmr_init()
# 	tmTblNew <- tmr(tmTblNew, step = step)
# }
#
# for (step in 1:1E4) {
# 	if(step == 1) tmTblOld <- timer(start = TRUE)
# 	tmTblOld <- timer(tmTblOld, step = step)
# }
#
# tstApplyGen <- lapply(1:1E4, function(x) {
# 	if(x ==1) tstApply <<- tmr_init()
# 	tstApply <<- tmr(tstApply, step = x)
# })
#
# tstApply <- tstApply %>%
# 	tmr_compute() %>%
# 	mutate(dt_seconds = tmrDt %>% as.numeric) %>%
# 	mutate(type = "apply")
#
# tmTblNew <- tmTblNew %>%
# 	tmr_compute() %>%
# 	mutate(dt_seconds = tmrDt %>% as.numeric) %>%
# 	mutate(type = "new")
# tmTblOld <- tmTblOld %>%
# 	mutate(type = "old")
# bothTbl <- rbind(
# 	tmTblNew
# 	, tmTblOld
# 	, tstApply
# 	, fill = T)
# bothTbl %>% filter(dt_seconds > 1E-2) %>%
# 	as_tibble %>%
# 	print(n = 30)
#
# # pas de grand changement entre for et apply. Mais nouveau 3 fois + rapide
# bothTbl %>%
# 	ggplot(aes(step, dt_seconds, colour = type)) +
# 	geom_line()+
# 	coord_cartesian(ylim = c(0,0.0012))
#
#
# lum_0_100(60)
# max(tmTblOld$heure) - min(tmTblOld$heure)
# library(ggplot2)
# ggplot(tmTblOld, aes(step, dt_seconds))+
# 	geom_line()+
# 	coord_cartesian(ylim = c(0,0.004))
#
# t1 <- tmTbl$tmrDateTime[7]
# t2 <- tmTbl$tmrDateTime[8]
#
# tdiff <- t2 - t1
#
# microseconds(tdiff)
#
#
#
