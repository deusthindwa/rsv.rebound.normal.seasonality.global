#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# CREATE OUT OF SEASON STATUS
#================================================================

#decide whether country was out of season in 2021 using onset data and some threshold
climate <-
  climate %>%
  dplyr::left_join(
    onset2 %>% dplyr::mutate(precov = precov*52/(2*pi), wave1 = wave1*52/(2*pi), wave2 = wave2*52/(2*pi), wave3 = wave3*52/(2*pi)) %>%
      dplyr::mutate(outSeasW1 = ifelse(abs(precov-wave1) >8.69, "yes", "no")) %>% #more than 2 months out of season in 2021 = yes
      dplyr::select(country, outSeasW1)) 

#===================================================================================
# CREATE DATASET FOR TIME TO ONSET WEEK DURING FIRST WAVE FROM COVID-19 SUPPRESSION
#===================================================================================

#time to first wave onset from 2020 COVID-19 suppression in April
DSonset1 <-
  onset2x %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  dplyr::filter(wave == "wave1") %>%
  dplyr::mutate(onset = as.integer(difftime(date, as.Date("2020-04-01"), units = "weeks"))) %>%
  dplyr::select(country, onset, clim, hemi, outSeasW1)

DSonset1 <-
  DSonset1 %>% 
  dplyr::arrange(country, clim, hemi, outSeasW1) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = date(seq(as.Date("2020-04-12"), by = "weeks", length.out = n()))) %>% #fix date here to match number of onset weeks
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything())

#combine stringency, population demographics and onset datasets
DSonset1 <-
  DSonset1 %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#create a variable that shows when event happen
DSonset1 <-
  DSonset1 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L),
                event = ifelse(is.na(event), 1L, event)) %>%
  dplyr::select(country, event, everything())

#===================================================================================
# CREATE DATASET FOR TIME TO ONSET WEEK DURING SECOND WAVE FROM FIRST WAVE
#===================================================================================

#time to second wave onset from first wave onset
DSonset2 <- 
  onset2x %>%
  dplyr::select(country, date, wave, loc) %>%
  dplyr::filter(wave != "precov", wave != "wave3") %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::group_by(country) %>%
  dplyr::mutate(date = date(date), onset = as.integer(difftime(lead(date), date, units = "weeks"))+1) %>% #add 1 week to the difference to correct
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(onset)) %>%
  dplyr::select(country, date, onset, clim, hemi, outSeasW1)

DSonset2 <-
  DSonset2 %>% 
  dplyr::arrange(country, clim, hemi, outSeasW1) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country)  %>%
  dplyr::mutate_at(vars(date), funs(replace(., duplicated(.), NA))) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq.Date(from = first(ymd(date)), by = "weeks", length.out = n())) %>% #fix date here to match number of onset weeks
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything(), -date)

#combine stringency and onset datasets
DSonset2 <-
  DSonset2 %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#create a variable that shows when event happen
DSonset2 <-
  DSonset2 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L),
                event = ifelse(is.na(event), 1L, event)) %>%
  dplyr::select(country, event, everything())

#=================================================================================
# CREATE DATASET FOR TIME TO PEAK WEEK DURING FIRST WAVE FROM COVID-19 SUPPRESSION
#=================================================================================

#time to first wave peak from 2020 COVID-19 suppression in April
DSpeak1 <-
  peak2x %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  dplyr::filter(wave == "wave1") %>%
  dplyr::mutate(peak = as.integer(difftime(date, as.Date("2020-04-01"), units = "weeks"))) %>%
  dplyr::select(country, peak, clim, hemi, outSeasW1)

DSpeak1 <-
  DSpeak1 %>% 
  dplyr::arrange(country, clim, hemi, outSeasW1) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = date(seq(as.Date("2020-04-12"), by = "weeks", length.out = n()))) %>% #fix date here to match number of onset weeks
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything())

#combine stringency, population demographics and peak datasets
DSpeak1 <-
  DSpeak1 %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#create a variable that shows when event happen
DSpeak1 <-
  DSpeak1 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L),
                event = ifelse(is.na(event), 1L, event)) %>%
  dplyr::select(country, event, everything())

#========================================================================
# CREATE DATASET FOR TIME TO PEAK WEEK DURING SECOND WAVE FROM FIRST WAVE
#========================================================================

#time to second wave peak from first wave onset
DSpeak2 <- 
  peak2x %>%
  dplyr::select(country, date, wave, loc) %>%
  dplyr::filter(wave != "precov", wave != "wave3") %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::group_by(country) %>%
  dplyr::mutate(date = date(date), peak = as.integer(difftime(lead(date), date, units = "weeks"))+1) %>% #add 1 week to the difference to correct
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(peak)) %>%
  dplyr::select(country, date, peak, clim, hemi, outSeasW1)

DSpeak2 <-
  DSpeak2 %>% 
  dplyr::arrange(country, clim, hemi, outSeasW1) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country)  %>%
  dplyr::mutate_at(vars(date), funs(replace(., duplicated(.), NA))) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq.Date(from = first(ymd(date)), by = "weeks", length.out = n())) %>% #fix date here to match number of onset weeks
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything(), -date)

#combine stringency and peak datasets
DSpeak2 <-
  DSpeak2 %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#create a variable that shows when event happen
DSpeak2 <-
  DSpeak2 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L),
                event = ifelse(is.na(event), 1L, event)) %>%
  dplyr::select(country, event, everything())

#================================================================
# CREATE DATASET FOR GROWTH RATE DURING FIRST WAVE
#================================================================

DSgrowth1 <-
  growth2x %>%
  dplyr::select(country, wave, date, fderiv) %>%
  dplyr::filter(wave == "wave1") %>%
  dplyr::rename("fdate" = "date", "grate" = "fderiv") %>%
  dplyr::mutate(fdate = date(fdate)) %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#================================================================
# CREATE DATASET FOR GROWTH RATE DURING SECOND WAVE
#================================================================

DSgrowth2 <-
  growth2x %>%
  dplyr::select(country, wave, date, fderiv) %>%
  dplyr::filter(wave == "wave2") %>%
  dplyr::rename("fdate" = "date", "grate" = "fderiv") %>%
  dplyr::mutate(fdate = date(fdate)) %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#================================================================
# CREATE DATASET FOR INTENSITY DURING FIRST WAVE
#================================================================

DSintense1 <-
  intense2x %>%
  dplyr::mutate(wave = if_else(wave == "precov", "precov", 
                               if_else(wave == "2020", "wave1",
                                       if_else(wave == "2021", "wave2", "wave3")))) %>%
  
  dplyr::select(country, wave, date, intensity) %>%
  dplyr::filter(wave == "wave1") %>%
  dplyr::rename("fdate" = "date") %>%
  dplyr::mutate(fdate = date(fdate)) %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::mutate(fdate = if_else(fdate == date('2019-10-27'), date('2020-07-12'), fdate)) %>% #reset erroneous date for RSA
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#================================================================
# CREATE DATASET FOR INTENSITY DURING SECOND WAVE
#================================================================

DSintense2 <-
  intense2x %>%
  dplyr::mutate(wave = if_else(wave == "precov", "precov", 
                               if_else(wave == "2020", "wave1",
                                       if_else(wave == "2021", "wave2", "wave3")))) %>%
  
  dplyr::select(country, wave, date, intensity) %>%
  dplyr::filter(wave == "wave2") %>%
  dplyr::rename("fdate" = "date") %>%
  dplyr::mutate(fdate = date(fdate)) %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>% 
  dplyr::left_join(stringency %>% dplyr::mutate(fdate = date(fdate)))

#================================================================
# ADD VARIABLE TO CAPTURE THE EFFECT OF INTENSITY
#================================================================

#Does intensity in 2021 influence onset in 2022 
DSonset2 <- 
  DSonset2 %>% 
  dplyr::left_join(
    DSintense1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intenseW1" = "intensity"))

#Does intensity in 2021 influence peak in 2022 
DSpeak2 <- 
  DSpeak2 %>% 
  dplyr::left_join(
    DSintense1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intenseW1" = "intensity"))

#Does intensity in 2021 influence growth in 2022 
DSgrowth2 <- 
  DSgrowth2 %>% 
  dplyr::left_join(
    DSintense1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intenseW1" = "intensity"))

#Does intensity in 2021 influence intensity in 2022
DSintense2 <- 
  DSintense2 %>% 
  dplyr::left_join(
    DSintense1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intenseW1" = "intensity"))

#================================================================
# UNIVARIATE REGRESSION ANALYSES
#================================================================

#onset timing during first wave
DSonset1 <- 
  DSonset1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), cluster = country, data = DSonset1, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X1 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "First wave onset timing") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#onset timing during second wave from first wave
DSonset2 <- 
  DSonset2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)", "outSeasW1x", "scale(intenseW1)")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X2 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Second wave onset timing") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#peak timing during first wave
DSpeak1 <- 
  DSpeak1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X3 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "First wave peak timing") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#peak timing during second wave from first wave
DSpeak2 <- 
  DSpeak2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)", "outSeasW1x", "scale(intenseW1)")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X4 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Second wave peak timing") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#growth rate during first wave
DSgrowth1 <- 
  DSgrowth1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("grate ~", i)), data = DSgrowth1), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X5 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "First wave growth rate") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#growth rate during second wave
DSgrowth2 <- 
  DSgrowth2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)", "outSeasW1x", "scale(intenseW1)")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("grate ~", i)), data = DSgrowth2), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X6 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Second wave growth rate") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#intensity during the first wave
DSintense1 <- 
  DSintense1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity ~", i)), data = DSintense1), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X7 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "First wave intensity") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#intensity during the second wave
DSintense2 <- 
  DSintense2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim2 = as.factor(clim),
         outSeasW1x = as.factor(outSeasW1))

X <- list()
for (i in c("hemi2", "clim2", "scale(strin_indxL)", "scale(pop_dens)", "outSeasW1x", "scale(intenseW1)")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity ~", i)), data = DSintense2), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X8 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Second wave intensity") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)

#================================================================
# MULTIVARIATE REGRESSION ANALYSES
#================================================================

#onset timing during second wave from first wave
for (i in c("hemi2", "scale(strin_indxL)", "scale(pop_dens)", "outSeasW1x", "scale(intenseW1)")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}
for (i in c("hemi2 + scale(strin_indxL)", "hemi2 + scale(pop_dens)", "hemi2 + outSeasW1x", "hemi2 + scale(intenseW1)")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}
for (i in c("hemi2 + scale(pop_dens) + scale(strin_indxL)", "hemi2 + scale(pop_dens) + outSeasW1x", "hemi2 + scale(pop_dens) + scale(intenseW1)")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}
for (i in c("hemi2 + scale(pop_dens) + scale(intenseW1) + scale(strin_indxL)", "hemi2 + scale(pop_dens) + scale(intenseW1) + outSeasW1x")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}
for (i in c("hemi2 + scale(pop_dens) + scale(intenseW1) + scale(strin_indxL) + outSeasW1x" )) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}
Y2 = coxph(Surv(time, event) ~  hemi2 + scale(strin_indxL) + scale(pop_dens) + scale(intenseW1), 
           data = DSonset2, 
           control = coxph.control(iter.max = 1000))
Y2 <- 
  tidy(Y2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
  dplyr::mutate(ds = "Second wave onset timing")


Y3 = coxph(Surv(time, event) ~  hemi2 + scale(strin_indxL) + scale(pop_dens) + scale(intenseW1) + outSeasW1x, #adjust for out of season seperately
           data = DSonset2, 
           control = coxph.control(iter.max = 1000))
Y3 <- 
  tidy(Y3, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
  dplyr::mutate(ds = "Second wave onset timing")

#peak timing during second wave from first wave
for (i in c("hemi2", "clim2", "scale(strin_indxL)")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}
for (i in c("scale(strin_indxL) + hemi2", "scale(strin_indxL) + clim2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}
for (i in c("scale(strin_indxL) + hemi2 + clim2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

Y4 = coxph(Surv(time, event) ~ hemi2 + clim2 + scale(strin_indxL), 
           data = DSpeak2, 
           control = coxph.control(iter.max = 1000))
Y4 <- 
  tidy(Y4, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
  dplyr::mutate(ds = "Second wave peak timing")
