 #Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#MAKE ALL RSV ONSET/PEAK/GROWTH/INTENSITY CORRELATION PLOTS
#====================================================================

ggsave(here("output", "fig3_corHemisphere.png"),
       plot = ((A1/A2) | (B1/B2))/((C1/C2) | (D1/D2)),
       width = 22, height = 18, unit = "in", dpi = 300)

ggsave(here("output", "sfig7_corClimatezone.png"),
       plot = ((A3/A4) | (B3/B4))/((C3/C4) | (D3/D4)),
       width = 22, height = 18, unit = "in", dpi = 300)

#================================================================
# OUT OF SEASON STATUS
#================================================================

#decide whether country was out of season in 2021 using onset data and some threshold
climate <-
  climate %>%
  dplyr::left_join(
    rsv_onset %>%
      dplyr::mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = covper, values_from = epiwk) %>%
      dplyr::select(everything(), -row) %>%
      dplyr::group_by(country) %>%
      tidyr::fill(precov, .direction = "up") %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(precov), !is.na(y2021)) %>%
      dplyr::mutate(out_seas21 = ifelse(abs(precov-y2021) >8.69, "yes", "no")) %>% #more than 2 months out of season in 2021 = yes
      dplyr::select(country, out_seas21)) 

#================================================================
# CREATE DATASET FOR TIME TO ONSET WEEK DURING FIRST WAVE
#================================================================

#time to first onset in 2021 from 2020 COVID-19 suppression in July
DSonset1 <-
rsv_onset %>%
  dplyr::select(country, covper, epiwk, clim_zone, hemi) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper == "y2021") %>%
  dplyr::mutate(covper = str_sub(covper, 2, 5),
                epiwk = round(epiwk, digits = 0),
                onset = as.Date(paste(covper, epiwk, 1, sep="-"), "%Y-%W-%u"),
                onset = as.integer(difftime(onset, as.Date("2020-07-01"), units = "weeks")),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::select(country, onset, clim_zone, hemi, out_seas21) %>%
  dplyr::filter(clim_zone != "")

DSonset1 <-
  DSonset1 %>% 
  dplyr::filter(!is.na(onset)) %>%
  dplyr::arrange(country, clim_zone, hemi, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq(as.Date("2020-07-13"), by = "weeks", length.out = n())) %>% #fix date here to match number of onset weeks
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything())

#combine stringency and onset datasets
DSonset1 <-
  DSonset1 %>% 
  dplyr::left_join(stringency)

#create a variable that shows when event happen
DSonset1 <-
DSonset1 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L)) %>%
  dplyr::select(country, event, everything())

#================================================================
# CREATE DATASET FOR TIME TO ONSET WEEK DURING SECOND WAVE
#================================================================

#time to second onset post-COVID-19 from 2021 COVID-19 suppression
DSonset2 <- 
  rsv_onset %>%
  dplyr::select(country, covper, epiwk, clim_zone, hemi) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper != "precov") %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  dplyr::mutate(y2021 = round(y2021, digits = 0),
                y2022 = round(y2022, digits = 0),
                y2021 = as.Date(paste("2021", y2021, 1, sep="-"), "%Y-%W-%u"),
                y2022 = as.Date(paste("2022", y2022, 1, sep="-"), "%Y-%W-%u"),
                onset = as.integer(difftime(y2022, y2021, units = "weeks")),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::select(country, onset, clim_zone, hemi, out_seas21, y2021) %>%
  dplyr::filter(clim_zone != "")

DSonset2 <-
  DSonset2 %>% 
  dplyr::filter(!is.na(onset)) %>%
  dplyr::arrange(country, clim_zone, hemi, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                y2021 = ymd(date(y2021)),
                y2021 = if_else(time == 1, y2021 %m+% period("1 week"), NA_Date_),
                fdate = seq((y2021[1]), length.out = n(), by = "weeks")) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything(), -y2021)

#combine stringency and onset datasets
DSonset2 <-
  DSonset2 %>% 
  dplyr::left_join(stringency)

#create a variable that shows when event happen
DSonset2 <-
  DSonset2 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L)) %>%
  dplyr::select(country, event, everything())

#================================================================
# CREATE DATASET FOR TIME TO PEAK WEEK FIRST WAVE
#================================================================

#time to first onset in 2021 from 2020 COVID-19 suppression in July
DSpeak1 <-
  rsv_peak2 %>%
  dplyr::select(country, covper, difloc, clim_zone, hemi) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper == "y2021") %>%
  dplyr::mutate(covper = str_sub(covper, 2, 5),
                difloc = round(difloc, digits = 0),
                peak = as.Date(paste(covper, difloc, 1, sep="-"), "%Y-%W-%u"),
                peak = as.integer(difftime(peak, as.Date("2020-07-01"), units = "weeks")),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::select(country, peak, clim_zone, hemi, out_seas21) %>%
  dplyr::filter(clim_zone != "")

DSpeak1 <-
  DSpeak1 %>% 
  dplyr::filter(!is.na(peak)) %>%
  dplyr::arrange(country, clim_zone, hemi, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq(as.Date("2020-07-13"), by = "weeks", length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything())

#combine stringency and onset datasets
DSpeak1 <-
  DSpeak1 %>% 
  dplyr::left_join(stringency)

#create a variable that shows when event happen
DSpeak1 <-
  DSpeak1 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L)) %>%
  dplyr::select(country, event, everything())

#================================================================
# CREATE DATASET FOR TIME TO PEAK WEEK DURING SECOND WAVE
#================================================================

#time to second onset post-COVID-19 from 2021 COVID-19 suppression
DSpeak2 <- 
  rsv_peak2 %>%
  dplyr::select(country, covper, difloc, clim_zone, hemi) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper != "precov") %>%
  pivot_wider(names_from = covper, values_from = difloc) %>%
  dplyr::mutate(y2021 = round(y2021, digits = 0),
                y2022 = round(y2022, digits = 0),
                y2021 = as.Date(paste("2021", y2021, 1, sep="-"), "%Y-%W-%u"),
                y2022 = as.Date(paste("2022", y2022, 1, sep="-"), "%Y-%W-%u"),
                peak = as.integer(difftime(y2022, y2021, units = "weeks")),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::select(country, peak, clim_zone, hemi, out_seas21, y2021) %>%
  dplyr::filter(clim_zone != "")

DSpeak2 <-
  DSpeak2 %>% 
  dplyr::filter(!is.na(peak)) %>%
  dplyr::arrange(country, clim_zone, hemi, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                y2021 = ymd(date(y2021)),
                y2021 = if_else(time == 1, y2021 %m+% period("1 week"), NA_Date_),
                fdate = seq((y2021[1]), length.out = n(), by = "weeks")) %>%
  dplyr::ungroup() %>%
  dplyr::select(country, time, fdate, everything(), -y2021)

#combine stringency and onset datasets
DSpeak2 <-
  DSpeak2 %>% 
  dplyr::left_join(stringency)

#create a variable that shows when event happen
DSpeak2 <-
  DSpeak2 %>%
  dplyr::mutate(event = ifelse(time+1 == lead(time), 0L, 1L)) %>%
  dplyr::select(country, event, everything())

#================================================================
# CREATE DATASET FOR GROWTH RATE DURING FIRST WAVE
#================================================================

#create dataset for regression
DSgrowth1 <-
  rsv_growth2 %>%
  dplyr::filter(covper == "y2021") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale, -region, -latitude)

#combine stringency and onset datasets
DSgrowth1 <-
  DSgrowth1 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = median(strin_indx),
                pop_dens = median(pop_dens),
                med_age = median(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR GROWTH RATE DURING SECOND WAVE
#================================================================

#create dataset for regression
DSgrowth2 <-
  rsv_growth2 %>%
  dplyr::filter(covper == "y2022") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale, -region, -latitude)

#combine stringency and onset datasets
DSgrowth2 <-
  DSgrowth2 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = median(strin_indx),
                pop_dens = median(pop_dens),
                med_age = median(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR INTENSITY DURING FIRST WAVE
#================================================================

#create dataset for regression
DSintens1 <-
  rsv_intens2 %>%
  dplyr::filter(covper == "y2021") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale, -region, -latitude)

#combine stringency and onset datasets
DSintens1 <-
  DSintens1 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = median(strin_indx),
                pop_dens = median(pop_dens),
                med_age = median(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR INTENSITY DURING SECOND WAVE
#================================================================

#create dataset for regression
DSintens2 <-
  rsv_intens2 %>%
  dplyr::filter(covper == "y2022") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone)) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale, -region, -latitude)

#combine stringency and onset datasets
DSintens2 <-
  DSintens2 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = median(strin_indx),
                pop_dens = median(pop_dens),
                med_age = median(med_age)) %>%
      ungroup()
  )

#================================================================
# ADD MISCELLENEOUS VARIABLES
#================================================================

#Does intensity in 2021 influence onset in 2022 
DSonset2 <- 
  DSonset2 %>% 
  dplyr::left_join(
    DSintens1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intens2021" = "intensity"))

#Does intensity in 2021 influence peak in 2022 
DSpeak2 <- 
  DSpeak2 %>% 
  dplyr::left_join(
    DSintens1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intens2021" = "intensity"))

#Does intensity in 2021 influence growth in 2022 
DSgrowth2 <- 
  DSgrowth2 %>% 
  dplyr::left_join(
    DSintens1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intens2021" = "intensity"))

#Does intensity in 2021 influence intensity in 2022
DSintens2 <- 
  DSintens2 %>% 
  dplyr::left_join(
    DSintens1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intens2021" = "intensity"))

#================================================================
# UNIVARIATE REGRESSION ANALYSES
#================================================================

#ONSET DURING FIRST WAVE
#standardize all continuous variables
DSonset1 <- 
  DSonset1 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSonset1 <- 
  DSonset1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone))

#fit univariate models
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X1 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Onset timing 2021") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#ONSET DURING SECOND WAVE
#standardize all continuous variables
DSonset2 <- 
  DSonset2 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         intens20212 = (intens2021-mean(intens2021))/sd(intens2021))

#transform categorical variables to factors
DSonset2 <- 
  DSonset2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit univariate models
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X2 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Onset timing 2022") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#PEAK DURING FIRST WAVE
#standardize all continuous variables
DSpeak1 <- 
  DSpeak1 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSpeak1 <- 
  DSpeak1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X3 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Peak timing 2021") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#PEAK DURING SECOND WAVE
#standardise all continuous variables
DSpeak2 <- 
  DSpeak2 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         intens20212 = (intens2021-mean(intens2021))/sd(intens2021))

#transform categorical variables to factors
DSpeak2 <- 
  DSpeak2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000)), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X4 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Peak timing 2022") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#GROWTH DURING FIRST WAVE
#standardise all continuous variables
DSgrowth1 <- 
  DSgrowth1 %>% 
  mutate(pks2 = (pks - mean(pks))/sd(pks),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSgrowth1 <- 
  DSgrowth1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth1), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X5 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Growth rate 2021") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#GROWTH DURING SECOND WAVE
#standardise all continuous variables
DSgrowth2 <- 
  DSgrowth2 %>% 
  mutate(pks2 = (pks - mean(pks))/sd(pks),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         intens20212 = (intens2021-mean(intens2021))/sd(intens2021))

#transform categorical variables to factors
DSgrowth2 <- 
  DSgrowth2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth2), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X6 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Growth rate 2022") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#INTENSITY DURING FIRST WAVE
#standardise all continuous variables
DSintens1 <- 
  DSintens1 %>% 
  mutate(intensity2 = (intensity - mean(intensity))/sd(intensity),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSintens1 <- 
  DSintens1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens1), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X7 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Intensity 2021") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)


#INTENSITY DURING SECOND WAVE
#standardise all continuous variables
DSintens2 <- 
  DSintens2 %>% 
  mutate(intensity2 = (intensity - mean(intensity))/sd(intensity),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         intens20212 = (intens2021-mean(intens2021))/sd(intens2021))

#transform categorical variables to factors
DSintens2 <- 
  DSintens2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2), exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95))}
X8 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Intensity 2022") %>% dplyr::filter(term != "(Intercept)") %>% dplyr::select(term, estimate, conf.low, conf.high, p.value, ds)
