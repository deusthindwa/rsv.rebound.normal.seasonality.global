#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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
# CREATE DATASET FOR TIME TO ONSET WEEK IN 2021
#================================================================

#time to first onset in 2021 from 2020 COVID-19 suppression in July
DSonset1 <-
rsv_onset %>%
  dplyr::select(country, covper, epiwk, clim_zone, hemi, region) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper == "y2021") %>%
  dplyr::mutate(covper = str_sub(covper, 2, 5),
                epiwk = round(epiwk, digits = 0),
                onset = as.Date(paste(covper, epiwk, 1, sep="-"), "%Y-%W-%u"),
                onset = as.integer(difftime(onset, as.Date("2020-07-01"), units = "weeks")),
                region = ifelse(region == "Africa", "Africa & SEA",
                                ifelse(region == "South East Asia", "Africa & SEA", region)),
                region = ifelse(region == "Europe", " Europe", region),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
                latitude = ifelse(latitude == "00-C", "15-S",
                                  ifelse(latitude == "30-S", "45-S",
                                         ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::select(country, onset, clim_zone, hemi, region, latitude, out_seas21) %>%
  dplyr::filter(clim_zone != "")

DSonset1 <-
  DSonset1 %>% 
  dplyr::filter(!is.na(onset)) %>%
  dplyr::arrange(country, clim_zone, hemi, region, latitude, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq(as.Date("2020-07-01"), by = "weeks", length.out = n())) %>%
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
# CREATE DATASET FOR TIME TO ONSET WEEK IN 2022
#================================================================

#time to second onset post-COVID-19 from 2021 COVID-19 suppression
DSonset2 <- 
  rsv_onset %>%
  dplyr::select(country, covper, epiwk, clim_zone, hemi, region) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper != "precov") %>%
  pivot_wider(names_from = covper, values_from = epiwk) %>%
  dplyr::mutate(y2021 = round(y2021, digits = 0),
                y2022 = round(y2022, digits = 0),
                y2021 = as.Date(paste("2021", y2021, 1, sep="-"), "%Y-%W-%u"),
                y2022 = as.Date(paste("2022", y2022, 1, sep="-"), "%Y-%W-%u"),
                onset = as.integer(difftime(y2022, y2021, units = "weeks")),
                region = ifelse(region == "Africa", "Africa & SEA",
                                ifelse(region == "South East Asia", "Africa & SEA", 
                                       ifelse(region == "Europe", " Europe", region))),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
                latitude = ifelse(latitude == "00-C", "15-S",
                                  ifelse(latitude == "30-S", "45-S",
                                         ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::select(country, onset, clim_zone, hemi, region, latitude, out_seas21, y2021) %>%
  dplyr::filter(clim_zone != "")

DSonset2 <-
  DSonset2 %>% 
  dplyr::filter(!is.na(onset)) %>%
  dplyr::arrange(country, clim_zone, hemi, region, latitude, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(onset) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                y2021 = date(ifelse(time == 1, y2021, NA_Date_)),
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
# CREATE DATASET FOR TIME TO PEAK WEEK IN 2021
#================================================================

#time to first onset in 2021 from 2020 COVID-19 suppression in July
DSpeak1 <-
  rsv_peak2 %>%
  dplyr::select(country, covper, difloc, clim_zone, hemi, region) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper == "y2021") %>%
  dplyr::mutate(covper = str_sub(covper, 2, 5),
                difloc = round(difloc, digits = 0),
                peak = as.Date(paste(covper, difloc, 1, sep="-"), "%Y-%W-%u"),
                peak = as.integer(difftime(peak, as.Date("2020-07-01"), units = "weeks")),
                region = ifelse(region == "Africa", "Africa & SEA",
                                ifelse(region == "South East Asia", "Africa & SEA", region)),
                region = ifelse(region == "Europe", " Europe", region),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
                latitude = ifelse(latitude == "00-C", "15-S",
                                  ifelse(latitude == "30-S", "45-S",
                                         ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::select(country, peak, clim_zone, hemi, region, latitude, out_seas21) %>%
  dplyr::filter(clim_zone != "")

DSpeak1 <-
  DSpeak1 %>% 
  dplyr::filter(!is.na(peak)) %>%
  dplyr::arrange(country, clim_zone, hemi, region, latitude, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                fdate = seq(as.Date("2020-07-01"), by = "weeks", length.out = n())) %>%
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
# CREATE DATASET FOR TIME TO PEAK WEEK IN 2022
#================================================================

#time to second onset post-COVID-19 from 2021 COVID-19 suppression
DSpeak2 <- 
  rsv_peak2 %>%
  dplyr::select(country, covper, difloc, clim_zone, hemi, region) %>%
  dplyr::left_join(climate) %>%
  dplyr::filter(covper != "precov") %>%
  pivot_wider(names_from = covper, values_from = difloc) %>%
  dplyr::mutate(y2021 = round(y2021, digits = 0),
                y2022 = round(y2022, digits = 0),
                y2021 = as.Date(paste("2021", y2021, 1, sep="-"), "%Y-%W-%u"),
                y2022 = as.Date(paste("2022", y2022, 1, sep="-"), "%Y-%W-%u"),
                peak = as.integer(difftime(y2022, y2021, units = "weeks")),
                region = ifelse(region == "Africa", "Africa & SEA",
                                ifelse(region == "South East Asia", "Africa & SEA", 
                                       ifelse(region == "Europe", " Europe", region))),
                clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
                latitude = ifelse(latitude == "00-C", "15-S",
                                  ifelse(latitude == "30-S", "45-S",
                                         ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::select(country, peak, clim_zone, hemi, region, latitude, out_seas21, y2021) %>%
  dplyr::filter(clim_zone != "")

DSpeak2 <-
  DSpeak2 %>% 
  dplyr::filter(!is.na(peak)) %>%
  dplyr::arrange(country, clim_zone, hemi, region, latitude, out_seas21) %>%
  utils::type.convert(as.is = TRUE) %>% 
  tidyr::uncount(peak) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(time = seq.int(from = 1, by = 1, length.out = n()),
                y2021 = date(ifelse(time == 1, y2021, NA_Date_)),
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
# CREATE DATASET FOR GROWTH RATE 2021
#================================================================

#create dataset for regression
DSgrowth1 <-
  rsv_growth2 %>%
  dplyr::filter(covper == "y2021") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(region = ifelse(region == "Africa", "Africa & SEA",
                         ifelse(region == "South East Asia", "Africa & SEA", region)),
         region = ifelse(region == "Europe", " Europe", region),
         clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
         latitude = ifelse(latitude == "00-C", "15-S",
                           ifelse(latitude == "30-S", "45-S",
                                  ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale)

#combine stringency and onset datasets
DSgrowth1 <-
  DSgrowth1 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = mean(strin_indx),
                pop_dens = mean(pop_dens),
                med_age = mean(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR GROWTH RATE 2022
#================================================================

#create dataset for regression
DSgrowth2 <-
  rsv_growth2 %>%
  dplyr::filter(covper == "y2022") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(region = ifelse(region == "Africa", "Africa & SEA",
                         ifelse(region == "South East Asia", "Africa & SEA", region)),
         region = ifelse(region == "Europe", " Europe", region),
         clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
         latitude = ifelse(latitude == "00-C", "15-S",
                           ifelse(latitude == "30-S", "45-S",
                                  ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale)

#combine stringency and onset datasets
DSgrowth2 <-
  DSgrowth2 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = mean(strin_indx),
                pop_dens = mean(pop_dens),
                med_age = mean(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR INTENSITY 2021
#================================================================

#create dataset for regression
DSintens1 <-
  rsv_intens2 %>%
  dplyr::filter(covper == "y2021") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(region = ifelse(region == "Africa", "Africa & SEA",
                         ifelse(region == "South East Asia", "Africa & SEA", region)),
         region = ifelse(region == "Europe", " Europe", region),
         clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
         latitude = ifelse(latitude == "00-C", "15-S",
                           ifelse(latitude == "30-S", "45-S",
                                  ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale)

#combine stringency and onset datasets
DSintens1 <-
  DSintens1 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = mean(strin_indx),
                pop_dens = mean(pop_dens),
                med_age = mean(med_age)) %>%
      ungroup()
  )

#================================================================
# CREATE DATASET FOR INTENSITY 2022
#================================================================

#create dataset for regression
DSintens2 <-
  rsv_intens2 %>%
  dplyr::filter(covper == "y2022") %>%
  left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct()) %>%
  left_join(climate) %>%
  mutate(region = ifelse(region == "Africa", "Africa & SEA",
                         ifelse(region == "South East Asia", "Africa & SEA", region)),
         region = ifelse(region == "Europe", " Europe", region),
         clim_zone = ifelse(clim_zone == "Temperate", " Temperate", clim_zone),
         latitude = ifelse(latitude == "00-C", "15-S",
                           ifelse(latitude == "30-S", "45-S",
                                  ifelse(latitude == "15-N", "30-N", latitude)))) %>%
  dplyr::filter(clim_zone != "") %>%
  dplyr::select(everything(), -covper, -wk_scale)

#combine stringency and onset datasets
DSintens2 <-
  DSintens2 %>% 
  left_join(
    stringency %>%
      group_by(country) %>%
      summarise(strin_indx = mean(strin_indx),
                pop_dens = mean(pop_dens),
                med_age = mean(med_age)) %>%
      ungroup()
  )

#================================================================
# ADD MISCELLENEOUS VARIABLES
#================================================================

#Does intensity in 2021 influence intensity in 2022 
DSintens2 <- 
  DSintens2 %>% 
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

#Does intensity in 2021 influence onset in 2022 
DSonset2 <- 
  DSonset2 %>% 
  dplyr::left_join(
    DSintens1 %>%
      dplyr::select(country, intensity) %>%
      dplyr::rename("intens2021" = "intensity"))

#Does growth in 2021 influence intensity in 2021 
DSintens1 <- 
  DSintens1 %>% 
  dplyr::left_join(
    DSgrowth1 %>%
      dplyr::select(country, pks) %>%
      dplyr::rename("growth2021" = "pks"))

#Does growth in 2021 influence intensity in 2022 
DSintens2 <- 
  DSintens2 %>% 
  dplyr::left_join(
    DSgrowth1 %>%
      dplyr::select(country, pks) %>%
      dplyr::rename("growth2021" = "pks"))

#Does growth in 2022 influence intensity in 2022 
DSintens2 <- 
  DSintens2 %>% 
  dplyr::left_join(
    DSgrowth2 %>%
      dplyr::select(country, pks) %>%
      dplyr::rename("growth2022" = "pks"))

#================================================================
# UNIVARIATE REGRESSION ANALYSES
#================================================================

#ONSET 2021
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
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region))

#fit univariate models
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000)), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X1 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Onset timing 2021") %>% dplyr::filter(term != "(Intercept)")


#ONSET 2022
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
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit univariate models
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000)), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X2 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Onset timing 2022") %>% dplyr::filter(term != "(Intercept)")


#PEAK 2021
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
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000)), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X3 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Peak timing 2021") %>% dplyr::filter(term != "(Intercept)")


#PEAK 2022
#standardise all continuous variables
DSpeak2 <- 
  DSpeak2 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSpeak2 <- 
  DSpeak2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212")) {
  X[[i]] <- print(tidy(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000)), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X4 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Peak timing 2022") %>% dplyr::filter(term != "(Intercept)")


#GROWTH 2021
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
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth1), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X5 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Growth rate 2021") %>% dplyr::filter(term != "(Intercept)")


#GROWTH 2022
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
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth2), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X6 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Growth rate 2022") %>% dplyr::filter(term != "(Intercept)")


#INTENSITY 2021
#standardise all continuous variables
DSintens1 <- 
  DSintens1 %>% 
  mutate(intensity2 = (intensity - mean(intensity))/sd(intensity),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         growth20212 = (growth2021-mean(growth2021))/sd(growth2021))

#transform categorical variables to factors
DSintens1 <- 
  DSintens1 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "growth20212")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens1), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X7 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Intensity 2021") %>% dplyr::filter(term != "(Intercept)")


#INTENSITY 2022
#standardise all continuous variables
DSintens2 <- 
  DSintens2 %>% 
  mutate(intensity2 = (intensity - mean(intensity))/sd(intensity),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age),
         intens20212 = (intens2021-mean(intens2021))/sd(intens2021),
         growth20212 = (growth2021-mean(growth2021))/sd(growth2021),
         growth20222 = (growth2022-mean(growth2022))/sd(growth2022))

#transform categorical variables to factors
DSintens2 <- 
  DSintens2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the models iteratively & store model fitted values
X <- list()
for (i in c("hemi2", "clim_zone2", "region2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212", "growth20212", "growth20222")) {
  X[[i]] <- print(tidy(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2), exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95))}
X8 <- dplyr::bind_rows(X) %>% dplyr::mutate(ds = "Intensity 2022") %>% dplyr::filter(term != "(Intercept)")

DSregressUv <- 
  dplyr::bind_rows(X1, X2, X3, X4, X5, X6, X7, X8) %>%
  dplyr::group_by(term) %>%
  dplyr::mutate(widthx = 0.1*n()) %>% #fix the problem of differing errobar width due to different number of terms in the dataset
  dplyr::mutate(term = case_when(term == "clim_zone2Sub-tropical" ~ "Climate Zone \n Subtropical (vs temperate)",
                                 term == "clim_zone2Tropical" ~ "Climate Zone \n Tropical (vs temperate)",
                                 term == "growth20212" ~ "RSV growth in 2021",
                                 term == "growth20222" ~ "RSV growth in 2022",
                                 term == "hemi2Southern hemisphere" ~ "Hemisphere \n Southern (vs Northern)",
                                 term == "intens20212" ~ "RSV intensity in 2021",
                                 term == "med_age2" ~ "Population median age",
                                 term == "out_seas212yes" ~ "RSV out-of-season in 2021, yes",
                                 term == "pop_dens2" ~ "Population density",
                                 term == "region2Africa & SEA" ~ "Region \n Africa/SEA (vs Europe)",
                                 term == "region2Eastern Mediterranean" ~ "Region \n Eastern Mediterranean (vs Europe)",
                                 term == "region2North Americas" ~ "Region \n North Americas (vs Europe)",
                                 term == "region2South Americas" ~ "Region \n South Americas (vs Europe)",
                                 term == "region2Western Pacific" ~ "Region \n Western Pacific (vs Europe)",
                                 term == "strin_indx2" ~ "Contact stringency",
                                 TRUE ~ NA_character_))
A <-
  DSregressUv %>%
  ggplot(aes(y = reorder(term, ds), x = estimate, color = ds, width = widthx)) +
  geom_point(shape = 16, size = 4, stroke = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1.5, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", cex = 0.6, alpha = 0.8) +
  labs(title = "", x = "Log-scale estimates from univariate regression models with 95%CI", y = "") + 
  facet_grid(.~ ds, scales = "free_x") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  guides(color = guide_legend(title = "")) +
  theme(legend.position = "none", strip.text.x = element_text(size = 16)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))  + 
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#A9A9A9", "#36454F", "#7CAE00", "#C77CFF", "#FFC133", "#7D33FF"))


ggsave(here("output", "fig6_regressUnivariate.png"),
       plot = A,
       width = 21, height = 10, unit="in", dpi = 300)

rm(X1, X2, X3, X4, X5, X6, X7, X8, A)
