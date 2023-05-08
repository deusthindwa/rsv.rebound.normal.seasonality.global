#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# DOWNLOAD AND BUILD A STRINGENCY DATASET
#================================================================

#input dynamics stringency dataset
stringency <- read.csv(curl("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

stringency <-
  stringency %>%
  dplyr::select(location, date, stringency_index, population_density, median_age) %>%
  dplyr::rename("country" = location, "fdate" = date, "strin_indx" = stringency_index, "pop_dens" = population_density, "med_age" = median_age) %>%
  dplyr::filter(!is.na(strin_indx)) %>%
  dplyr::mutate(fdate = date(fdate),
                country = ifelse(country == "United Kingdom", "Scotland", country))

stringency <-
  stringency %>%
  dplyr::filter(country == "Scotland") %>%
  mutate(country = ifelse(country == "Scotland", "Northern Ireland", country)) %>%
  bind_rows(stringency)

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
# REGRESSION ANALYSIS OF ONSET 2021
#================================================================

#log rank test for the difference between variable categories/values
survdiff(Surv(time, event) ~ hemi, data = DSonset1)
survdiff(Surv(time, event) ~ latitude, data = DSonset1)
survdiff(Surv(time, event) ~ clim_zone, data = DSonset1)
survdiff(Surv(time, event) ~ region, data = DSonset1)
survdiff(Surv(time, event) ~ strin_indx, data = DSonset1)
survdiff(Surv(time, event) ~ pop_dens, data = DSonset1)
survdiff(Surv(time, event) ~ med_age, data = DSonset1)

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

#fit the model & store model fitted values
Modonset1 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + region2 + strin_indx2 + pop_dens2 + med_age2, 
                   data = DSonset1,
                   control = coxph.control(iter.max = 1000))
tidy(Modonset1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
DSonset1Mod <- tidy(Modonset1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#store model predictions
DSonset1Pre <- augment(Modonset1, DSonset1)

#extract metrics for model quality
glance(Modonset1)[c(1, 2, 11, 12, 15, 16, 17)]

#================================================================
# REGRESSION ANALYSIS OF ONSET 2022
#================================================================

#log rank test for the difference between variable categories/values
survdiff(Surv(time, event) ~ hemi, data = DSonset2)
survdiff(Surv(time, event) ~ latitude, data = DSonset2)
survdiff(Surv(time, event) ~ clim_zone, data = DSonset2)
survdiff(Surv(time, event) ~ region, data = DSonset2)
survdiff(Surv(time, event) ~ out_seas21, data = DSonset2)
survdiff(Surv(time, event) ~ strin_indx, data = DSonset2)
survdiff(Surv(time, event) ~ pop_dens, data = DSonset2)
survdiff(Surv(time, event) ~ med_age, data = DSonset2)

#standardize all continuous variables
DSonset2 <- 
  DSonset2 %>% 
  mutate(strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSonset2 <- 
  DSonset2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the model & store model fitted values
Modonset2 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + region2 + out_seas212 + strin_indx2 + pop_dens2 + med_age2, 
                  data = DSonset2,
                  control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
DSonset2Mod <-tidy(Modonset2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#store model predictions
DSonset2Pre <- augment(Modonset2, DSonset2)

#extract metrics for model quality
glance(Modonset2)[c(1,2,11,12, 15, 16, 17)]

#================================================================
# REGRESSION ANALYSIS OF PEAK 2021
#================================================================

#log rank test for the difference between variable categories/values
survdiff(Surv(time, event) ~ hemi, data = DSpeak1)
survdiff(Surv(time, event) ~ latitude, data = DSpeak1)
survdiff(Surv(time, event) ~ clim_zone, data = DSpeak1)
survdiff(Surv(time, event) ~ region, data = DSpeak1)
survdiff(Surv(time, event) ~ strin_indx, data = DSpeak1)
survdiff(Surv(time, event) ~ pop_dens, data = DSpeak1)
survdiff(Surv(time, event) ~ med_age, data = DSpeak1)

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

#fit the model & store model fitted values
Modpeak1 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + region2 + strin_indx2 + pop_dens2 + med_age2, 
                  data = DSpeak1,
                  control = coxph.control(iter.max = 1000))
tidy(Modpeak1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
DSpeak1Mod <- tidy(Modpeak1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#store model predictions
DSpeak1Pre <- augment(Modpeak1, DSpeak1)

#extract metrics for model quality
glance(Modpeak1)[c(1, 2, 11, 12, 15, 16, 17)]

#================================================================
# REGRESSION ANALYSIS OF PEAK 2022
#================================================================

#log rank test for the difference between variable categories/values
survdiff(Surv(time, event) ~ hemi, data = DSpeak2)
survdiff(Surv(time, event) ~ latitude, data = DSpeak2)
survdiff(Surv(time, event) ~ clim_zone, data = DSpeak2)
survdiff(Surv(time, event) ~ region, data = DSpeak2)
survdiff(Surv(time, event) ~ out_seas21, data = DSpeak2)
survdiff(Surv(time, event) ~ strin_indx, data = DSpeak2)
survdiff(Surv(time, event) ~ pop_dens, data = DSpeak2)
survdiff(Surv(time, event) ~ med_age, data = DSpeak2)

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

#fit the model & store model fitted values
Modpeak2 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + region2 + out_seas212 + strin_indx2 + pop_dens2 + med_age2, 
                 data = DSpeak2,
                 control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
DSpeak2Mod <- tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#store model predictions
DSpeak2Pre <- augment(Modpeak2, DSpeak2)

#extract metrics for model quality
glance(Modpeak2)[c(1, 2, 11, 12, 15, 16, 17)]

#================================================================
# REGRESSION ANALYSIS OF GROWTH 2021
#================================================================

#data varoable description
knitr::kable(describeBy(DSgrowth1) %>% round(2))

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
         region2 = as.factor(region))

#fit the model & store model fitted values
Modgrowth1 = lm(pks2 ~ hemi2 + clim_zone2 + region2  + strin_indx2 + pop_dens2 + med_age2, 
                data = DSgrowth1)
get_regression_table(Modgrowth1, conf.level = 0.95) %>% kable()
DSgrowth1Mod <- get_regression_table(Modgrowth1, conf.level = 0.95)

#store model predictions
DSgrowth1Pre <- get_regression_points(Modgrowth1)

#extract metrics for model quality
get_regression_summaries(Modgrowth1)

#================================================================
# REGRESSION ANALYSIS OF GROWTH 2022
#================================================================

#data varoable description
knitr::kable(describeBy(DSgrowth2) %>% round(2))

#standardise all continuous variables
DSgrowth2 <- 
  DSgrowth2 %>% 
  mutate(pks2 = (pks - mean(pks))/sd(pks),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSgrowth2 <- 
  DSgrowth2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region),
         out_seas212 = as.factor(out_seas21))

#fit the model & store model fitted values
Modgrowth2 = lm(pks ~ hemi2 + clim_zone2 + region2 + out_seas212 + strin_indx2 + pop_dens2 + med_age2, 
                data = DSgrowth2)
get_regression_table(Modgrowth2, conf.level = 0.95) %>% kable()
DSgrowth2Mod <- get_regression_table(Modgrowth2, conf.level = 0.95)

#store model predictions
DSgrowth2Pre <- get_regression_points(Modgrowth2)

#extract metrics for model quality
get_regression_summaries(Modgrowth2)

#================================================================
# REGRESSION ANALYSIS OF INTENSITY 2021
#================================================================

#data varoable description
knitr::kable(describeBy(DSintens1) %>% round(2))

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
         region2 = as.factor(region))

#fit the model & store model fitted values
Modintensity1 = lm(intensity2 ~ hemi2 + clim_zone2 + region2  + strin_indx2 + pop_dens2 + med_age2, 
                data = DSintens1)
get_regression_table(Modintensity1, conf.level = 0.95) %>% kable()
DSintensity1Mod <- get_regression_table(Modintensity1, conf.level = 0.95)

#store model predictions
DSintensity1Pre <- get_regression_points(Modintensity1)

#extract metrics for model quality
get_regression_summaries(Modintensity1)

#================================================================
# REGRESSION ANALYSIS OF INTENSITY 2022
#================================================================

#data varoable description
knitr::kable(describeBy(DSintens2) %>% round(2))

#standardise all continuous variables
DSintens2 <- 
  DSintens2 %>% 
  mutate(intensity2 = (intensity - mean(intensity))/sd(intensity),
         strin_indx2 = (strin_indx - mean(strin_indx))/sd(strin_indx),
         pop_dens2 = (pop_dens - mean(pop_dens))/sd(pop_dens),
         med_age2 = (med_age - mean(med_age))/sd(med_age))

#transform categorical variables to factors
DSintens2 <- 
  DSintens2 %>% 
  mutate(hemi2 = as.factor(hemi),
         clim_zone2 = as.factor(clim_zone),
         region2 = as.factor(region))

#fit the model & store model fitted values
Modintensity2 = lm(intensity2 ~ hemi2 + clim_zone2 + region2  + strin_indx2 + pop_dens2 + med_age2, 
                   data = DSintens2)
get_regression_table(Modintensity2, conf.level = 0.95) %>% kable()
DSintensity2Mod <- get_regression_table(Modintensity2, conf.level = 0.95)

#store model predictions
DSintensity2Pre <- get_regression_points(Modintensity2)

#extract metrics for model quality
get_regression_summaries(Modintensity2)
