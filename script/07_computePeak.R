#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#PEAK TIMING OF RSV CASES IN EACH COUNTRY
#====================================================================

#split the dataset by country/regionUS to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States", 
                               "United States North East", "United States West", "United States South", "United States Mid West")) %>%
  dplyr::arrange(country, yr, wk, cases) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(seqwk, cases, country) %>%
  base::split(list(.$country))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models
Gmodels <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#create list to store peak timing and time series data for each country
peakDS <- list()
tsDS <- list()

#iterate for each country, compute timing of the peak and the peak value
for (i in names(X)){
  peakDS[[i]] = data.frame(findpeaks(abs(Gmodels[[i]]$fitted.values), MinPeakHeight = 10, MinPeakDistance = 1, MinPeakWidth = 1, MaxPeakWidth = Inf, DoubleSided = FALSE))
}

rsv_peak1 <-
  dplyr::bind_rows(peakDS, .id = "country") %>% 
  dplyr::select(country, baseline, pks, loc, parabol.x.from, parabol.x.to) %>% 
  dplyr::rename(locL = parabol.x.from, locU = parabol.x.to)

#iterate for each country, extract fitted case values
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
}

rsv_ts <- 
  dplyr::bind_rows(tsDS, .id = "country")

#====================================================================

#summaries the peak values for comparison before and after COVID-19
rsv_peak1 <-
rsv_peak1 %>% 
  dplyr::mutate(difloc = ifelse((loc > 0L & loc <= 52L), loc,
                           ifelse((loc > 52L & loc <= 104), subtract(loc, 52),
                                   ifelse((loc > 104L & loc <= 156L), subtract(loc, 104),
                                           ifelse((loc > 156L & loc <= 208L), subtract(loc, 156),
                                                   ifelse((loc > 208L & loc <= 260L), subtract(loc, 208),
                                                           ifelse((loc > 260L & loc <= 312L), subtract(loc, 260),
                                                                   ifelse((loc > 312L & loc <= 364L), subtract(loc, 312), NA_integer_))))))),
                diflocL = ifelse((locL > 0L & locL <= 52L), locL,
                                ifelse((locL > 52L & locL <= 104), subtract(locL, 52),
                                       ifelse((locL > 104L & locL <= 156L), subtract(locL, 104),
                                              ifelse((locL > 156L & locL <= 208L), subtract(locL, 156),
                                                     ifelse((locL > 208L & locL <= 260L), subtract(locL, 208),
                                                            ifelse((locL > 260L & locL <= 312L), subtract(locL, 260),
                                                                   ifelse((locL > 312L & locL <= 364L), subtract(locL, 312), NA_integer_))))))),
                diflocU = ifelse((locU > 0L & locU <= 52L), locU,
                                ifelse((locU > 52L & locU <= 104), subtract(locU, 52),
                                       ifelse((locU > 104L & locU <= 156L), subtract(locU, 104),
                                              ifelse((locU > 156L & locU <= 208L), subtract(locU, 156),
                                                     ifelse((locU > 208L & locU <= 260L), subtract(locU, 208),
                                                            ifelse((locU > 260L & locU <= 312L), subtract(locU, 260),
                                                                   ifelse((locU > 312L & locU <= 364L), subtract(locU, 312), NA_integer_))))))),
                year = ifelse((loc > 0L & loc <= 52L), "2017",
                              ifelse((loc > 52L & loc <= 104), "2018",
                                     ifelse((loc > 104L & loc <= 156L), "2019",
                                            ifelse((loc > 156L & loc <= 208L), "2020",
                                                   ifelse((loc > 208L & loc <= 260L), "2021",
                                                          ifelse((loc > 260L & loc <= 312L), "2022",
                                                                 ifelse((loc > 312L & loc <= 364L), "2023", NA_character_))))))))


#====================================================================

#summarise by precovid vs 2021/22 and 2022/23 seasons
rsv_peak2 <-
  rsv_peak1 %>%
  dplyr::select(country, year, difloc, diflocL, diflocU) %>%
  dplyr::mutate(covper = ifelse(year == "2023", "y2023",
                                ifelse(year == "2022", "y2022",
                                       ifelse(year == "2021", "y2021",
                                              ifelse(year == "2017" | year == "2018" | year == "2019", "precov", NA_character_))))) %>% 
  dplyr::filter(!is.na(covper)) %>%
  dplyr::select(everything(), -year) %>%
  
  dplyr::mutate(covper = ifelse(covper == "y2022" & country == "Hungary", "y2021",
                                ifelse(covper == "y2023" & country == "Hungary", "y2022", covper))) %>%
  
  dplyr::mutate(covper = ifelse(covper == "precov", "precov",
                                ifelse(covper == "y2021", "y2021/22", "y2022/23"))) %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "precov", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(difloc = max(difloc, na.rm = TRUE), diflocL = max(diflocL, na.rm = TRUE), diflocU = max(diflocU, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "y2021/22", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(difloc = max(difloc, na.rm = TRUE), diflocL = max(diflocL, na.rm = TRUE), diflocU = max(diflocU, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "y2022/23", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(difloc = max(difloc, na.rm = TRUE), diflocL = max(diflocL, na.rm = TRUE), diflocU = max(diflocU, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::left_join(climate %>% dplyr::select(country, clim_zone)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  dplyr::select(everything(), -covper2) %>%
  
  dplyr::mutate(covper = ifelse(covper == "precov", "precov",
                                ifelse(covper == "y2021/22", "y2021", "y2022")))

#====================================================================

#delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak1|rsv_peak2|X|rsv_ts|stringency|rsv_dtw|A|B|C|D", ls(), value = TRUE, invert = TRUE))
