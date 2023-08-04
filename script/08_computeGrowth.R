#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#GROWTH RATES OF RSV CASES IN EACH COUNTRY
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
  group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()),
                seqwk2 = seqwk) %>%
  dplyr::ungroup() %>%
  dplyr::select(seqwk, seqwk2, cases, country) %>%
  base::split(list(.$country))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[base::unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv.pred <- list()
rsv_growth2 <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
  
} #+ s(seqwk2, bs = "re")

#iterate for each country, extract fitted case values
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
 
  deriv.pred[[i]] = data.frame(deriv = diff(log(tsDS[[i]]$fitcases+1))/diff(tsDS[[i]]$seqwk)) %>%
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
  rsv_growth2[[i]] = data.frame(gsignal::findpeaks(deriv.pred[[i]]$deriv, MinPeakHeight = 0.01, MinPeakDistance = 1, MinPeakWidth = 1, MaxPeakWidth = Inf, DoubleSided = TRUE))
}

#unlist all the datasets
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")
deriv.pred <- dplyr::bind_rows(deriv.pred, .id = "country")
rsv_growth2 <- dplyr::bind_rows(rsv_growth2, .id = "country") %>% dplyr::filter(pks >0)

#====================================================================
#====================================================================

#combined dataset
rsv_growth1 <- 
  left_join(deriv.pred, tsDS) %>%
  left_join(X) %>%
  dplyr::select(country, seqwk, cases, fitcases, deriv) %>%
  dplyr::mutate(year = ifelse((seqwk > 0L & seqwk <= 52L), "2017",
                              ifelse((seqwk > 52L & seqwk <= 104), "2018",
                                     ifelse((seqwk > 104L & seqwk <= 156L), "2019",
                                            ifelse((seqwk > 156L & seqwk <= 208L), "2020",
                                                   ifelse((seqwk > 208L & seqwk <= 260L), "2021",
                                                          ifelse((seqwk > 260L & seqwk <= 312L), "2022",
                                                                 ifelse((seqwk > 312L & seqwk <= 364L), "2023", NA_character_))))))))

#generate a dataset for location of maximum growth
rsv_growth2 <-
  rsv_growth2 %>% 
  dplyr::select(country, baseline, pks, loc, parabol.x.from, parabol.x.to) %>% 
  dplyr::rename(locL = parabol.x.from, locU = parabol.x.to)

#combine growth 1 and growth 2 for plotting
rsv_growth1 <- rsv_growth1 %>% left_join(rsv_growth2, by = c("country", "seqwk" = "loc"))

#====================================================================

#summaries the growth values for comparison before and after COVID-19
rsv_growth2 <-
  rsv_growth2 %>% 
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
rsv_growth2 <-
  rsv_growth2 %>%
  dplyr::select(country, year, pks) %>%
  dplyr::mutate(covper = ifelse(year == "2023", "y2023",
                                ifelse(year == "2022", "y2022",
                                       ifelse(year == "2021", "y2021",
                                              ifelse(year == "2017" | year == "2018" | year == "2019", "precov", NA_character_))))) %>% 
  dplyr::mutate(covper = ifelse(country == "Mongolia" & year == 2022, "y2021",
                              ifelse(country == "Mongolia" & year == 2023, "y2022", covper))) %>%
  dplyr::filter(!is.na(covper), year != "y2023") %>%
  dplyr::select(everything(), -year) %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "precov", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "y2021", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::mutate(covper2 = ifelse(covper == "y2022", 1, seq(1:n()))) %>%
  dplyr::group_by(country, covper, covper2) %>%
  dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  dplyr::left_join(climate %>% dplyr::select(country, clim_zone)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  dplyr::select(everything(), -covper2)

rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak|rsv_growth|stringency|A|B", ls(), value = TRUE, invert = TRUE))
