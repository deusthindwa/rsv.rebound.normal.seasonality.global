#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#INTENSITY OF RSV CASES IN EACH COUNTRY
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
X <- X[base::unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv.pred <- list()
rsv_intens2 <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, extract fitted case values
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
 
  deriv.pred[[i]] = data.frame(deriv = diff(log(tsDS[[i]]$fitcases + 1))/diff(tsDS[[i]]$seqwk)) %>%
    dplyr::mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
}

#create a dataset for integration with positive log derivative series only
deriv.pred <-
  dplyr::bind_rows(deriv.pred, .id = "country") %>%
  dplyr::filter(deriv >0) %>%
  dplyr::mutate(intcat = cumsum(c(1, abs(seqwk[-length(seqwk)] - seqwk[-1]) > 1)),
                year = ifelse((seqwk > 0L & seqwk <= 52L), "2017",
                              ifelse((seqwk > 52L & seqwk <= 104), "2018",
                                     ifelse((seqwk > 104L & seqwk <= 156L), "2019",
                                            ifelse((seqwk > 156L & seqwk <= 208L), "2020",
                                                   ifelse((seqwk > 208L & seqwk <= 260L), "2021",
                                                          ifelse((seqwk > 260L & seqwk <= 312L), "2022",
                                                                 ifelse((seqwk > 312L & seqwk <= 364L), "2023", NA_character_)))))))) %>%
  dplyr::filter(year != "2023", year != "2020") %>%
  dplyr::group_by(intcat) %>%
  dplyr::mutate(counter = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(counter != 1)

#take the integral of the positive log derivative series (area under the curve)
rsv_intens2 <- 
  deriv.pred %>%  
  dplyr::group_by(intcat) %>%
  dplyr::mutate(intensity = integrate(splinefun(seqwk, deriv), range(seqwk)[1], range(seqwk)[2])$value) %>% #OR 'approxfun' for linear interpolation via trapezoids
  dplyr::ungroup() %>%
  dplyr::distinct(country, year, .keep_all = TRUE) %>%
  dplyr::select(country, year, intensity)

#====================================================================
#====================================================================

#combined dataset
rsv_intens1 <- 
  dplyr::bind_rows(tsDS, .id = "country") %>%
  dplyr::left_join(dplyr::bind_rows(X, .id = "country")) %>%
  dplyr::left_join(deriv.pred %>% dplyr::select(country, deriv, seqwk)) %>%
  dplyr::select(country, seqwk, cases, fitcases, deriv) %>%
  dplyr::mutate(year = ifelse((seqwk > 0L & seqwk <= 52L), "2017",
                              ifelse((seqwk > 52L & seqwk <= 104), "2018",
                                     ifelse((seqwk > 104L & seqwk <= 156L), "2019",
                                            ifelse((seqwk > 156L & seqwk <= 208L), "2020",
                                                   ifelse((seqwk > 208L & seqwk <= 260L), "2021",
                                                          ifelse((seqwk > 260L & seqwk <= 312L), "2022",
                                                                 ifelse((seqwk > 312L & seqwk <= 364L), "2023", NA_character_))))))))

#====================================================================

#summarise by precovid vs 2021/22 and 2022/23 seasons
rsv_intens2 <-
  rsv_intens2 %>%
  dplyr::mutate(covper = ifelse(year == "2023", "y2023",
                                ifelse(year == "2022", "y2022",
                                       ifelse(year == "2021", "y2021",
                                              ifelse(year == "2017" | year == "2018" | year == "2019", "precov", NA_character_))))) %>% 
  dplyr::filter(!is.na(covper)) %>%
  dplyr::select(everything(), -year) %>%
  dplyr::group_by(country, covper) %>%
  dplyr::summarise(intensity = mean(intensity, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim_zone)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct(.keep_all = TRUE))

rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak|rsv_growth|rsv_intens", ls(), value = TRUE, invert = TRUE))
