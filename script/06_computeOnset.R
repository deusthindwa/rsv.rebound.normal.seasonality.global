#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression
#(code adapted from Gigi's - https://github.com/Gigi112/RSV_tutorials/blob/main/2_RSV_timing_explain/RSV_timing_tutorial.Rmd)

#====================================================================

#definition of RSV onset
#disease onset calculations and intuition
#when the first derivative is positive, the original curve is increasing. 
#in epidemic, this corresponds to the early stage of an outbreak as cases are increasing. 
#when the second derivative reach its maximum in the segment that the first derivative is positive, it means that the growth rate of the increasing trend reach its maximum. 
#this fits for the definition of the starting point of an disease outbreak.   

#====================================================================

#split the dataset by country each having country name, yr, wk (up to 52 weeks) and cases from 2017-2019
#we restrict weeks to 52 (and take mean cases of week 52 and 53 and assign to week 52)

#drop country and rename within US regions as country to adapt the code below
W <-
  dplyr::left_join(rsv_all, climate) %>%
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States", 
                               "United States North East", "United States West", "United States South", "United States Mid West"),
                wk_scale == "inverted") %>%
  dplyr::select(country, yr, wk, cases) %>%
  dplyr::arrange(country, yr, wk) %>%
  dplyr::filter(wk <=52) %>%
  dplyr::mutate(yr = case_when((wk %in% 24:52 & yr == 2017) | (wk %in% 1:23 & yr == 2018) ~ "2017/18",
                               (wk %in% 24:52 & yr == 2018) | (wk %in% 1:23 & yr == 2019) ~ "2018/19",
                               (wk %in% 24:52 & yr == 2019) | (wk %in% 1:23 & yr == 2020) ~ "2019/20",
                               (wk %in% 24:52 & yr == 2020) | (wk %in% 1:23 & yr == 2021) ~ "2020/21",
                               (wk %in% 24:52 & yr == 2021) | (wk %in% 1:23 & yr == 2022) ~ "2021/22",
                               (wk %in% 24:52 & yr == 2022) | (wk %in% 1:23 & yr == 2023) ~ "2022/23",
                               (wk %in% 24:52 & yr == 2023) | (wk %in% 1:23 & yr == 2024) ~ "2023/24",
                               (wk %in% 24:52 & yr == 2024) | (wk %in% 1:23 & yr == 2025) ~ "2024/25",
                               (wk %in% 24:52 & yr == 2025) | (wk %in% 1:23 & yr == 2026) ~ "2025/26",
                               (wk %in% 24:52 & yr == 2026) | (wk %in% 1:23 & yr == 2027) ~ "2026/27",
                               TRUE ~ NA_character_)) %>%
  dplyr::filter(!is.na(yr)) %>%
  
  #for temperate countries, ensure seasonality starts from week 24 and ensure annual cases are 100+ only
  dplyr::group_by(country, yr) %>%
  mutate(wk = seq.int(from = 24, by = 1, length.out = n()),
         tcases = sum(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(yr != "2020/21",  tcases >250) %>% #filter out covid period
  dplyr::select(country, yr, wk, cases)

#split the dataset by country/regionUS and year to form list of datasets
X <- 
  W %>%  
  base::split(list(.$country, .$yr))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[base::unlist(lapply(X, nrow) != 0)]

#function to calculate derivative and assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create empty list to store GAM models
Gmodels <- list()

#run the GAM models where smoothing parameter/knots are automatically selected via a cross validation method
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = wk, bs = "ps"),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]]
  )
}

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2017
cases.samples <- list()
onset.samples_temp <- list()

#runs simulations on an outbreak GAM to estimate time series outbreak outcomes 
#and returns estimated time series outcomes for each simulation.
for (i in names(X)){
  cases.samples[[i]] = pspline.sample.timeseries(Gmodels[[i]], 
                                                 data.frame(wk = t), 
                                                 pspline.outbreak.cases, 
                                                 samples = 500)
}

#iterate for each country and year, compute first and second derivative
for (i in names(X)){
  onset.samples_temp[[i]] = cases.samples[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$wk), # calculate the first derivative
                              wk = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            wk = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$wk%in%indicator$wk,]
      
      onset = dtime[second_d_test$wk[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$wk == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#compute mean, low and upper 95%CI for onset by country and preCOVID-19 vs each year after COVID-19 year
onset_temp <-
  dplyr::bind_rows(onset.samples_temp, .id = "id") %>%
  dplyr::mutate(yr = str_sub(id, -7, -1),
                country = word(id, 1, sep = "\\.")) %>%
  dplyr::select(everything(), -id) %>%
  dplyr::mutate(covper = if_else(yr == "2017/18" | yr == "2018/19" | yr == "2019/20", "preCOVID-19",
                          if_else(yr == "2021/22", "2021/22",
                                  if_else(yr == "2022/23", "2022/23",
                                          if_else(yr == "2023/24", "2023/24",
                                                  if_else(yr == "2024/25", "2024/25",
                                                          if_else(yr == "2025/26", "2025/26", NA_character_))))))) %>%
  dplyr::filter(!is.na(covper)) %>%
  dplyr::group_by(country, covper) %>%
  dplyr::summarise(epiwk = mean(onset, na.rm = TRUE),
                   l_epiwk = quantile(onset, 0.025),
                   u_epiwk = quantile(onset, 0.975)) %>%
  dplyr::ungroup()

#====================================================================
#====================================================================

#drop country and rename within US regions as country to adapt the code below
W <-
  dplyr::left_join(rsv_all, climate) %>%
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States", 
                               "United States North East", "United States West", "United States South", "United States Mid West"), 
                wk_scale == "normal") %>%
  dplyr::select(country, yr, wk, cases) %>%
  dplyr::group_by(country, yr) %>%
  dplyr::mutate(tcases = sum(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(wk <=52, yr != 2020, yr != 2023, tcases >99) %>% 
  dplyr::select(country, yr, wk, cases) %>%
  dplyr::arrange(country, yr, wk)

#split the dataset by country/regionUS and year to form list of datasets
X <- 
  W %>%  
  split(list(.$country, .$yr))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[unlist(lapply(X, nrow) != 0)]

#function to calculate derivative and assist derivative calculation
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#generate time sequence
t = seq(0.5, 52.5, 0.01)
dtime = seq(0.5, 52.5, 0.01)

#create empty list to store GAM models
Gmodels <- list()

#run the GAM models where smoothing parameter/knots are automatically selected via a cross validation method
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = wk, bs = "ps"),
                      family = poisson,
                      method = "REML",
                      control = list(maxit =100000),
                      data = X[[i]]
  )
}

#uncertainty interval of RSV trajectory and onset timing
#create list to store case and onset samples for 2017
cases.samples <- list()
onset.samples_trop <- list()

#runs simulations on an outbreak GAM to estimate time series outbreak outcomes 
#and returns estimated time series outcomes for each simulation.
for (i in names(X)){
  cases.samples[[i]] = pspline.sample.timeseries(Gmodels[[i]], 
                                                 data.frame(wk = t), 
                                                 pspline.outbreak.cases, 
                                                 samples = 500)
}

#iterate for each country and year, compute first and second derivative
for (i in names(X)){
  onset.samples_trop[[i]] = cases.samples[[i]] %>% 
    group_by(pspline.sample) %>% # for each sample do the following
    do((function(data){
      deriv.pred = data.frame(deriv = diff(data$cases)/diff(data$wk), # calculate the first derivative
                              wk = c(1:length(diff(t)))) 
      
      second_d = data.frame(second.deriv = deriv(middle_pts(dtime), deriv(dtime, data$cases)), # calculate the second derivative
                            wk = c(1:(length(diff(t))-1))) 
      
      indicator = deriv.pred[which(deriv.pred$deriv > 0),]  # only look at second derivatives in the increasing segment (first derivative > 0 )
      second_d_test <- second_d[second_d$wk%in%indicator$wk,]
      
      onset = dtime[second_d_test$wk[second_d_test$second.deriv == max(second_d_test$second.deriv)]] #find when second derivative of smooth functions reached its maximum 
      
      data.frame(
        pspline.sample = tail(data$pspline.sample, 1),
        onset = onset,
        cases = data$cases[which(data$wk == onset)]) #find the case number when the second derivative reach its maximum
    })(.)) %>%
    ungroup()
}

#compute mean, low and upper 95%CI for onset by country and preCOVID-19 vs each year after COVID-19 year
onset_trop <-
  dplyr::bind_rows(onset.samples_trop, .id = "id") %>%
  dplyr::mutate(yr = str_sub(id, -4, -1),
                country = word(id, 1, sep = "\\.")) %>%
  dplyr::select(everything(), -id) %>%
  dplyr::mutate(covper = if_else(yr == "2017" | yr == "2018" | yr == "2019", "preCOVID-19",
                          if_else(yr == "2021", "2021",
                                  if_else(yr == "2022", "2022",
                                          if_else(yr == "2023", "2023",
                                                  if_else(yr == "2024", "2024",
                                                          if_else(yr == "2025", "2025", NA_character_))))))) %>%
  dplyr::filter(!is.na(covper)) %>%
  dplyr::group_by(country, covper) %>%
  dplyr::summarise(epiwk = mean(onset, na.rm = TRUE),
            l_epiwk = quantile(onset, 0.025),
            u_epiwk = quantile(onset, 0.975)) %>%
  dplyr::ungroup()

#====================================================================
#MERGE THE RSV ONSET DATASETS FOR TEMPERATE & TROPICAL
#====================================================================

rsv_onset <-
  dplyr::rows_append(onset_temp, onset_trop) %>% 
  dplyr::left_join(climate %>% dplyr::select(country, clim_zone)) %>%
  dplyr::left_join(rsv_all %>% dplyr::select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  dplyr::mutate(covper = if_else(covper == "2021/22", "y2021",
                                 if_else(covper == "2022/23", "y2022",
                                         if_else(covper == "2021", "y2021",
                                                 if_else(covper == "2022", "y2022", 
                                                         if_else(covper == "preCOVID-19", "precov", NA_character_))))))

#only consider countries with all the data e.g., preCovid, 2021, and 2022
rsv_onset <- 
  rsv_onset %>% 
  dplyr::left_join(
    rsv_onset %>%
      dplyr::group_by(country) %>%
      tally()) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(n == 3)

#delete all temporary variables and keep usable objects
rm(list = grep("rsv_all|climate|rsv_onset", ls(), value = TRUE, invert = TRUE))
