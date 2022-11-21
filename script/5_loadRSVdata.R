#written by Deus
#18/11/2022
#global reemergence of RSV onset, duration and peak

#d1 <-  ~jsonlite::read_json("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT", simplifyVector = T))

#read the WHO RSV update file into R
rsv <- runIfExpired('who_rsv', maxage = 168, ~read.csv(curl("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT?$format=csv")))

#data munging to get the required variables
rsvds <-
  rsv %>%
  dplyr::filter(!is.na(RSV)) %>%
  dplyr::select(WHOREGION, FLUSEASON, HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, MMWR_YEAR, MMWR_WEEK, RSV) %>%
  arrange(WHOREGION, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE)

  
rsvds %>%
  write_csv(x = rsvds, file = here("data", "RSVglobal.csv"))
  
  