#written by Deus
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#d1 <-  ~jsonlite::read_json("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT", simplifyVector = T))

#read the WHO RSV update file into R
rsv <- runIfExpired('who_rsv', maxage = 168, ~read.csv(curl("https://frontdoor-l4uikgap6gz3m.azurefd.net/FLUMART/VIW_FNT?$format=csv")))

#get the required variables
rsvds <-
  rsv %>%
  dplyr::filter(!is.na(RSV)) %>%
  dplyr::select(WHOREGION, FLUSEASON, HEMISPHERE, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE, ORIGIN_SOURCE, RSV) %>%
  dplyr::arrange(WHOREGION, COUNTRY_AREA_TERRITORY, MMWR_WEEKSTARTDATE) %>%
  dplyr::rename("region" = WHOREGION,
                "fluseas" = FLUSEASON,
                "hemi" = HEMISPHERE,
                "country" = COUNTRY_AREA_TERRITORY,
                "date"= MMWR_WEEKSTARTDATE,
                "sentin" = ORIGIN_SOURCE,
                "cases" = RSV) %>%
  dplyr::mutate(date = lubridate::ymd(date),
                wk = lubridate::week(date),
                mon = lubridate::month(date),
                yr = lubridate::year(date),
                sentin = factor(sentin),
                hemi = factor(hemi),
                fluseas = factor(fluseas))
  