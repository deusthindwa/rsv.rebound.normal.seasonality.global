#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#====================================================================

#country by hemisphere
rsv_onset %>%
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  distinct(country, .keep_all = TRUE) %>%
  dplyr::group_by(hemi) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#country by climate zone
rsv_onset %>%
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  distinct(country, .keep_all = TRUE) %>%
  dplyr::group_by(clim_zone) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#country by region
rsv_onset %>%
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  distinct(country, .keep_all = TRUE) %>%
  dplyr::group_by(region) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#country by out of season
rsv_onset %>%
  left_join(climate %>% dplyr::select(country, out_seas21)) %>%
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  distinct(country, .keep_all = TRUE) %>%
  dplyr::group_by(out_seas21) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)


