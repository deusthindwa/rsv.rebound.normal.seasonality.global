#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#Onset
onset2 %>%
  dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
  dplyr::mutate(w1corr = abs(round((circular::cor.circular(precov, wave1))[1], digits = 3)),
                w2corr = abs(round((circular::cor.circular(precov, wave2))[1], digits = 3)),
                w3corr = abs(round((circular::cor.circular(precov, wave3))[1], digits = 3)),
                cat = "Overall")

#Peak
peak2 %>%
  dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
  dplyr::mutate(w1corr = abs(round((circular::cor.circular(precov, wave1))[1], digits = 3)),
                w2corr = abs(round((circular::cor.circular(precov, wave2))[1], digits = 3)),
                w3corr = abs(round((circular::cor.circular(precov, wave3))[1], digits = 3)),
                cat = "Overall")

#Growth
left_join(
  growth2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                  w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
  growth2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3)))) %>%
  
  dplyr::mutate(cat = "Overall")

#Intensity
left_join(
  intense2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                  w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
  intense2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3)))) %>%
  dplyr::mutate(cat = "Overall")
