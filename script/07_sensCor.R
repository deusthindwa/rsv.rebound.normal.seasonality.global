#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#Onset
onset2 %>%
  dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
  dplyr::mutate(w1corr = round((circular::cor.circular(sin(precov), sin(wave1)))[1], digits = 2),
                w2corr = round((circular::cor.circular(sin(precov), sin(wave2)))[1], digits = 2),
                w3corr = round((circular::cor.circular(sin(precov), sin(wave3)))[1], digits = 2),
                w1L = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w1U = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w2L = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w2U = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w3L = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w3U = round(boot.ci(boot(as.data.frame(onset2 %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L),
                cat = "Overall")

#Peak
peak2 %>%
  dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
  dplyr::mutate(w1corr = round((circular::cor.circular(sin(precov), sin(wave1)))[1], digits = 2),
                w2corr = round((circular::cor.circular(sin(precov), sin(wave2)))[1], digits = 2),
                w3corr = round((circular::cor.circular(sin(precov), sin(wave3)))[1], digits = 2),
                w1L = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w1U = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w2L = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w2U = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w3L = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[2], digits = 2),
                w3U = round(boot.ci(boot(as.data.frame(peak2 %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(sin(i[d,1]), sin(i[d,2])), R=1000), type = "norm")$normal[3], digits = 2),
                w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L),
                cat = "Overall")

#Growth
left_join(
  growth2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::mutate(w1corr = round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 2),
                  w2corr = round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 2),
                  w1L = round((stats::cor.test(wave1, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w1U = round((stats::cor.test(wave1, precov, method = c("pearson"))$conf.int[2]), digits = 2),
                  w2L = round((stats::cor.test(wave2, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w2U = round((stats::cor.test(wave2, precov, method = c("pearson"))$conf.int[2]), digits = 2)),
  growth2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 2),
                  w3L = round((stats::cor.test(wave3, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w3U = round((stats::cor.test(wave3, precov, method = c("pearson"))$conf.int[2]), digits = 2))) %>%
  
  dplyr::mutate(cat = "Overall")

#Intensity
left_join(
  intense2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::mutate(w1corr = round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 2),
                  w2corr = round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 2),
                  w1L = round((stats::cor.test(wave1, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w1U = round((stats::cor.test(wave1, precov, method = c("pearson"))$conf.int[2]), digits = 2),
                  w2L = round((stats::cor.test(wave2, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w2U = round((stats::cor.test(wave2, precov, method = c("pearson"))$conf.int[2]), digits = 2)),
  intense2 %>%
    dplyr::filter(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "Netherlands", "South Africa")) %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 2),
                  w3L = round((stats::cor.test(wave3, precov, method = c("pearson"))$conf.int[1]), digits = 2),
                  w3U = round((stats::cor.test(wave3, precov, method = c("pearson"))$conf.int[2]), digits = 2))) %>%
  dplyr::mutate(cat = "Overall")
