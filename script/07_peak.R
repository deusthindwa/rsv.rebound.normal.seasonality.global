#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#PEAK TIMING OF RSV CASES IN EACH COUNTRY
#====================================================================

#split the dataset by country to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
  dplyr::arrange(country, yr, wk, cases) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = week(date), by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, seqwk, cases, country) %>%
  base::split(list(.$country))

#delete empty country data frames from list X
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
peakDS <- list()
tsDS <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 35),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, compute timing of the peak and the peak value
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% #fitted values
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n()))
  
  peakDS[[i]] = data.frame(findpeaks(abs(tsDS[[i]]$fitcases), MinPeakHeight = 2, MinPeakDistance = 1, MinPeakWidth = 1, MaxPeakWidth = Inf, DoubleSided = FALSE))
}

#unlist all the datasets and keep location of peak
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")

#create peak dataset but first correct the starting sequence number for southern (w2) and northern hemisphere (w24)
peak2 <-
  dplyr::bind_rows(peakDS, .id = "country") %>% 
  dplyr::select(country, pks, loc) %>%
  dplyr::mutate(loc = if_else(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", "Peru", "South Africa"), loc+1, loc+23)) %>%
  dplyr::rename("seqwk" = "loc") %>%
  left_join(tsDS) %>%
  dplyr::filter(!is.na(year(date)))

#compute circular timing around 52 weeks
peak2 <-
  peak2 %>% 
  dplyr::mutate(loc = week(date),
                loc = if_else(loc == 53, 52, loc),
                year = year(date),
                seqwk2 = seqwk) %>% 
  dplyr::select(country, date, year, seqwk, seqwk2, loc, pks) 

#fixing issues per observation to ensure accuracy of peak timing
peak2 <- 
peak2 %>%
  dplyr::mutate(year = if_else(country == "Mongolia" & seqwk == 56, 2017, year)) %>%
  dplyr::mutate(year = if_else(country == "Mexico" & seqwk == 109, 2018, year)) %>%
  dplyr::mutate(year = if_else(country == "United States" & seqwk == 55, 2017,
                               if_else(country == "United States" & seqwk == 106, 2018, year))) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & (seqwk == 309 |  seqwk == 328), 2024,
                               if_else(country == "Brazil" & seqwk == 221, 2020, year))) %>%
  dplyr::mutate(year = if_else(country == "India" & seqwk == 264, 2021, year)) %>%
  dplyr::mutate(year = if_else(country == "Colombia" & seqwk == 213, 2020, year)) %>%
  dplyr::mutate(year = if_else(country == "France" & seqwk == 221, 2020, year)) %>%
  dplyr::mutate(year = if_else(country == "Iceland" & seqwk == 313, 2023, year)) %>%
  dplyr::mutate(year = if_else(country == "Japan" & seqwk == 164, 2019, year)) %>%
  dplyr::mutate(year = if_else(country == "Qatar" & seqwk == 328, 2024, year)) %>%
  
  dplyr::group_by(country, year) %>%
  dplyr::filter(pks == max(pks)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year != 2024) %>%

dplyr::mutate(year = if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == 2022, 2023, 
                             if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == 2021, 2022, 
                                     if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == 2020, 2021, year)))) %>%
  
  dplyr::mutate(year = if_else(country %in% c("Iceland", "Netherlands", "Canada", "Denmark", "Germany", "Hungary", "Portugal", "Sweden") & year == 2018, 2017,
                               if_else(country %in% c("Iceland", "Netherlands", "Canada", "Denmark", "Germany", "Hungary", "Portugal", "Sweden") & year == 2019, 2018,
                                       if_else(country %in% c("Iceland", "Netherlands", "Canada", "Denmark", "Germany", "Hungary", "Portugal", "Sweden") & year == 2020, 2019, year)))) %>%
  
  dplyr::mutate(year = if_else(country %in% c("Hungary") & year == 2023, 2022, year)) %>%
  dplyr::mutate(year = if_else(country %in% c("Mongolia") & year == 2022, 2021, 
                               if_else(country %in% c("Mongolia") & year == 2023, 2022, year)),
                year = factor(year))

peak1 <- 
  dplyr::left_join(tsDS, X) %>%
  dplyr::left_join(peak2 %>% dplyr::select(everything(), -year, -loc, -date))

#ggplot of RSV time series and peak values
P0 <-
  ggplot() +
  geom_line(data = peak1, aes(x = seqwk, y = fitcases), size = 1.5, color = "black") + # GAM fitted curve
  geom_point(data = peak2, aes(x = seqwk2, y = pks), color = "darkgreen", shape = 4, size = 2,  stroke = 3) + #onset times
  geom_vline(xintercept = 171, linetype = "dotted", color = "black", size = 1.5) +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "darkgreen", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1, 325, 52), limits = c(0, 325)) +
  labs(x = "Reporting weeks between 2017 and 2023", y = "GAM fitted RSV cases and estimated outbreak peak", title ="RSV PEAK TIMING") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig3_peak.png"),
       plot = P0,
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV PEAK TIMING COMPARISONS BY OVERALL AND HEMISPHERE
#====================================================================

#create different RSV waves in different countries
peak2 <-  
  peak2 %>% 
  dplyr::mutate(wave = if_else(year == "2017" | year == "2018" | year == "2019", "precov",
                               if_else(year == "2021", "wave1",
                                       if_else(year == "2022", "wave2", "wave3"))))

peak2x <- peak2 #peak2x dataset used for regression

#calculate circular mean for pre-Covid period (2017, 2018, 2019)
peak2 <-
  peak2 %>%
  dplyr::select(country, wave, loc) %>%
  dplyr::group_by(country, wave) %>%
  dplyr::mutate(loc = (circular::mean.circular((2*pi/52)*(loc-1))/(2*pi/52) + 52) %% 52) %>% #52 weeks per year (circular)
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = wave, values_from = loc) %>%
  dplyr::select(everything(), -row) %>%
  dplyr::filter(!is.na(wave1), !is.na(wave2)) %>%
  data_frame() %>%
  ungroup() %>%
  
  dplyr::mutate(precov = circular(precov, units = "degrees", template = "geographics", modulo = "pi"),
                wave1 =  circular(wave1, units = "degrees", template = "geographics", modulo = "pi"),
                wave2 = circular(wave2, units = "degrees", template = "geographics", modulo = "pi"),
                wave3 = circular(wave3, units = "degrees", template = "geographics", modulo = "pi"))

#bootstraping dataset
peakb <- 
  peak2 %>% 
  dplyr::left_join(rsv_all %>% select(hemi, country) %>% distinct()) %>%
  dplyr::left_join(climate %>% select(country, clim) %>% distinct())

#1)onset timing by overall status (circular correlation coefficient)
peak_overall <-
  peak2 %>%
  dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),
                
                w1L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w1U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w2L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w2U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w3L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w3U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                cat = "Overall") %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

#2)peak timing by hemisphere (circular correlation coefficient)
peak_hemi <-
  peak2 %>%
  inner_join(rsv_all %>% dplyr::select(country, hemi) %>% dplyr::distinct())

scatterXY <- list()
for (j in c("Northern hemisphere", "Southern hemisphere")) {
  scatterXY[[j]] =
    peak_hemi %>%
    dplyr::filter(hemi == j) %>%
    dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                  w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                  w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),
                  
                  w1L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w1U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w2L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w2U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w3L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3, hemi) %>% dplyr::filter(!is.na(wave3), hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w3U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3, hemi) %>% dplyr::filter(!is.na(wave3), hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2))
}
peak_hemi <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-hemi) %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

P1 <-
  dplyr::bind_rows(peak_overall, peak_hemi) %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV peak timing", title = "PEAK TIMING") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

P2 <-
  dplyr::bind_rows(peak_overall, peak_hemi) %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV peak timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

P3 <-
  dplyr::bind_rows(peak_overall, peak_hemi) %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w3L,",",w3U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV peak timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#RSV PEAK TIMING COMPARISONS BY CLIMATE ZONE
#====================================================================

#3)peak timing by climate zone (circular correlation coefficient)
peak_clim <-
  peak2 %>%
  inner_join(climate %>% dplyr::select(country, clim) %>% dplyr::distinct())

scatterXY <- list()
for (j in c("Tropical", "Temperate")) {
  scatterXY[[j]] =
    peak_clim %>%
    dplyr::filter(clim == j) %>%
    dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                  w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                  w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),
                  
                  w1L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w1U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave1, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w2L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w2U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave2, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w3L = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3, clim) %>% dplyr::filter(!is.na(wave3), clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w3U = round(boot.ci(boot(as.data.frame(peakb %>% dplyr::select(precov, wave3, clim) %>% dplyr::filter(!is.na(wave3), clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2))
}
peak_clim <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-clim) %>%
  dplyr::mutate(cat = ifelse(cat == "Tropical", "(Sub)tropical", cat)) %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

P4 <-
  peak_clim %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV peak timing", title = "PEAK TIMING") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

P5 <-
  peak_clim %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV peak timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

P6 <-
  peak_clim %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4.5, label = paste0("c = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w3L,",",w3U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV peak timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#====================================================================

# #delete unnecessary files
rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))
