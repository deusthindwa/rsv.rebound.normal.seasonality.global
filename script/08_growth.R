#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#RSV GROWTH RATE COMPUTATION
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

#create empty list to store GAM models, derivatives & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv1 <- list()
growth2 <- list()

#run the GAM models via random effects maximum likelihood (REML)
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, extract fitted values, compute 1st derivatives
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% #fitted values
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n()))
  
  deriv1[[i]] = data.frame(fderiv = diff(log(tsDS[[i]]$fitcases))/diff(tsDS[[i]]$seqwk)) %>% #first derivative of log fitted spline == growth rate
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n())) 
  }

#unlist all the datasets and keep value and location of maximum first derivative
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")
deriv1 <- dplyr::bind_rows(deriv1, .id = "country")

growth2 <- 
  deriv1 %>% 
  dplyr::filter(fderiv > 0) %>%
  dplyr::mutate(intcat = factor(cumsum(c(1, abs(seqwk[-length(seqwk)] - seqwk[-1]) > 1))),
                year = factor(year(date))) %>%
  dplyr::filter(year != "2023") %>%
  dplyr::group_by(intcat) %>%
  dplyr::mutate(counter = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(counter != 1) %>%
  
  dplyr::group_by(country, intcat, year) %>%
  dplyr::mutate(max_fderiv = max(fderiv, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(fderiv == max_fderiv) %>%
  
  #fixing issues per observation to ensure accuracy of growth rates
  dplyr::mutate(year = if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa") & year == "2020", "No_Oubreak", year)) %>%
  dplyr::filter(year != "No_Oubreak") %>%
  
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 300, "2021", year)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 212, "2020", year)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 53, "2018", year)) %>%
  
  dplyr::mutate(year = if_else(country == "France" & seqwk == 210, "2018", year)) %>%
  
  dplyr::mutate(year = if_else(country == "Iceland" & (seqwk == 158 | seqwk == 209), "2019", 
                               if_else(country == "Iceland" & (seqwk == 302 | seqwk == 266), "2023", 
                                       if_else(country == "Iceland" & (seqwk == 246), "2022", year)))) %>%
  
  dplyr::mutate(year = if_else(country == "Denmark" & (seqwk == 210 | seqwk == 202), "2024", year)) %>%
  dplyr::mutate(year = if_else(country == "Mongolia" & (seqwk == 262), "2019", year)) %>%
  dplyr::mutate(year = if_else(country == "Netherlands" & (seqwk == 306), "2023", year)) %>%
  dplyr::mutate(year = if_else(country == "South Africa" & (seqwk == 261), "2022", year)) %>%
  dplyr::mutate(year = if_else(country == "Sweden" & (seqwk == 210), "2022", year)) %>%
  dplyr::mutate(fderiv = if_else(country == "Paraguay" & (seqwk == 302), 0.1, fderiv)) %>%
  dplyr::mutate(fderiv = if_else(country == "India" & seqwk == 312, 0.01, fderiv)) %>%
  
  
  dplyr::mutate(year = if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == "2022", "2023", 
                               if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == "2021", "2022", 
                                       if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "South Africa") & year == "2020", "2021", year)))) %>%
  
  dplyr::group_by(country, year) %>%
  dplyr::filter(max_fderiv == max(fderiv)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year != 2024)

#compute timing of growth rates around 52 weeks
 growth2 <-
   growth2 %>% 
   dplyr::mutate(loc = week(date),
                 loc = if_else(loc == 53, 52, loc),
                 seqwk2 = seqwk) %>% 
   dplyr::select(country, year, seqwk, seqwk2, loc, fderiv, date)

growth1 <- 
  dplyr::left_join(tsDS, X) %>%
  dplyr::left_join(growth2 %>% dplyr::select(everything(), -year, -loc, -date))

#ggplot of RSV time series and growth rate values
G0 <-
  ggplot() +
  geom_line(data = deriv1, aes(x = seqwk, y = fderiv), size = 1.5, color = "black") + # GAM fitted curve
  geom_point(data = growth1, aes(x = seqwk2, y = fderiv), color = "red", shape = 4, size = 2,  stroke = 2) + #onset times
  geom_vline(xintercept = 171, linetype = "dotted", color = "black", size = 1.5) +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "red", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1, 325, 52), limits = c(0, 325)) +
  labs(x = "Reporting weeks between 2017 and 2023", y = "Estimated growth rate of RSV epidemic (Max 1st derivative of log fitted spline)", title ="RSV GROWTH RATE") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig4_growth.png"),
       plot = G0,
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV GROWTH RATE COMPARISONS BY OVERALL AND HEMISPHERE
#====================================================================

#create different RSV waves in different countries
growth2 <-  
  growth2 %>% 
  dplyr::mutate(wave = if_else(year == "2017" | year == "2018" | year == "2019", "precov", 
                               if_else(year == "2021", "wave1",
                                       if_else(year == "2022", "wave2", "wave3"))))

growth2x <- growth2 #peak2x dataset used for regression

#calculate mean for pre-Covid period (2017, 2018, 2019)
growth2 <-
  growth2 %>%
  dplyr::select(country, wave, fderiv) %>%
  dplyr::group_by(country, wave) %>%
  dplyr::mutate(fderiv = mean(fderiv)) %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = wave, values_from = fderiv) %>%
  dplyr::select(everything(), -row) %>%
  dplyr::filter(!is.na(wave1), !is.na(wave2)) %>%
  data_frame() %>%
  ungroup()

#1)growth rate by overall status (pearson correlation coefficient)
growth_overall <-
  left_join(
  growth2 %>%
    dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                  w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
  growth2 %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3)))) %>%
  
  dplyr::mutate(cat = "Overall")

#2)growth rate by hemisphere (pearson correlation coefficient)
growth_hemi <-
  growth2 %>%
  inner_join(rsv_all %>% dplyr::select(country, hemi) %>% dplyr::distinct())

scatterXY <- list()
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  scatterXY[[i]] =
    left_join(
      growth_hemi %>%
        dplyr::filter(hemi == i) %>%
        dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                      w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
      growth_hemi %>%
        dplyr::filter(hemi == i) %>%
        dplyr::select(country, precov, wave3) %>%
        dplyr::filter(!is.na(wave3)) %>%
        dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3))))
}

growth_hemi <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-hemi)

G1 <-
  dplyr::bind_rows(growth_overall, growth_hemi) %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.6, y = 0.1, label = paste0("r = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV growth rate", title = "GROWTH RATE") +
  theme(legend.position = "NONE", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

G2 <-
  dplyr::bind_rows(growth_overall, growth_hemi) %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.6, y = 0.1, label = paste0("r = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV growth rate", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

G3 <-
  dplyr::bind_rows(growth_overall, growth_hemi) %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(data = dplyr::bind_rows(growth_overall, growth_hemi) %>% dplyr::filter(!is.na(w3corr)), aes(x = 1.6, y = 0.1, label = paste0("r = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV growth rate", title = "") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#RSV ONSET TIMING COMPARISONS BY CLIMATE ZONE
#====================================================================

#3)onset timing by climate zone (circular correlation coefficient)
growth_clim <-
  growth2 %>%
  inner_join(climate %>% dplyr::select(country, clim) %>% dplyr::distinct())

scatterXY <- list()
for (i in c("Tropical", "Temperate")) {
  scatterXY[[i]] =
    left_join(
      growth_clim %>%
        dplyr::filter(clim == i) %>%
        dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                      w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
      growth_clim %>%
        dplyr::filter(clim == i) %>%
        dplyr::select(country, precov, wave3) %>%
        dplyr::filter(!is.na(wave3)) %>%
        dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3))))
}
growth_clim <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-clim) %>%
  dplyr::mutate(cat = ifelse(cat == "Tropical", "(Sub)tropical", cat))

G4 <-
  growth_clim %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.6, y = 0.1, label = paste0("r = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV growth rate", title = "GROWTH RATE") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

G5 <-
  growth_clim %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.6, y = 0.1, label = paste0("r = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV growth rate", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

G6 <-
  growth_clim %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(data = growth_clim %>% dplyr::filter(!is.na(w3corr)), aes(x = 1.6, y = 0.1, label = paste0("r = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 2)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV growth rate", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#====================================================================

# #delete unnecessary files
rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))
