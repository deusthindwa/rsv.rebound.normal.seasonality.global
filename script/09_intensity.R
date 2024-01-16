#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#RSV INTENSITY COMPUTATION
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
intense2 <- list()

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
  
  deriv1[[i]] = data.frame(fderiv = diff(log(tsDS[[i]]$fitcases+1))/diff(tsDS[[i]]$seqwk)) %>% #first derivative of log fitted spline == growth rate
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n()))
  }

#unlist all the datasets and keep value and location of maximum first derivative
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")
deriv1 <- dplyr::bind_rows(deriv1, .id = "country")

intense2 <-
  deriv1 %>%
  dplyr::filter(fderiv >0) %>%
  dplyr::mutate(intcat = cumsum(c(1, abs(seqwk[-length(seqwk)] - seqwk[-1]) > 1)), year = factor(year(date))) %>%
  dplyr::filter(year != "2023") %>%
  dplyr::group_by(intcat) %>%
  dplyr::mutate(counter = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(counter != 1) %>%
  dplyr::select(everything(), -counter)

#fixing issues per observation to ensure accuracy of intensity (area under the curve)
intense2 <-
intense2 %>%
  dplyr::mutate(area = if_else(country == "Australia" & (intcat == 6 | intcat == 9), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Brazil" & (intcat == 15 | intcat == 19), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Canada" & (seqwk >=193 & seqwk <=225), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Colombia" & (intcat == 30 | intcat == 34), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Costa Rica" & (intcat == 38 | intcat == 41), 0, 1)) %>% dplyr::filter(area == 1) %>% 
  dplyr::mutate(area = if_else(country == "Denmark" & (intcat == 42 | intcat == 46), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "France" & (intcat == 49), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Germany" & (intcat == 58), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Hungary" & (intcat == 62 | intcat == 65), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Iceland" & (intcat == 68), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "India" & (intcat == 74 | intcat == 75 | intcat == 78 | intcat == 81), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Ireland" & (intcat == 85), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Japan" & (intcat == 91 | intcat == 93), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Mexico" & (intcat == 96 | intcat == 99), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Mongolia" & (intcat == 102 | intcat == 105), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Netherlands" & (intcat == 112), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Paraguay" & (intcat == 127 | intcat == 130 | intcat == 131 | intcat == 134), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Portugal" & (seqwk >=158 & seqwk <=210), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Qatar" & (intcat == 149), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Scotland" & (intcat == 156 | intcat == 153), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "South Africa" & (intcat == 160 | intcat == 166), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Spain" & (intcat == 170), 0, 1)) %>% dplyr::filter(area == 1) %>%
  dplyr::mutate(area = if_else(country == "Sweden" & (intcat == 174 | intcat == 177), 0, 1)) %>% dplyr::filter(area == 1)

#take the integral of the positive log derivative series (area under the curve)
intense2 <- 
  intense2 %>%  
  dplyr::group_by(country, intcat) %>%
  dplyr::mutate(intensity = stats::integrate(splinefun(seqwk, fderiv), range(seqwk)[1], range(seqwk)[2])$value) %>% #OR 'approxfun' for linear interpolation via trapezoids
  dplyr::ungroup()

#generate additional variables for merging intensity to other datasets
 intense2 <-
   intense2 %>%
   dplyr::mutate(seqwk2 = seqwk) %>%
   dplyr::select(country, intcat, year, seqwk, seqwk2, fderiv, intensity, date)
 
 #intense2x <- intense2 #peak2x dataset used for regression
intense1 <- 
  dplyr::left_join(tsDS, X) %>%
  dplyr::left_join(intense2 %>% dplyr::select(everything(), -year, -date))

#ggplot of RSV time series and intensity values
I0 <-
  intense1 %>%
  dplyr::mutate() %>%
  ggplot() +
  geom_line(data = intense1, aes(x = seqwk, y = fderiv), size = 1.5, color = "yellow4") + # GAM fitted curve
  geom_vline(xintercept = 171, linetype = "dotted", color = "black", size = 1.5) +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "yellow4", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1, 325, 52), limits = c(0, 325)) +
  labs(x = "Reporting weeks between 2017 and 2023", y = "Estimated intensity of RSV epidemic (Integral of the positive 1st derivative of log fitted spline)", title ="RSV INTENSITY") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig5_intensity.png"),
       plot = I0,
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV INTENSITY COMPARISONS BY OVERALL AND HEMISPHERE
#====================================================================

#regenerate the intensity dataset with correct waves
intense2 <-
  intense2 %>%
  dplyr::distinct(country, intcat, .keep_all = TRUE) %>%
  dplyr::mutate(wave = if_else(year == "2017" | year == "2018" | year == "2019", "precov", year)) %>%
  dplyr::mutate(wave = if_else(country == "Argentina" & intcat == 5, "2021",
                       if_else(country == "Australia" & intcat == 12, "2022",
                       if_else(country == "Canada" & intcat == 24, "2020",
                       if_else(country == "Canada" & intcat == 25, "2021",
                       if_else(country == "Costa Rica" & intcat == 39, "2020", 
                       if_else(country == "Denmark" & intcat == 47, "2020",
                       if_else(country == "Denmark" & intcat == 48, "2021",
                       if_else(country == "Germany" & intcat == 59, "2020",
                       if_else(country == "Germany" & intcat == 60, "2021",
                       if_else(country == "Hungary" & intcat == 66, "2020",
                       if_else(country == "Hungary" & intcat == 67, "2021",
                       if_else(country == "India" & intcat == 79, "2020",
                       if_else(country == "India" & intcat == 80, "2021",
                       if_else(country == "Ireland" & intcat == 86, "2020",
                       if_else(country == "Ireland" & intcat == 87, "2021",
                       if_else(country == "Mexico" & intcat == 100, "2020",
                       if_else(country == "Mexico" & intcat == 101, "2021",
                       if_else(country == "Mongolia" & intcat == 106, "2020",
                       if_else(country == "Mongolia" & intcat == 107, "2021",
                       if_else(country == "Netherlands" & intcat == 113, "2020",
                       if_else(country == "Northern Ireland" & intcat == 121, "2021",
                       if_else(country == "Oman" & intcat == 126, "2021",
                       if_else(country == "Paraguay" & intcat == 133, "2021",
                       if_else(country == "Peru" & intcat == 139, "2021",
                       if_else(country == "Portugal" & intcat == 143, "2020",
                       if_else(country == "Portugal" & intcat == 144, "2021",
                       if_else(country == "Qatar" & intcat == 150, "2020",
                       if_else(country == "Qatar" & intcat == 151, "2021",
                       if_else(country == "Scotland" & intcat == 157, "2020",
                       if_else(country == "Scotland" & intcat == 158, "2021",
                       if_else(country == "South Africa" & intcat == 163, "2020",
                       if_else(country == "South Africa" & intcat == 164, "2021",
                       if_else(country == "South Africa" & intcat == 165, "2022",
                       if_else(country == "Spain" & intcat == 172, "2021",
                       if_else(country == "Sweden" & intcat == 178, "2020",
                       if_else(country == "Sweden" & intcat == 179, "2021",
                       if_else(country == "United States" & intcat == 184, "2021", wave))))))))))))))))))))))))))))))))))))))

intense2x <- intense2

intense2 <-
  intense2 %>%
  dplyr::group_by(country, wave) %>%
  dplyr::summarise(intensity = mean(intensity)) %>%
  ungroup() %>%

#create different RSV waves in different countries
  dplyr::mutate(wave = if_else(wave == "precov", "precov", 
                               if_else(wave == "2020", "wave1",
                                       if_else(wave == "2021", "wave2", "wave3"))))

#calculate mean for pre-Covid period (2017, 2018, 2019)
intense2 <-
  intense2 %>%
  dplyr::select(country, wave, intensity) %>%
  dplyr::group_by(country, wave) %>%
  dplyr::mutate(intensity = mean(intensity)) %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = wave, values_from = intensity) %>%
  dplyr::select(everything(), -row) %>%
  dplyr::filter(!is.na(wave1), !is.na(wave2)) %>%
  data_frame() %>%
  ungroup()

#1)intensity by overall status (pearson correlation coefficient)
intense_overall <-
  left_join(
    intense2 %>%
    dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                  w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
    intense2 %>%
    dplyr::select(country, precov, wave3) %>%
    dplyr::filter(!is.na(wave3)) %>%
    dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3)))) %>%
  
  dplyr::mutate(cat = "Overall")

#2)intensity by hemisphere (pearson correlation coefficient)
intense_hemi <-
  intense2 %>%
  inner_join(rsv_all %>% dplyr::select(country, hemi) %>% dplyr::distinct())

scatterXY <- list()
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  scatterXY[[i]] =
    left_join(
      intense_hemi %>%
        dplyr::filter(hemi == i) %>%
        dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                      w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
      intense_hemi %>%
        dplyr::filter(hemi == i) %>%
        dplyr::select(country, precov, wave3) %>%
        dplyr::filter(!is.na(wave3)) %>%
        dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3))))
}

intense_hemi <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-hemi)

I1 <-
  dplyr::bind_rows(intense_overall, intense_hemi) %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 0.5, label = paste0("r = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV intensity", title = "INTENSITY") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

I2 <-
  dplyr::bind_rows(intense_overall, intense_hemi) %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 0.5, label = paste0("r = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV intensity", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

I3 <-
  dplyr::bind_rows(intense_overall, intense_hemi) %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(data = dplyr::bind_rows(intense_overall, intense_hemi) %>% dplyr::filter(!is.na(w3corr)), aes(x = 8, y = 0.5, label = paste0("r = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV intensity", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#RSV ONSET TIMING COMPARISONS BY CLIMATE ZONE
#====================================================================

#3)onset timing by climate zone (pearson correlation coefficient)
intense_clim <-
  intense2 %>%
  inner_join(climate %>% dplyr::select(country, clim) %>% dplyr::distinct())

scatterXY <- list()
for (i in c("Tropical", "Temperate")) {
  scatterXY[[i]] =
    left_join(
      intense_clim %>%
        dplyr::filter(clim == i) %>%
        dplyr::mutate(w1corr = abs(round((stats::cor(wave1, precov, method = c("pearson")))[1], digits = 3)),
                      w2corr = abs(round((stats::cor(wave2, precov, method = c("pearson")))[1], digits = 3))),
      intense_clim %>%
        dplyr::filter(clim == i) %>%
        dplyr::select(country, precov, wave3) %>%
        dplyr::filter(!is.na(wave3)) %>%
        dplyr::mutate(w3corr = abs(round((stats::cor(wave3, precov, method = c("pearson")))[1], digits = 3))))
}
intense_clim <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-clim) %>%
  dplyr::mutate(cat = ifelse(cat == "Tropical", "(Sub)tropical", cat))

I4 <-
  intense_clim %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 0.5, label = paste0("r = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV intensity", title = "INTENSITY") +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

I5 <-
  intense_clim %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 0.5, label = paste0("r = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV intensity", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

I6 <-
  intense_clim %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(data = intense_clim %>% dplyr::filter(!is.na(w3corr)), aes(x = 8, y = 0.5, label = paste0("r = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV intensity", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#====================================================================

# #delete unnecessary files
rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))
