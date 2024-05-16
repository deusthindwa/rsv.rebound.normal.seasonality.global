#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#RSV ONSET TIMING COMPUTATION
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
  dplyr::mutate(seqwk = seq.int(from = week(date), by = 1, length.out = n()),
                time = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, time, seqwk, cases, country) %>%
  base::split(list(.$country))

#delete empty country data frames from list X
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models, derivatives & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv1 <- list()
deriv2 <- list()
onset2 <- list()

# estTimes <- list()
# estCases <- list()

#functions to numerically calculate the second derivative
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#run the GAM models via random effects maximum likelihood (REML)
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = time, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

# #equal time interval estimation
# eps = 0.5
# for (i in names(X)) {
#   estTimes[[i]] = data.frame(time=seq(min(X[[i]]$time) - 0.5, max(X[[i]]$time) + 0.5 - eps, by=eps))
# }
# 
# #estimate incidence
# for (i in names(X)){
#   estCases[[i]] = pspline.estimate.timeseries(
#     Gmodels[[i]], 
#     estTimes[[i]],
#     pspline.outbreak.cases,
#     samples = 100, 
#     level=.95
#   )
# }

#iterate for each country, extract fitted values, compute 1st and 2nd derivatives
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% #fitted values
  #tsDS[[i]] = data.frame(fitcases = estCases[[i]]$cases.median) %>% #fitted values
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n()))
  
  deriv1[[i]] = data.frame(fderiv = diff(tsDS[[i]]$fitcases)/diff(tsDS[[i]]$seqwk)) %>% 
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n())) #first derivative
  
  deriv2[[i]] = data.frame(sderiv = deriv(middle_pts(tsDS[[i]]$seqwk), deriv(tsDS[[i]]$seqwk, tsDS[[i]]$fitcases))) %>%
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n())) #second derivative

  #alternative formula for second derivative without using deriv or middle_pts functions
  #deriv2[[i]] = data.frame(sderiv = diff(deriv1[[i]]$fderiv)/diff(deriv1[[i]]$seqwk)) %>% 
  #mutate(seqwk = seq.int(from = 1, by = 1, length.out = n())) #second derivative
  }

#unlist all the datasets and keep location of maximim second derivative
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")
deriv1 <- dplyr::bind_rows(deriv1, .id = "country")
deriv2 <- dplyr::bind_rows(deriv2, .id = "country")
onset2 <- 
  deriv2 %>% 
  dplyr::left_join(deriv1) %>% 
  dplyr::filter(fderiv > 0) %>%
  dplyr::select(everything(), -fderiv) %>%
  dplyr::mutate(intcat = factor(cumsum(c(1, abs(seqwk[-length(seqwk)] - seqwk[-1]) > 1))),
                year = factor(year(date))) %>%
  dplyr::filter(year != "2023") %>%
  dplyr::group_by(intcat) %>%
  dplyr::mutate(counter = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(counter != 1) %>%
  
  dplyr::group_by(country, intcat, year) %>%
  dplyr::mutate(max_sderiv = max(sderiv, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(sderiv == max_sderiv) %>%
  
  #fixing issues per observation to ensure accuracy of onset timing
  dplyr::mutate(year = if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa") & year == "2020", "No_Oubreak", year)) %>%
  dplyr::filter(year != "No_Oubreak") %>%
  
  dplyr::mutate(year = if_else(country == "Iceland" & seqwk == 215, "2020", year)) %>%
  dplyr::mutate(year = if_else(country == "Sweden" & seqwk == 106, "2018", year)) %>%
  dplyr::mutate(year = if_else(country == "Netherlands" & year == 2021, "2020", 
                               if_else(country == "Netherlands" & year == 2022, "2021", year))) %>%
  dplyr::mutate(year = if_else(country == "Netherlands" & seqwk == 306, "2022", year)) %>%
  dplyr::mutate(sderiv = if_else(country == "South Africa" & seqwk == 261, 0, sderiv)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 53, "2021", year)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 300, "2021", year)) %>%
  dplyr::mutate(sderiv = if_else(country == "India" & seqwk == 311, 0.0001, sderiv)) %>%
  
  dplyr::group_by(country, year) %>%
  dplyr::filter(max_sderiv == max(sderiv)) %>%
  dplyr::ungroup()

#compute circular timing around 52 weeks
 onset2 <-
   onset2 %>% 
   dplyr::mutate(loc = week(date),
                 loc = if_else(loc == 53, 52, loc),
                 seqwk2 = seqwk) %>% 
   dplyr::select(country, year, seqwk, seqwk2, loc, date)

onset1 <- 
  dplyr::left_join(tsDS, X) %>%
  dplyr::left_join(deriv2 %>% dplyr::select(everything(), -date)) %>%
  dplyr::left_join(onset2 %>% dplyr::select(everything(), -year, -loc, -date))

#ggplot of RSV time series AND onset values
O0 <-
  ggplot() +
  geom_point(data = onset1, aes(x = seqwk, y = cases), color = "gray", shape = 1, size = 2,  stroke = 2) + #onset times
  geom_line(data = onset1, aes(x = seqwk, y = fitcases), size = 1.5, color = "black") + #GAM fitted curve
  geom_point(data = onset1, aes(x = seqwk2, y = fitcases), color = "blue", shape = 4, size = 2,  stroke = 2) + #onset times
  geom_vline(xintercept = 171, linetype = "dotted", color = "black", size = 1.5) +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(1, 325, 52), limits = c(0, 325)) +
  labs(x = "Reporting weeks between 2017 and 2023", y = "GAM fitted RSV cases and estimated outbreak onset", title ="RSV ONSET TIMING") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))
  

ggsave(here("output", "sfig2_onset.png"),
       plot = O0,
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV ONSET TIMING COMPARISONS BY OVERALL AND HEMISPHERE
#====================================================================

#create different RSV waves in different countries
onset2 <-  
  onset2 %>% 
  dplyr::mutate(wave = if_else(year == "2017" | year == "2018" | year == "2019", "precov", year)) %>%
  
  dplyr::mutate(wave = if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2020", "wave1",
                               if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2021", "wave2", 
                                       if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2022", "wave3", wave)))) %>%
  
  dplyr::mutate(wave = if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2021", "wave1", 
                               if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2022", "wave2", wave)))

onset2x <- onset2 #onset2x dataset used for regression

#calculate circular mean for pre-Covid period (2017, 2018, 2019)
onset2 <-
onset2 %>%
  dplyr::select(country, wave, loc) %>%
  dplyr::group_by(country, wave) %>%
  #dplyr::mutate(loc = (circular::mean.circular((2*pi/52)*(loc-1))/(2*pi/52) + 52) %% 52) %>% #52 weeks per year (circular)
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
onsetb <- 
  onset2 %>% 
  dplyr::left_join(rsv_all %>% select(hemi, country) %>% distinct()) %>%
  dplyr::left_join(climate %>% select(country, clim) %>% distinct())

#1)onset timing by overall status (circular correlation coefficient)
onset_overall <-
  onset2 %>%
  dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),

                w1L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w1U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w2L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w2U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w3L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w3U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                cat = "Overall") %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

#2)onset timing by hemisphere (circular correlation coefficient)
onset_hemi <-
  onset2 %>%
  inner_join(rsv_all %>% dplyr::select(country, hemi) %>% dplyr::distinct())

scatterXY <- list()
for (j in c("Northern hemisphere", "Southern hemisphere")) {
  scatterXY[[j]] =
    onset_hemi %>%
    dplyr::filter(hemi == j) %>%
    dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                  w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                  w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),
                  
                  w1L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w1U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w2L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w2U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2, hemi) %>% dplyr::filter(hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w3L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3, hemi) %>% dplyr::filter(!is.na(wave3), hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w3U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3, hemi) %>% dplyr::filter(!is.na(wave3), hemi ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2))
}
onset_hemi <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-hemi) %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

O1 <-
  dplyr::bind_rows(onset_overall, onset_hemi) %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV onset timing", title = "") +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O2 <-
  dplyr::bind_rows(onset_overall, onset_hemi) %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O3 <-
  dplyr::bind_rows(onset_overall, onset_hemi) %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w3L,",",w3U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Overall", "Northern hemisphere", "Southern hemisphere"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "fig3_corHemisphere.png"),
       plot = ((O1 + labs(title = "RSV ONSET TIMING"))/O2/(O3 + labs(x = "PreCOVID (2017-19) mean onset timing"))),
       width = 16, height = 14, unit = "in", dpi = 300)

#====================================================================
#RSV ONSET TIMING COMPARISONS BY CLIMATE ZONE
#====================================================================

#3)onset timing by climate zone (circular correlation coefficient)
onset_clim <-
  onset2 %>%
  inner_join(climate %>% dplyr::select(country, clim) %>% dplyr::distinct())

scatterXY <- list()
for (j in c("Tropical", "Temperate")) {
  scatterXY[[j]] =
    onset_clim %>%
    dplyr::filter(clim == j) %>%
    dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                  w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                  w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),

                  w1L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w1U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w2L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w2U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2, clim) %>% dplyr::filter(clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                  w3L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3, clim) %>% dplyr::filter(!is.na(wave3), clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                  w3U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3, clim) %>% dplyr::filter(!is.na(wave3), clim ==j)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2))
}
onset_clim <- 
  bind_rows(scatterXY, .id = "id") %>%
  dplyr::rename("cat" = "id") %>%
  dplyr::select(-clim) %>%
  dplyr::mutate(cat = ifelse(cat == "Tropical", "(Sub)tropical", cat)) %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

O4 <-
  onset_clim %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV onset timing", title = "ONSET TIMING") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O5 <-
  onset_clim %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O6 <-
  onset_clim %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 4, label = paste0("c = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 1, label = paste0("(", w3L,",",w3U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ factor(cat, levels = c("Temperate", "(Sub)tropical"))) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#====================================================================

#delete unnecessary files
rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))

