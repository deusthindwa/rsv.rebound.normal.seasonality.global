#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES PLOT OF ONSET TIMINGS
#====================================================================

#split the dataset by country/regionUS to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
  dplyr::arrange(country, yr, wk, cases) %>%
  group_by(country) %>%
  mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()),
         seqwk2 = seqwk) %>%
  ungroup() %>%
  select(seqwk, seqwk2, cases, country) %>%
  split(list(.$country))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv.pred <- list()
deriv2.pred <- list()
rsv_onset2 <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
  
} #+ s(seqwk2, bs = "re")

#iterate for each country, extract fitted case values
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
  deriv.pred[[i]] = data.frame(deriv = diff(tsDS[[i]]$fitcases)/diff(tsDS[[i]]$seqwk)) %>%
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
  deriv2.pred[[i]] = data.frame(deriv2 = diff(deriv.pred[[i]]$deriv)/diff(deriv.pred[[i]]$seqwk)) %>%
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
  #.Machine$double.eps default MinPeakHeight
  rsv_onset2[[i]] = data.frame(gsignal::findpeaks(deriv2.pred[[i]]$deriv2, MinPeakHeight = 0.01, MinPeakDistance = 1, MinPeakWidth = 1, MaxPeakWidth = Inf, DoubleSided = TRUE))
}

#unlist all the datasets
X <- bind_rows(X, .id = "country")
tsDS <- bind_rows(tsDS, .id = "country")
deriv.pred <- bind_rows(deriv.pred, .id = "country")
deriv2.pred <- bind_rows(deriv2.pred, .id = "country")
rsv_onset2 <- bind_rows(rsv_onset2, .id = "country") %>% dplyr::filter(pks > 0)

#only select peak values on second derivative where the positive derivative is positive
rsv_onset2 <- 
  left_join(rsv_onset2, left_join(deriv.pred, deriv2.pred) %>% dplyr::rename("loc" = "seqwk")) %>%
  dplyr::filter(deriv >0)

#====================================================================

#combined dataset
rsv_onset1 <- 
  left_join(deriv2.pred, tsDS) %>%
  left_join(deriv.pred) %>%
  left_join(X) %>%
  select(country, seqwk, cases, fitcases, deriv, deriv2) %>%
  mutate(year = ifelse((seqwk > 0L & seqwk <= 52L), "2017",
                       ifelse((seqwk > 52L & seqwk <= 104), "2018",
                              ifelse((seqwk > 104L & seqwk <= 156L), "2019",
                                     ifelse((seqwk > 156L & seqwk <= 208L), "2020",
                                            ifelse((seqwk > 208L & seqwk <= 260L), "2021",
                                                   ifelse((seqwk > 260L & seqwk <= 312L), "2022",
                                                          ifelse((seqwk > 312L & seqwk <= 364L), "2023", NA_character_))))))))

#generate a dataset for location of maximum growth
rsv_onset2 <-
  rsv_onset2 %>% 
  select(country, baseline, pks, loc, parabol.x.from, parabol.x.to) %>% 
  rename(locL = parabol.x.from, locU = parabol.x.to)

#combine growth 1 and growth 2 for plotting
rsv_onset1 <- rsv_onset1 %>% left_join(rsv_onset2, by = c("country", "seqwk" = "loc"))

#====================================================================

#ggplot plotting of time series of onset values
A0 <-
  ggplot() +
  #geom_point(data = rsv_onset1, aes(x = seqwk, y = cases), size = 2, color = "gray60", shape = 1, stroke = 1) + #original data
  #geom_line(data = rsv_onset1, aes(x = seqwk, y = fitcases), size = 0.8) + # simulated GAM fitted model
  #geom_line(data = rsv_onset1, aes(x = seqwk, y = deriv/0.1), size = 0.8, color = "darkblue") + # differentiated fitted curve
  #scale_y_continuous("Repoted and fitted weekly RSV cases", sec.axis = sec_axis(~. *0.2, name = "Estimated onset")) + 
  geom_line(data = rsv_onset1, aes(x = seqwk, y = deriv2), size = 1.5, color = "black") + # differentiated fitted curve
  geom_point(data = rsv_onset1, aes(x = seqwk, y = pks), color = "red", shape = 4, size = 2,  stroke = 2) + #timing of peak and peak value
  #geom_errorbarh(data = rsv_onset1, aes(y = pks, xmin = locL, xmax = locU), height = 0, size = 1, color = "darkred") +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 325, 52), limits = c(0, 325)) +
  #theme(axis.text.y.right = element_text(color = "darkred"), axis.ticks.y.right = element_line(color = "darkred"), axis.title.y.right = element_text(color = "darkred")) +
  labs(x = "Reporting week between 2017 and 2023", y = "Estimated onset timing (2nd derivative of fitted P-spline)", title ="RSV ONSET TIMING") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig2_onset.png"),
       plot = (A0),
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV ONSET BY OVERALL STATUS
#====================================================================

#loop in the specified vector content
onset_rand <-  
  rsv_onset %>% 
  dplyr::filter(country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West") %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = covper, values_from = epiwk) %>%
  #tidyr::fill(precov, .direction = "up")
  dplyr::select(everything(), -row)

#reshape the onset datasets for scatter plotting
normX =
  left_join(
    left_join(
      onset_rand %>%
        dplyr::filter(is.na(y2021), is.na(y2022)) %>%
        dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
        dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
      
      onset_rand %>%
        dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2021)) %>%
        dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
    
    onset_rand %>%
      dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
      dplyr::filter(!is.na(y2022)) %>%
      dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>%
  
  dplyr::mutate(id = "All countries") %>%
  dplyr::select(id, country, everything())  %>%
  
  mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
         y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
         y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
  
  dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))

#====================================================================
#RSV ONSET BY HEMISPHERE
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  onset_hemi <-  
    rsv_onset %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = epiwk) %>%
    #tidyr::fill(precov, .direction = "up")
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] =
    left_join(
      left_join(
        onset_hemi %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        onset_hemi %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      onset_hemi %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>%
    
    mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))
  
}

hemiX <- bind_rows(scatterXY, .id = "id")

#====================================================================
#====================================================================

A1 <-
  dplyr::bind_rows(hemiX, normX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%

  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("c = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "1st RSV wave after COVID-19 suppression", title = "(A) RSV ONSET TIMING") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

A2 <-
  dplyr::bind_rows(hemiX, normX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("c = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "2nd RSV wave after COVID-19 suppression", title ="") +
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#RSV ONSET BY CLIMATE ZONES
#====================================================================

#there are 5 classifications of climatic zones according to Köppen-Geiger climate classification system
#tropical, dry, temperate, continental, and polar
#countries here are classified as tropical (tropical, dry), temperate (continental, polar), and subtropical (tropical, temperate) to check if climate zones align with onset of RSV cases
#countries climate data can be found here (https://www.worlddata.info)

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Temperate", "Sub-tropical", "Tropical")) {
  
  onset_cz <-  
    rsv_onset %>% 
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = epiwk) %>%
    #tidyr::fill(precov, .direction = "up")
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        onset_cz %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, l_epiwk, u_epiwk) %>%
          dplyr::rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        onset_cz %>%
          dplyr::select(country, y2021, l_epiwk, u_epiwk) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      onset_cz %>%
        dplyr::select(country, y2022, l_epiwk, u_epiwk) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk"))  %>%
    
    mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))
}

climX <- bind_rows(scatterXY, .id = "id")

#====================================================================
#====================================================================

A3 <-
  dplyr::bind_rows(climX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  #geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("c = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "1st RSV wave after COVID-19 suppression", title = "(A) RSV ONSET TIMING") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

A4 <-
  dplyr::bind_rows(climX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  #geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("c = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "2nd RSV wave after COVID-19 suppression", title ="") +
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#====================================================================

# #delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset|stringency|A|B|C|D", ls(), value = TRUE, invert = TRUE))
rm("rsv_onset1", "rsv_onset2")
