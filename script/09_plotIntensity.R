#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES OF RSV INTENSITY
#====================================================================

#ggplot plotting of time series and peak values
D0 <-
  ggplot() +
  geom_point(data = rsv_intens1, aes(x = seqwk, y = cases), size = 2, color = "gray60", shape = 1, stroke = 1) + #original data
  geom_line(data = rsv_intens1, aes(x = seqwk, y = fitcases), size = 0.8) + # simulated GAM fitted model
  scale_y_continuous("Repoted and GAM fitted weekly RSV cases", sec.axis = sec_axis(~. *0.002, name = "Estimated intesity")) + 
  geom_line(data = rsv_intens1, aes(x = seqwk, y = deriv/0.002), size = 2, color = "red") + # differentiated fitted curve
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 325, 52), limits = c(0, 325)) +
  theme(axis.text.y.right = element_text(color = "red"), axis.ticks.y.right = element_line(color = "darkred"), axis.title.y.right = element_text(color = "darkred")) +
  labs(x = "Reporting week between 2017 and 2023", y = "", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig5_intensity.png"),
       plot = (D0),
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV INTENSITY BY HEMISPHERE
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  intensity_hemi <-  
    rsv_intens2 %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = intensity) %>%
    dplyr::select(everything(), -row)

  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] =
    left_join(
      left_join(
        intensity_hemi %>%
          dplyr::select(country, precov),
        
        intensity_hemi %>%
          dplyr::select(country, y2021) %>%
          dplyr::filter(!is.na(y2021))),
      
      intensity_hemi %>%
        dplyr::select(country, y2022) %>%
        dplyr::filter(!is.na(y2022))) %>%
    dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
    
    dplyr::mutate(corr2021 = abs(round(stats::cor(y2021, precov), digits = 3)),
                  corr2022 = abs(round(stats::cor(y2022, precov), digits = 3)))
}

hemiX <- dplyr::bind_rows(scatterXY, .id = "id")

#====================================================================
#RSV PEAK BY OVERALL STATUS
#====================================================================

#loop in the specified vector content
intensity_rand <-  
  rsv_intens2 %>% 
  dplyr::filter(country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West") %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = covper, values_from = intensity) %>%
  dplyr::select(everything(), -row)

#reshape the onset datasets for scatter plotting
normX =
  left_join(
    left_join(
      intensity_rand %>%
        dplyr::select(country, precov),
      
      intensity_rand %>%
        dplyr::select(country, y2021) %>%
        dplyr::filter(!is.na(y2021))),
    
    intensity_rand %>%
      dplyr::select(country, y2022) %>%
      dplyr::filter(!is.na(y2022))) %>%
  dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
  
  dplyr::mutate(corr2021 = abs(round(stats::cor(y2021, precov), digits = 3)),
                corr2022 = abs(round(stats::cor(y2022, precov), digits = 3))) %>% 
  
  dplyr::mutate(id = "All countries") %>%
  dplyr::select(id, country, everything())

#====================================================================
#====================================================================

D1 <-
  dplyr::bind_rows(hemiX, normX) %>%
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr2021)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "", title = "(D) RSV INTENSITY") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  scale_shape_manual(values = c(18, 4)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

D2 <-
  dplyr::bind_rows(hemiX, normX) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr2022)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID (2017-19) mean estimate", y = "", title ="") +
  scale_shape_manual(values = c(18, 4)) +
  theme(axis.text.x = element_text(size = 12)) + 
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#RSV PEAK BY CLIMATE ZONES
#====================================================================

#there are 5 classifications of climatic zones according to Köppen-Geiger climate classification system
#tropical, dry, temperate, continental, and polar
#countries here are classified as tropical (tropical, dry), temperate (continental, polar), and subtropical (tropical, temperate) to check if climate zones align with onset of RSV cases
#countries climate data can be found here (https://www.worlddata.info)

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Temperate", "Sub-tropical", "Tropical")) {
  
  intensity_cz <-  
    rsv_intens2 %>% 
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = intensity) %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        intensity_cz %>%
          dplyr::select(country, precov),
        
        intensity_cz %>%
          dplyr::select(country, y2021) %>%
          dplyr::filter(!is.na(y2021))),
      
      intensity_cz %>%
        dplyr::select(country, y2022) %>%
        dplyr::filter(!is.na(y2022))) %>%
    dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
    
    dplyr::mutate(corr2021 = abs(round(stats::cor(y2021, precov), digits = 3)),
                  corr2022 = abs(round(stats::cor(y2022, precov), digits = 3)))
}

climX <- dplyr::bind_rows(scatterXY, .id = "id")

#====================================================================
#====================================================================

D3 <-
  dplyr::bind_rows(climX) %>%
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr2021)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "", title = "(D) RSV INTENSITY") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  scale_shape_manual(values = c(18, 4)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

D4 <-
  dplyr::bind_rows(climX) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr2022)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID-19 (2017-19) mean estimate", y = "", title ="") +
  scale_shape_manual(values = c(18, 4)) +
  theme(axis.text.x = element_text(size = 12)) + 
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#====================================================================

#delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak|rsv_growth|rsv_intens|stringency|A|B|C|D", ls(), value = TRUE, invert = TRUE))
