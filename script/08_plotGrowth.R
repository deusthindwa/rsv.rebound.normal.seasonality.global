#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES OF RSV GROWTH
#====================================================================

C0 <-
  ggplot() +
  #geom_point(data = rsv_growth1, aes(x = seqwk, y = cases), size = 2, color = "gray60", shape = 1, stroke = 1) + #original data
  #geom_line(data = rsv_growth1, aes(x = seqwk, y = fitcases), size = 0.8) + # simulated GAM fitted model
  #scale_y_continuous("Repoted and fitted weekly RSV cases", sec.axis = sec_axis(~. *0.01, name = "Estimated growth rate")) + 
  geom_line(data = rsv_growth1, aes(x = seqwk, y = deriv), size = 1.5, color = "black") + # differentiated fitted curve
  geom_point(data = rsv_growth1, aes(x = seqwk, y = pks), color = "red", shape = 4, size = 2,  stroke = 2) + #timing of peak and peak value
  geom_errorbarh(data = rsv_growth1, aes(y = pks, xmin = locL, xmax = locU), height = 0, size = 1, color = "darkred") +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 325, 52), limits = c(0, 325)) +
  #theme(axis.text.y.right = element_text(color = "darkred"), axis.ticks.y.right = element_line(color = "darkred"), axis.title.y.right = element_text(color = "darkred")) +
  labs(x = "Reporting week (2017-2023) ", y = "Growth rate (derivative of log fitted P-spline)", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig4_growth.png"),
       plot = (C0),
       width = 20, height = 22, unit="in", dpi = 300)

#====================================================================
#RSV PEAK BY HEMISPHERE
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  growth_hemi <-  
    rsv_growth2 %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = pks) %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] =
    left_join(
      left_join(
        growth_hemi %>%
          dplyr::select(country, precov) %>%
          dplyr::filter(!is.na(precov)),
        
        growth_hemi %>%
          dplyr::select(country, y2021) %>%
          dplyr::filter(!is.na(y2021))),
      
      growth_hemi %>%
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
growth_rand <-  
  rsv_growth2 %>% 
  dplyr::filter(country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West") %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = covper, values_from = pks) %>%
  dplyr::select(everything(), -row)

#reshape the onset datasets for scatter plotting
normX =
  left_join(
    left_join(
      growth_rand %>%
        dplyr::select(country, precov),
      
      growth_rand %>%
        dplyr::select(country, y2021) %>%
        dplyr::filter(!is.na(y2021))),
    
    growth_rand %>%
      dplyr::select(country, y2022) %>%
      dplyr::filter(!is.na(y2022))) %>%
  dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
  
  dplyr::mutate(corr2021 = abs(round(stats::cor(y2021, precov), digits = 3)),
                corr2022 = abs(round(stats::cor(y2022, precov), digits = 3))) %>% 
  
  dplyr::mutate(id = "All countries") %>%
  dplyr::select(id, country, everything())

#====================================================================
#====================================================================

C1 <-
  dplyr::bind_rows(hemiX, normX) %>%
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.1, y = 0.2, label = paste0("r = ", corr2021)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "1st RSV wave after COVID-19 suppression", title = "(C) RSV GROWTH RATE") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  scale_shape_manual(values = c(18, 4)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

C2 <-
  dplyr::bind_rows(hemiX, normX) %>%

  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.1, y = 0.2, label = paste0("r = ", corr2022)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID-19 (2017-19) mean estimate", y = "2nd RSV wave after COVID-19 suppression", title ="") +
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
  
  growth_cz <-  
    rsv_growth2 %>% 
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = pks) %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        growth_cz %>%
          dplyr::select(country, precov),
        
        growth_cz %>%
          dplyr::select(country, y2021) %>%
          dplyr::filter(!is.na(y2021))),
      
      growth_cz %>%
        dplyr::select(country, y2022) %>%
        dplyr::filter(!is.na(y2022))) %>%
    dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
    
    dplyr::mutate(corr2021 = abs(round(stats::cor(y2021, precov), digits = 3)),
                  corr2022 = abs(round(stats::cor(y2022, precov), digits = 3)))
}

climX <- dplyr::bind_rows(scatterXY, .id = "id")

#====================================================================
#====================================================================

C3 <-
  dplyr::bind_rows(climX) %>%
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.1, y = 0.2, label = paste0("r = ", corr2021)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "1st RSV wave after COVID-19 suppression", title = "(C) RSV GROWTH RATE") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  scale_shape_manual(values = c(18, 4)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

C4 <-
  dplyr::bind_rows(climX) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 1.1, y = 0.2, label = paste0("r = ", corr2022)), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  scale_y_continuous(breaks = seq(0, 1.2, 0.2), limits = c(0, 1.3)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID-19 (2017-19) mean estimate", y = "2nd RSV wave after COVID-19 suppression", title ="") +
  scale_shape_manual(values = c(18, 4)) +
  theme(axis.text.x = element_text(size = 12)) + 
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#====================================================================

#delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak|rsv_growth|stringency|rsv_dtw|A|B|C|D", ls(), value = TRUE, invert = TRUE))
