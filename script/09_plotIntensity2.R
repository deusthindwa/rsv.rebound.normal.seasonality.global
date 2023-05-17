#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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
    
    dplyr::mutate(corr = abs(round(stats::cor(y2021, y2022), digits = 3)*100))
}

hemiX <- dplyr::bind_rows(scatterXY, .id = "id")

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
    
    dplyr::mutate(corr = abs(round(stats::cor(y2021, y2022), digits = 3)*100))
}

climX <- dplyr::bind_rows(scatterXY, .id = "id")

#====================================================================
#RSV PEAK BY NORMALCY STATUS
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
  
  dplyr::mutate(corr = abs(round(stats::cor(y2021, y2022), digits = 3)*100)) %>% 
  
  dplyr::mutate(id = "All countries") %>%
  dplyr::select(id, country, everything())

#====================================================================
#COMBINE RSV PEAK PLOTS FOR HEMISPHERE, CLIMATE ZONE & NORMALCY
#====================================================================

A <-
  dplyr::bind_rows(hemiX, climX, normX) %>%
  
  ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV intensity in 2022/23", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  scale_shape_manual(values = c(18, 4)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#RSV PEAK BY WORLD HEALTH ORGANISATION REGIONS
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Europe", "North Americas", "Western Pacific", "Eastern Mediterranean", "South Americas", "Africa & SEAR")) {
  
  intensity_reg <-  
    rsv_intens2 %>% 
    dplyr::mutate(region = if_else(region == "Africa" | region == "South East Asia", "Africa & SEAR", region)) %>%
    dplyr::filter(region == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = intensity) %>%
    dplyr::select(everything(), -row)

  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        intensity_reg %>%
          dplyr::select(country, precov),
        
        intensity_reg %>%
          dplyr::select(country, y2021) %>%
          dplyr::filter(!is.na(y2021))),
      
      intensity_reg %>%
        dplyr::select(country, y2022) %>%
        dplyr::filter(!is.na(y2022))) %>%
    dplyr::filter(!is.na(precov), !is.na(y2021), !is.na(y2022)) %>%
    
    dplyr::mutate(corr = abs(round(stats::cor(y2021, y2022, method = "pearson"), digits = 3)*100))
}

B <-
  dplyr::bind_rows(scatterXY, .id = "id") %>%
  ggplot(aes(x = y2021, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 8, y = 1, label = paste0("r = ", corr, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "RSV intensity in 2021/22", y = "RSV intensity in 2022/23", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#MAKE ALL RSV PEAK PLOTS
#====================================================================

ggsave(here("output", "fig5_intensityMain2.png"),
       plot = (A/B),
       width = 22, height = 9, unit = "in", dpi = 300)
