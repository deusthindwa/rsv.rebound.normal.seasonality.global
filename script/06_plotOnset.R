#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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
#RSV ONSET BY NORMALCY STATUS
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
#COMBINE RSV ONSET PLOTS FOR HEMISPHERE, CLIMATE ZONE & NORMALCY
#====================================================================

#combine datasets
# onsetX <-
#   bind_rows(hemiX, climX, normX)

A <-
  dplyr::bind_rows(hemiX, climX, normX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%

  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV onset timing in 2021/22", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

B <-
  dplyr::bind_rows(hemiX, climX, normX) %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV onset timing in 2022/23", title ="") +
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#RSV ONSET BY WORLD HEALTH ORGANISATION REGIONS
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Europe", "North Americas", "Western Pacific", "Eastern Mediterranean", "South Americas", "Africa & SEAR")) {
  
  onset_cz <-  
    rsv_onset %>% 
    dplyr::mutate(region = if_else(region == "Africa" | region == "South East Asia", "Africa & SEAR", region)) %>%
    dplyr::filter(region == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
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

C <-
  dplyr::bind_rows(scatterXY, .id = "id") %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV onset timing in 2021/22", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

D <-
  dplyr::bind_rows(scatterXY, .id = "id") %>%
  dplyr::mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID (2017-19) RSV mean onset timing", y = "RSV onset timing in 2022/23", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#MAKE ALL RSV ONSET PLOTS
#====================================================================

ggsave(here("output", "fig2_onsetMain.png"),
       plot = (A/B/C/D),
       width = 22, height = 18, unit = "in", dpi = 300)

#delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset", ls(), value = TRUE, invert = TRUE))
