#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#RSV PEAK BY HEMISPHERE
#====================================================================

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Northern hemisphere", "Southern hemisphere")) {
  
  peak_hemi <-  
    rsv_peak2 %>% 
    dplyr::filter(hemi == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = difloc) %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] =
    left_join(
      left_join(
        peak_hemi %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, diflocL, diflocU) %>%
          dplyr::rename("lwk1" = "diflocL", "uwk1" = "diflocU"),
        
        peak_hemi %>%
          dplyr::select(country, y2021, diflocL, diflocU) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "diflocL", "uwk2" = "diflocU")),
      
      peak_hemi %>%
        dplyr::select(country, y2022, diflocL, diflocU) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "diflocL", "uwk3" = "diflocU"))  %>%
    
    mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))
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
  
  peak_cz <-  
    rsv_peak2 %>% 
    dplyr::filter(clim_zone == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = difloc) %>%
    #tidyr::fill(precov, .direction = "up") %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        peak_cz %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, diflocL, diflocU) %>%
          dplyr::rename("lwk1" = "diflocL", "uwk1" = "diflocU"),
        
        peak_cz %>%
          dplyr::select(country, y2021, diflocL, diflocU) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "diflocL", "uwk2" = "diflocU")),
      
      peak_cz %>%
        dplyr::select(country, y2022, diflocL, diflocU) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "diflocL", "uwk3" = "diflocU"))  %>%
    
    mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))
}

climX <- dplyr::bind_rows(scatterXY, .id = "id")

#====================================================================
#RSV PEAK BY NORMALCY STATUS
#====================================================================

#loop in the specified vector content
peak_rand <-  
  rsv_peak2 %>% 
  dplyr::filter(country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West") %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = covper, values_from = difloc) %>%
  #tidyr::fill(precov, .direction = "up") %>%
  dplyr::select(everything(), -row)

#reshape the onset datasets for scatter plotting
normX =
  left_join(
    left_join(
      peak_rand %>%
        dplyr::filter(is.na(y2021), is.na(y2022)) %>%
        dplyr::select(country, precov, diflocL, diflocU) %>%
        dplyr::rename("lwk1" = "diflocL", "uwk1" = "diflocU"),
      
      peak_rand %>%
        dplyr::select(country, y2021, diflocL, diflocU) %>%
        dplyr::filter(!is.na(y2021)) %>%
        dplyr::rename("lwk2" = "diflocL", "uwk2" = "diflocU")),
    
    peak_rand %>%
      dplyr::select(country, y2022, diflocL, diflocU) %>%
      dplyr::filter(!is.na(y2022)) %>%
      dplyr::rename("lwk3" = "diflocL", "uwk3" = "diflocU"))   %>%
  
  mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
         y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
         y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
  
  dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0))) %>% 
  
  dplyr::mutate(id = "All countries") %>%
  dplyr::select(id, country, everything())

#====================================================================
#COMBINE RSV PEAK PLOTS FOR HEMISPHERE, CLIMATE ZONE & NORMALCY
#====================================================================

A <-
  dplyr::bind_rows(hemiX, climX, normX) %>%
  dplyr::mutate(lwk3 = ifelse(country == "Canada" | country == "Sweden", 1, lwk3), #resolve circular ploem in plotting
                lwk2 = ifelse(country == "Hungary", 1, lwk2)) %>% #where weeks overlap between old and new years
  dplyr::mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1, 52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1, 52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV peak timing in 2021/22", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

B <-
  dplyr::bind_rows(hemiX, climX, normX) %>%
  dplyr::mutate(lwk3 = ifelse(country == "Canada" | country == "Sweden", 1, lwk3)) %>% #resolve circular ploem in plotting where weeks overlap between old and new years
  dplyr::mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1, 52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1, 52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV peak timing in 2022/23", title ="") +
  theme(legend.position = "none", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  guides(shape = FALSE)

#====================================================================
#RSV PEAK BY WORLD HEALTH ORGANISATION REGIONS
#====================================================================

#create a list and data frame to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("Europe", "North Americas", "Western Pacific", "Eastern Mediterranean", "South Americas", "Africa & SEAR")) {

  peak_reg <-  
    rsv_peak2 %>% 
    dplyr::mutate(region = if_else(region == "Africa" | region == "South East Asia", "Africa & SEAR", region)) %>%
    dplyr::filter(region == i, (country != "United States North East" & country != "United States South" & country != "United States West" & country != "United States Mid West")) %>%
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = covper, values_from = difloc) %>%
    #tidyr::fill(precov, .direction = "down") %>%
    dplyr::select(everything(), -row)
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        peak_reg %>%
          dplyr::filter(is.na(y2021), is.na(y2022)) %>%
          dplyr::select(country, precov, diflocL, diflocU) %>%
          dplyr::rename("lwk1" = "diflocL", "uwk1" = "diflocU"),
        
        peak_reg %>%
          dplyr::select(country, y2021, diflocL, diflocU) %>%
          dplyr::filter(!is.na(y2021)) %>%
          dplyr::rename("lwk2" = "diflocL", "uwk2" = "diflocU")),
      
      peak_reg %>%
        dplyr::select(country, y2022, diflocL, diflocU) %>%
        dplyr::filter(!is.na(y2022)) %>%
        dplyr::rename("lwk3" = "diflocL", "uwk3" = "diflocU"))  %>%
    
    mutate(precovx = circular(precov, units = "degrees", template = "geographics", modulo = "2pi"),
           y2021x =  circular(y2021, units = "degrees", template = "geographics", modulo = "2pi"),
           y2022x = circular(y2022, units = "degrees", template = "geographics", modulo = "2pi")) %>% 
    
    dplyr::mutate(corr2021 = abs(round(((cor.circular(precovx, y2021x))[1])*100, digits = 0)),
                  corr2022 = abs(round((cor.circular(precovx, y2022x))[1]*100, digits = 0)))
}

C <-
  dplyr::bind_rows(scatterXY, .id = "id") %>%
  dplyr::mutate(lwk2 = ifelse(country == "Hungary", 1, lwk2)) %>% #resolve circular problem where weeks overlap between old and new years
  
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2021, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "RSV peak timing in 2021/22", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

D <-
  dplyr::bind_rows(scatterXY, .id = "id") %>%
  dplyr::mutate(lwk3 = ifelse(country == "Canada" | country == "Sweden", 1, lwk3)) %>% #resolve circular problem where weeks overlap between old and new years
  
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 45, y = 5, label = paste0("r = ", corr2022, "%")), color = "black", size = 6, fontface = "bold") +
  facet_grid(.~ id) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(1,52)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "PreCOVID (2017-19) mean peak timing", y = "RSV peak timing in 2022/23", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#====================================================================
#MAKE ALL RSV PEAK PLOTS
#====================================================================

ggsave(here("output", "fig3_peakMain.png"),
       plot = (A/B/C/D),
       width = 22, height = 18, unit = "in", dpi = 300)

#====================================================================

#ggplot plotting of time series and peak values
E <-
  ggplot() +
  geom_point(data = X %>% bind_rows(.), aes(x = seqwk, y = cases), size = 2, color = "gray60", shape = 1, stroke = 1) + #original data
  geom_line(data = rsv_ts, aes(x = seqwk, y = fitcases), size = 0.8) + # simulated GAM fitted model
  geom_point(data = rsv_peak1, aes(x = loc, y = pks ), color = "red", shape = 4, size = 1,  stroke = 2) + #timing of peak and peak value
  geom_errorbarh(data = rsv_peak1, aes(y = pks, xmin = locL, xmax = locU), height = 0, size = 1, color = "red") +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 325, 52), limits = c(0, 325)) +
  labs(x = "Reporting week (2017-2023) ", y = "Repoted and GAM fitted weekly RSV cases", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "fig3_peakSupp.png"),
       plot = (E),
       width = 20, height = 22, unit="in", dpi = 300)


#delete unnecessary files
rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak", ls(), value = TRUE, invert = TRUE))
