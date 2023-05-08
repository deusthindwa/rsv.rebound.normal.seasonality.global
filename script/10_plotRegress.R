#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# COMBINE REGRESSION DATASETS FOR PLOTTING
#================================================================

X1 <-
bind_rows(
DSonset1Mod %>%
  dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
  dplyr::rename("p_value" = "p.value", "lower_ci" = "conf.low", "upper_ci" = "conf.high") %>%
  dplyr::mutate(year = "Onset2021 from July2020", model = "Time to RSV Onset \n (Survival Model)"),

DSonset2Mod %>%
  dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
  dplyr::rename("p_value" = "p.value", "lower_ci" = "conf.low", "upper_ci" = "conf.high") %>%
  dplyr::mutate(year = "Onset2022 from Onset2021", model = "Time to RSV Onset \n (Survival Model)"),

DSpeak1Mod %>%
  dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
  dplyr::rename("p_value" = "p.value", "lower_ci" = "conf.low", "upper_ci" = "conf.high") %>%
  dplyr::mutate(year = "Peak2021 from July2020", model = "Time to RSV Peak \n (Survival Model)"),

DSpeak2Mod %>%
  dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
  dplyr::rename("p_value" = "p.value", "lower_ci" = "conf.low", "upper_ci" = "conf.high") %>%
  dplyr::mutate(year = "Peak2022 from Peak2021", model = "Time to RSV Peak \n (Survival Model)")) %>%
  
  dplyr::mutate(term = ifelse(term == "hemi2Southern hemisphere", "Hemisphere \n (North) vs South",
                              ifelse(term == "clim_zone2Sub-tropical", "Climate zone \n (Temperate) vs Subtropical",
                                     ifelse(term == "clim_zone2Tropical", "Climate zone \n (Temperate) vs Tropical",
                                            ifelse(term == "region2Africa & SEA", "Region \n (Europe) vs Africa/Asia",
                                                   ifelse(term == "region2Eastern Mediterranean", "Region \n (Europe) vs Eastern Med",
                                                           ifelse(term == "region2North Americas", "Region \n (Europe) vs North Americas",
                                                                  ifelse(term == "region2South Americas", "Region \n (Europe) vs South Americas",
                                                                         ifelse(term == "region2Western Pacific", "Region \n (Europe) vs Western Pacific",
                                                                                ifelse(term == "out_seas212yes", "Out-of-season RSV2021 \n (No) vs Yes",
                                                                                       ifelse(term == "latitude230-N", "Latitude \n (15-S) vs 30-N",
                                                                                              ifelse(term == "latitude245-N", "Latitude \n (15-S) vs 45-N",
                                                                                                     ifelse(term == "latitude245-S", "Latitude \n (15-S) vs 45-S",
                                                                                                            ifelse(term == "latitude260-N", "Latitude \n (15-S) vs 60-N",
                                                                                                                   ifelse(term == "strin_indx2", "COVID-19 contact stringency",
                                                                                                                          ifelse(term == "pop_dens2", "Populaltion density", "Median age"))))))))))))))))
                                      
X2 <-            
  bind_rows(
  DSgrowth1Mod %>%
    dplyr::select(term, estimate, p_value, lower_ci, upper_ci) %>%
    dplyr::mutate(year = "Growth in 2021", model = "RSV Growth Rate \n (Linear Model)"),
  
  DSgrowth2Mod %>%
    dplyr::select(term, estimate, p_value, lower_ci, upper_ci) %>%
    dplyr::mutate(year = "Growth in 2022", model = "RSV Growth Rate \n (Linear Model)"),
  
  DSintensity1Mod %>%
    dplyr::select(term, estimate, p_value, lower_ci, upper_ci) %>%
    dplyr::mutate(year = "Intensity in 2021", model = "RSV Intensity \n (Linear Model)"),
  
  DSintensity2Mod %>%
    dplyr::select(term, estimate, p_value, lower_ci, upper_ci) %>%
    dplyr::mutate(year = "Intensity in 2022", model = "RSV Intensity \n (Linear Model)")) %>%
  
  dplyr::filter(term != "intercept") %>%
  
  dplyr::mutate(term = ifelse(term == "hemi2: Southern hemisphere", "Hemisphere \n (North) vs South",
                              ifelse(term == "clim_zone2: Sub-tropical", "Climate zone \n (Temperate) vs Subtropical",
                                     ifelse(term == "clim_zone2: Tropical", "Climate zone \n (Temperate) vs Tropical",
                                            ifelse(term == "region2: Africa & SEA", "Region \n (Europe) vs Africa/Asia",
                                                   ifelse(term == "region2: Eastern Mediterranean", "Region \n (Europe) vs Eastern Med",
                                                          ifelse(term == "region2: North Americas", "Region \n (Europe) vs North Americas",
                                                                 ifelse(term == "region2: South Americas", "Region \n (Europe) vs South Americas",
                                                                        ifelse(term == "region2: Western Pacific", "Region \n (Europe) vs Western Pacific",
                                                                               ifelse(term == "out_seas212: yes", "Out-of-season RSV2021 \n (No) vs Yes",
                                                                                      ifelse(term == "latitude2: 30-N", "Latitude \n (15-S) vs 30-N",
                                                                                             ifelse(term == "latitude2: 45-N", "Latitude \n (15-S) vs 45-N",
                                                                                                    ifelse(term == "latitude2: 45-S", "Latitude \n (15-S) vs 45-S",
                                                                                                           ifelse(term == "latitude2: 60-N", "Latitude \n (15-S) vs 60-N",
                                                                                                                  ifelse(term == "strin_indx2", "COVID-19 contact stringency",
                                                                                                                         ifelse(term == "pop_dens2", "Populaltion density", "Median age"))))))))))))))))              
                
#make plots of all season metrics regression
DSregress <- bind_rows(X1, X2)

A <-
  DSregress %>%
  ggplot(aes(y = reorder(term, year), x = estimate, color = year)) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.5, size = 1.2, position = position_dodge2(width = 0.5)) +
  geom_point(shape = 16, size = 4, stroke = 2, position = position_dodge2(width = 0.5)) +  
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", cex = 0.6, alpha = 0.8) +
  labs(title = "", x = "Estimates from regression models (Log scale) and 95% Confidence Intervals", y = "") + 
  facet_grid(.~ model, scales = "free_x") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  guides(color = guide_legend(title = "")) +
  theme(legend.position = "bottom", strip.text.x = element_text(size = 16)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))  + 
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#A9A9A9", "#36454F", "#7CAE00", "#C77CFF", "#FFC133", "#7D33FF"))

ggsave(here("output", "fig6_regressMain.png"),
       plot = A,
       width = 17, height = 11, unit="in", dpi = 300)
