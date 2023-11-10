#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# SMOOTHED STRINGENCY INDEX PLOT
#================================================================

A <-
  stringency %>%
  ggplot(aes(x = date, y = cases)) +
  geom_point(aes(x = fdate, y = strin_indx, group = country, color = country), color = "gray50", size = 3.5) + #reported
  geom_line(aes(x = fdate, y = strin_indxG, group = country, color = country), size = 1.5) + #GAM fitted
  geom_line(aes(x = fdate, y = strin_indxL, group = country), color = "black", size = 1.5) + #30d moving average
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) + 
  labs(title = "", x = "Reporting date", y = "Stringency index") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE), limits = c(0,100)) +
  facet_wrap(.~ country, ncol = 4, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "none", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

#combined plots and saving
ggsave(here("output", "sfig7_stringency.png"),
       plot = (A),
       width = 20, height = 17, unit="in", dpi = 300)

#====================================================================
#PLOT ALL CORRELATIONN PLOTS (ONSET/PEAK/GROWTH/INTENSITY)
#====================================================================

ggsave(here("output", "sfig8_corAllHemi.png"),
       plot = ((P1/P2/P3) | (G1/G2/G3)) | ((I1/I2/I3)),
       width = 30, height = 16, unit = "in", dpi = 300)

ggsave(here("output", "sfig9_corClimate.png"),
       plot = ((O4/O5/O6) | (P4/P5/P6) | (G4/G5/G6) | (I4/I5/I6)),
       width = 32, height = 16, unit = "in", dpi = 300)

#====================================================================
#generate the description dataset
#====================================================================

#define dataset for descriptive stats
rsv_descp <-
rsv_onset %>%
  dplyr::filter(!(country %in% c("United States North East", "United States South", "United States West", "United States Mid West"))) %>%
  distinct(country, .keep_all = TRUE) %>%
  dplyr::select(country, clim_zone, hemi, region)

#====================================================================
# numbers and proportions by predictors
#hemi
rsv_descp %>%
  dplyr::group_by(hemi) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#climate zone
rsv_descp %>%
  dplyr::group_by(clim_zone) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#out of normal RSV season
rsv_descp %>%
  dplyr::left_join(climate %>% dplyr::select(country, out_seas21)) %>%
  dplyr::group_by(out_seas21) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#====================================================================
# distribution of stringency
#hemi
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::mutate(phase = ifelse(date(fdate) >='2020-01-01' & date(fdate) <='2021-06-30', "covid",
                               ifelse(date(fdate) >='2021-07-01' & date(fdate) <='2022-06-30', "t2021",
                                      ifelse(date(fdate) >='2022-07-01' & date(fdate) <='2023-06-30', "t2022", NA_character_)))) %>%
  dplyr::filter(!is.na(phase)) %>%
  dplyr::group_by(phase, hemi) %>%
  dplyr::summarise(Q2str = median(strin_indx),
                   Q1str = quantile(strin_indx, 0.25),
                   Q3str = quantile(strin_indx, 0.75)) %>%
  dplyr::ungroup()

#climate zone
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::mutate(phase = ifelse(date(fdate) >='2020-01-01' & date(fdate) <='2021-06-30', "covid",
                               ifelse(date(fdate) >='2021-07-01' & date(fdate) <='2022-06-30', "t2021",
                                      ifelse(date(fdate) >='2022-07-01' & date(fdate) <='2023-06-30', "t2022", NA_character_)))) %>%
  dplyr::filter(!is.na(phase)) %>%
  dplyr::group_by(phase, clim_zone) %>%
  dplyr::summarise(Q2str = median(strin_indx),
                   Q1str = quantile(strin_indx, 0.25),
                   Q3str = quantile(strin_indx, 0.75)) %>%
  dplyr::ungroup()

#====================================================================
# distribution of population density
#hemi
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(hemi) %>%
  summarise(Q2pop = median(pop_dens),
            Q1pop = quantile(pop_dens, 0.25),
            Q3pop = quantile(pop_dens, 0.75))

#climate zone
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(clim_zone) %>%
  summarise(Q2pop = median(pop_dens),
            Q1pop = quantile(pop_dens, 0.25),
            Q3pop = quantile(pop_dens, 0.75))

#====================================================================
# distribution of median age
#hemi
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(hemi) %>%
  summarise(Q2pop = median(med_age),
            Q1pop = quantile(med_age, 0.25),
            Q3pop = quantile(med_age, 0.75))

#climate zone
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(clim_zone) %>%
  summarise(Q2pop = median(med_age),
            Q1pop = quantile(med_age, 0.25),
            Q3pop = quantile(med_age, 0.75))

#====================================================================
# plot stringency overtime, population density and population age
#====================================================================

D1 <-
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  ggplot() +
  geom_line(aes(x = fdate, y = strin_indx, color = hemi)) +
  theme_bw(base_size = 14, base_family = 'American typewriter') +
  labs(x = "Date of report", y = "Stringency index", title = "(A)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_color_manual(values=c("#36454F", "#7CAE00"))


D2 <-
rsv_descp %>%
  dplyr::left_join(stringency) %>%
  ggplot() +
  geom_line(aes(x = fdate, y = strin_indx, color = clim_zone)) +
  theme_bw(base_size = 14, base_family = 'American typewriter') +
  labs(x = "Date of report", y = "Stringency index", title = "(B)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_color_manual(values=c("#00BFC4", "#FFC133", "#7D33FF"))

D3 <- 
rsv_descp %>%
  dplyr::left_join(
    stringency %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(pop_den = mean(pop_dens)) %>%
      dplyr::ungroup()) %>%
  ggplot(aes(y = pop_den, fill = hemi)) + 
  geom_boxplot(color = "black", size = 1) +
  theme_bw(base_size = 14, base_family = "American typewriter") +
  scale_y_continuous(trans = log10_trans()) +
  labs(x = "", y = "Population density (log10)", title = "(C)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 0), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#36454F", "#7CAE00"))

D4 <-
rsv_descp %>%
  dplyr::left_join(
    stringency %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(pop_den = mean(pop_dens)) %>%
      dplyr::ungroup()) %>%
  ggplot(aes(y = pop_den, fill = clim_zone)) + 
  geom_boxplot(color = "black", size = 1) +
  theme_bw(base_size = 14, base_family = "American typewriter") +
  scale_y_continuous(trans = log10_trans()) +
  labs(x = "", y = "Population density (log10)", title = "(D)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 0), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#00BFC4", "#FFC133", "#7D33FF"))

D5 <-
rsv_descp %>%
  dplyr::left_join(
    stringency %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(pop_age = mean(med_age)) %>%
      dplyr::ungroup()) %>%
  ggplot(aes(y = pop_age, fill = hemi)) + 
  geom_boxplot(color = "black", size = 1) +
  theme_bw(base_size = 14, base_family = "American typewriter") +
  labs(x = "", y = "Population median age (years)", title = "(E)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 0), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text = element_text(size = 11), legend.title = element_blank()) + 
  theme(legend.position = "right", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#36454F", "#7CAE00"))

D6 <-
rsv_descp %>%
  dplyr::left_join(
    stringency %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(pop_age = mean(med_age)) %>%
      dplyr::ungroup()) %>%
  ggplot(aes(y = pop_age, fill = clim_zone)) + 
  geom_boxplot(color = "black", size = 1) +
  theme_bw(base_size = 14, base_family = "American typewriter") +
  labs(x = "", y = "Population median age (years)", title = "(F)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 0), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text = element_text(size = 11), legend.title = element_blank()) + 
  theme(legend.position = "right", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#00BFC4", "#FFC133", "#7D33FF"))


# combine all the plots
ggsave(here("output", "sfig7_pred_destribution.png"),
       plot = (D1 | D3 | D5)/(D2 | D4 | D6),
       width = 20, height = 14, unit="in", dpi = 300)

#====================================================================
#====================================================================

#load cluster dataset
rsv_cluster <- import(here("data", "cluster.xlsx"))

#combine cluster dataset with onset dataset
A <-
rsv_onset %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::group_by(cluster, covper) %>%
  dplyr::mutate(avg_onset = median(epiwk)) %>%
  dplyr::ungroup() %>%
  ggplot() +
  geom_point(aes(x = cluster, y = avg_onset, color = covper), size = 3, shape = 1, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with peak timing dataset
B <-
rsv_peak2 %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::group_by(cluster, covper) %>%
  dplyr::mutate(avg_peak = median(difloc)) %>%
  dplyr::ungroup() %>%
  ggplot() +
  geom_point(aes(x = cluster, y = avg_peak, color = covper), size = 3, shape = 2, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with intensity dataset
C <-
rsv_intens2 %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::group_by(cluster, covper) %>%
  dplyr::mutate(avg_intensity = median(intensity)) %>%
  dplyr::ungroup() %>%
  ggplot() +
  geom_point(aes(x = cluster, y = avg_intensity, color = covper), size = 3, shape = 5, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with growth rate dataset
D <-
rsv_growth2%>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::group_by(cluster, covper) %>%
  dplyr::mutate(avg_growth = median(pks),
                covper = if_else(covper == "y2021", "first wave",
                                 if_else(covper == "y2022", "second wave", NA_character_))) %>%
  dplyr::ungroup() %>%
  ggplot() +
  geom_point(aes(x = cluster, y = avg_growth, color = covper), size = 3, shape = 0, stroke = 2) +
  theme_bw(base_size = 18, base_family = 'Lato')

A|B|C|D


#====================================================================
#====================================================================

#load cluster dataset
rsv_cluster <- import(here("data", "cluster.xlsx"))

#combine cluster dataset with onset dataset
A <-
  rsv_onset %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::rename("Onset" = "epiwk") %>%
  ggplot() +
  geom_point(aes(x = cluster, y = Onset, color = covper), size = 3, shape = 1, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with peak timing dataset
B <-
  rsv_peak2 %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::rename("Peak" = "difloc") %>%
  ggplot() +
  geom_point(aes(x = cluster, y = Peak, color = covper), size = 3, shape = 2, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with intensity dataset
C <-
  rsv_intens2 %>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::rename("Intensity" = "intensity") %>%
  ggplot() +
  geom_point(aes(x = cluster, y = Intensity, color = covper), size = 3, shape = 5, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato') +
  theme(legend.position = "none", legend.title = element_blank())

#combine cluster dataset with growth rate dataset
D <-
  rsv_growth2%>%
  left_join(rsv_cluster) %>%
  dplyr::filter(covper != "precov") %>%
  dplyr::mutate(covper = if_else(covper == "y2021", "first wave",
                                 if_else(covper == "y2022", "second wave", NA_character_))) %>%
  dplyr::rename("Growth" = "pks") %>%
  ggplot() +
  geom_point(aes(x = cluster, y = Growth, color = covper), size = 3, shape = 0, stroke = 2) +
  xlim(1,4) +
  theme_bw(base_size = 18, base_family = 'Lato')

A|B|C|D











#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#COMBINING EPIDEMIC ONSET AND INTENSITY ON THE SAME PLOT
#====================================================================

#intensity first and second wave
DSintensity <-
  bind_cols(
    DSintens1 %>% rename("intens1" = "intensity") %>% select(country, intens1),
    DSintens2 %>% rename("intens2" = "intensity") %>% select(intens2))

#onset first and second wave
DS01 <- DSonset1 %>% 
  dplyr::filter(event == 1) %>% 
  dplyr::mutate(wave = "first wave") %>% 
  left_join(DSintensity) %>%
  mutate(hemix = if_else(country %in% c("Colombia", "Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia"), "Northern hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden"), "Northern hemisphere ", "Southern hemisphere")))

DS02 <- DSonset2 %>% 
  dplyr::filter(event == 1) %>% 
  dplyr::mutate(wave = "second wave") %>% 
  left_join(DSintensity) %>%
  mutate(hemix = if_else(country %in% c("Colombia", "Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia"), "Northern hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden"), "Northern hemisphere ", "Southern hemisphere")))

#combine first and second wave onsets as adjacent columns
DS03 <-
  bind_cols(DS01, 
            DS02 %>% 
              dplyr::select(country, fdate, wave) %>% 
              rename("countryx" = "country", "fdatex" = "fdate", "wavex" = "wave")) %>%
  mutate(hemix = if_else(country %in% c("Colombia", "Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia"), "Northern hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden"), "Northern hemisphere ", "Southern hemisphere")))

#plot the bubble of waves and intensity
ggplot() +
  geom_point(data = DS01, aes(x = fdate, y = wave, color = country, size = intens1), shape = 21, alpha = 0.75,  stat = "identity") +
  geom_point(data = DS02, aes(x = fdate, y = wave, color = country, size = intens2), shape = 21, alpha = 0.75, stat = "identity") +
  scale_size_continuous(limits = c(0.01, 10), range = c(1,10)) + 
  geom_segment(data = DS03, aes(x = fdate, y = wave, xend = fdatex, yend = wavex, colour = country), stat = "identity") +
  geom_text(data = DS03, aes(x = fdatex, y = wavex, label = str_sub(country, 1,2)), size = 2, angle = "45", vjust = -2, hjust = 0.5, fontface = "bold", position = position_dodge(width = 1.5)) +
  scale_x_date(breaks = c((date(c("2021-03-01", "2021-06-01", "2021-09-01", "2021-12-01", "2022-03-01", "2022-06-01", "2022-09-01", "2022-12-01")))),
               limits = c((date(c("2021-02-01", "2023-01-01")))),
               date_labels = "%m-%Y") +
  facet_grid(hemix~.) +
  theme_bw(base_size = 14, base_family = "Lato") + 
  labs(title = "", x = "Onset date", y = "RSV epidemic waves") +
  theme(legend.text = element_text(size = 8), legend.position = "right", legend.title = element_text(size = 8)) +
  guides(size = guide_legend(title = "Intensity\n(scaled magnitude of cases)"), color = guide_legend(title = "Country")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  theme(axis.text.y = element_text(face = "bold", size = 10)) 

