#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#plot all sensitivity corrrelation plots (onset/peak/growth/intensity)
#====================================================================

ggsave(here("output", "sfig10_allHemi.png"),
       plot = ((P1/P2/P3 + labs(x = "PreCOVID (2017-19) mean peak timing")) | (G1/G2/G3 + labs(x = "PreCOVID (2017-19) mean growth rate"))) | ((I1/I2/I3 + labs(x = "PreCOVID (2017-19) mean intensity"))),
       width = 34, height = 16, unit = "in", dpi = 300)

ggsave(here("output", "sfig11_allClimate.png"),
       plot = ((O4/O5/O6 + labs(x = "PreCOVID (2017-19) mean onset timing")) | (P4/P5/P6 + labs(x = "PreCOVID (2017-19) mean peak timing")) | (G4/G5/G6 + labs(x = "PreCOVID (2017-19) mean growth rate")) | (I4/I5/I6 + labs(x = "PreCOVID (2017-19) mean intensity"))),
       width = 32, height = 16, unit = "in", dpi = 300)

#================================================================
#plot smooth stringency index
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
ggsave(here("output", "sfig6_stringency.png"),
       plot = (A),
       width = 20, height = 17, unit="in", dpi = 300)

#====================================================================
#generate the description dataset
#====================================================================

#define dataset for descriptive stats
rsv_desc <-
rsv_all %>%
  dplyr::select(country, hemi, region) %>%
  dplyr::distinct(country, .keep_all = TRUE) %>%
  dplyr::left_join(climate %>% dplyr::select(country, clim, outSeasW1)) %>%
  dplyr::mutate(clim = if_else(clim == "Tropical", "(Sub)Tropical", "Temperate"))

#numbers and proportions by predictors
#hemi
rsv_desc %>%
  dplyr::group_by(hemi) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#climate zone
rsv_desc %>%
  dplyr::group_by(clim) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#out of normal RSV season
rsv_desc %>%
  dplyr::group_by(outSeasW1) %>%
  dplyr::tally() %>%
  dplyr::mutate(p = n/sum(n)*100)

#distribution of stringency
#hemi
rsv_desc %>%
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
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  dplyr::mutate(phase = ifelse(date(fdate) >='2020-01-01' & date(fdate) <='2021-06-30', "covid",
                               ifelse(date(fdate) >='2021-07-01' & date(fdate) <='2022-06-30', "t2021",
                                      ifelse(date(fdate) >='2022-07-01' & date(fdate) <='2023-06-30', "t2022", NA_character_)))) %>%
  dplyr::filter(!is.na(phase)) %>%
  dplyr::group_by(phase, clim) %>%
  dplyr::summarise(Q2str = median(strin_indxL),
                   Q1str = quantile(strin_indxL, 0.25),
                   Q3str = quantile(strin_indxL, 0.75)) %>%
  dplyr::ungroup()

#distribution of population density
#hemi
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(hemi) %>%
  summarise(Q2pop = median(pop_dens),
            Q1pop = quantile(pop_dens, 0.25),
            Q3pop = quantile(pop_dens, 0.75))

#climate zone
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(clim) %>%
  summarise(Q2pop = median(pop_dens),
            Q1pop = quantile(pop_dens, 0.25),
            Q3pop = quantile(pop_dens, 0.75))

#distribution of median age
#hemi
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(hemi) %>%
  summarise(Q2pop = median(med_age),
            Q1pop = quantile(med_age, 0.25),
            Q3pop = quantile(med_age, 0.75))

#climate zone
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  dplyr::group_by(clim) %>%
  summarise(Q2pop = median(med_age),
            Q1pop = quantile(med_age, 0.25),
            Q3pop = quantile(med_age, 0.75))

#====================================================================
# plot stringency and population density
#====================================================================

D1 <-
rsv_desc %>%
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
rsv_desc %>%
  dplyr::left_join(stringency) %>%
  ggplot() +
  geom_line(aes(x = fdate, y = strin_indx, color = clim)) +
  theme_bw(base_size = 14, base_family = 'American typewriter') +
  labs(x = "Date of report", y = "Stringency index", title = "(B)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none", legend.title = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_color_manual(values=c("#00BFC4", "#FFC133"))

D3 <- 
rsv_desc %>%
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
  theme(legend.position = "right", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#36454F", "#7CAE00"))

D4 <-
rsv_desc %>%
  dplyr::left_join(
    stringency %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(pop_den = mean(pop_dens)) %>%
      dplyr::ungroup()) %>%
  ggplot(aes(y = pop_den, fill = clim)) + 
  geom_boxplot(color = "black", size = 1) +
  theme_bw(base_size = 14, base_family = "American typewriter") +
  scale_y_continuous(trans = log10_trans()) +
  labs(x = "", y = "Population density (log10)", title = "(D)") +
  theme(plot.title = element_text(size = 20), axis.text.x = element_text(face = "bold", size = 0), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "right", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  scale_fill_manual(values=c("#00BFC4", "#FFC133"))

# combine all the plots
ggsave(here("output", "sfig5_predDdistr.png"),
       plot = (D1 | D3)/(D2 | D4),
       width = 16, height = 14, unit="in", dpi = 300)

#====================================================================
#COMBINING EPIDEMIC ONSET AND INTENSITY ON THE SAME PLOT
#====================================================================

#intensity first and second wave
OnsetIntens <-
  bind_cols(
    DSintense1 %>% rename("intens1" = "intensity") %>% select(country, intens1),
    DSintense2 %>% rename("intens2" = "intensity") %>% select(intens2),
    intense2 %>% dplyr::select(wave3) %>% dplyr::rename("intens3" = "wave3"))

#onset first and second wave
OnsetIntens1 <- 
  DSonset1 %>% 
  dplyr::filter(event == 1) %>% 
  dplyr::mutate(wave = "first wave") %>% 
  left_join(OnsetIntens) %>%
  mutate(hemix = if_else(country %in% c("Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia", "India"), "Northern Hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States"), "Northern Hemisphere", "Southern Hemisphere"))) %>%
  dplyr::select(country, fdate, intens1, intens2, intens3, hemix, wave)

OnsetIntens2 <- 
  DSonset2 %>% 
  dplyr::filter(event == 1) %>% 
  dplyr::mutate(wave = "second wave") %>% 
  left_join(OnsetIntens) %>%
  mutate(hemix = if_else(country %in% c("Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia", "India"), "Northern Hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States"), "Northern Hemisphere", "Southern Hemisphere"))) %>%
  dplyr::select(country, fdate, intens1, intens2, intens3, hemix, wave)

OnsetIntens3 <- 
  OnsetIntens %>%
  left_join(
  onset2x %>% 
  dplyr::filter(wave == "wave3") %>% 
  dplyr::mutate(wave = "third wave")) %>% 

  mutate(hemix = if_else(country %in% c("Costa Rica", "Japan", "Canada", "Denmark", "France", "Hungary", "Iceland", "Mexico", "Mongolia", "India"), "Northern Hemisphere",
                         if_else(country %in% c("Germany",  "Ireland",  "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States"), "Northern Hemisphere", "Southern Hemisphere"))) %>%
  dplyr::select(country, date, intens1, intens2, intens3, hemix, wave)


#combine first, second and third wave onsets as adjacent columns
OnsetIntens4 <-
  bind_cols(OnsetIntens1, 
            OnsetIntens2 %>% 
              dplyr::select(country, fdate, wave) %>% 
              dplyr::rename("countryx" = "country", "fdatex" = "fdate", "wavex" = "wave"),
            OnsetIntens3 %>% 
              dplyr::select(country, date, wave) %>% 
              dplyr::rename("countryxx" = "country", "fdatexx" = "date", "wavexx" = "wave"))

#plot the bubble of waves and intensity
B <-
ggplot() +
  geom_point(data = OnsetIntens1, aes(x = fdate, y = wave, color = country, size = intens1), shape = 21, stroke = 2, alpha = 0.75,  stat = "identity") +
  geom_point(data = OnsetIntens2, aes(x = fdate, y = wave, color = country, size = intens2), shape = 21, stroke = 2, alpha = 0.75, stat = "identity") +
  geom_point(data = OnsetIntens3 %>% dplyr::filter(!is.na(date)), aes(x = date, y = wave, color = country, size = intens3), shape = 21, stroke = 2, alpha = 0.75, stat = "identity") +
  scale_size_continuous(limits = c(0.01, 10), range = c(1,10)) + 
  geom_segment(data = OnsetIntens4, aes(x = fdate, y = wave, xend = fdatex, yend = wavex, colour = country), stat = "identity") +
  geom_segment(data = OnsetIntens4 %>% dplyr::filter(!is.na(fdatexx)), aes(x = fdatex, y = wavex, xend = fdatexx, yend = wavexx, colour = country), stat = "identity") +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Southern Hemisphere"), aes(xmin = date('2020-03-01'), xmax = date('2020-06-30'), ymin = 0, ymax = Inf), alpha = 0.04) +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Southern Hemisphere"), aes(xmin = date('2021-03-01'), xmax = date('2021-06-30'), ymin = 0, ymax = Inf), alpha = 0.04) +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Southern Hemisphere"), aes(xmin = date('2022-03-01'), xmax = date('2022-06-30'), ymin = 0, ymax = Inf), alpha = 0.04) +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Northern Hemisphere"), aes(xmin = date('2020-09-01'), xmax = date('2020-12-31'), ymin = 0, ymax = Inf), alpha = 0.01) +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Northern Hemisphere"), aes(xmin = date('2021-09-01'), xmax = date('2021-12-31'), ymin = 0, ymax = Inf), alpha = 0.01) +
  geom_rect(data = dplyr::filter(OnsetIntens1, hemix == "Northern Hemisphere"), aes(xmin = date('2022-09-01'), xmax = date('2022-12-31'), ymin = 0, ymax = Inf), alpha = 0.01) +
  geom_text(data = OnsetIntens4, aes(x = fdate, y = wave, label = str_sub(country, 1,2)), size = 4, angle = "45", vjust = -0.5, hjust = 1.5, fontface = "bold", position = position_dodge(width = 1)) +
  scale_x_date(breaks = c((date(c("2020-03-01", "2020-06-01", "2020-09-01", "2020-12-01", "2021-03-01", "2021-06-01", "2021-09-01", "2021-12-01", "2022-03-01", "2022-06-01", "2022-09-01", "2022-12-01")))),
               limits = c((date(c("2020-03-01", "2023-01-01")))),
               date_labels = "%m-%Y") +
  facet_grid(hemix~.) +
  theme_bw(base_size = 14, base_family = "Amrican Typewriter") + 
  labs(title = "", x = "Epidemic onset date", y = "RSV epidemic waves following COVID-19 suppression") +
  theme(legend.text = element_text(size = 10), legend.position = "right", legend.title = element_text(size = 11)) +
  guides(size = guide_legend(title = "Intensity"), color = guide_legend(title = "Country")) +
  theme(strip.text.x = element_text(size = 0), strip.text.y = element_text(size = 16), strip.background = element_rect(fill = "gray80")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

#plot the onset-intensity relationship
ggsave(here("output", "fig5_onsetIntens.png"),
       plot = (B),
       width = 18, height = 10, unit="in", dpi = 300)

#====================================================================
#X-Y PLOT OF EPIDEMIC ONSET AND INTENSITY ON THE SAME PLOT
#====================================================================

A <-
  bind_rows(
    OnsetIntens4 %>% select(country, fdate, hemix, intens1) %>% dplyr::rename("intens" = "intens1") %>% dplyr::mutate(wave = "first wave"),
    OnsetIntens4 %>% select(country, fdatex, hemix, intens2) %>% dplyr::rename("intens" = "intens2", "fdate" = "fdatex")  %>% dplyr::mutate(wave = "second wave"),
    OnsetIntens4 %>% select(country, fdatexx, hemix, intens3) %>% dplyr::rename("intens" = "intens3",  "fdate" = "fdatexx")  %>% dplyr::mutate(wave = "third wave", fdate = date(fdate))) %>%
  dplyr::group_by(hemix, wave) %>%
  mutate(mintens = round(mean(intens, na.rm = TRUE), digits = 2)) %>%

  ggplot() +
  geom_point(aes(x = fdate, y = intens, color = country, size = intens), shape = 21, stroke = 2, alpha = 0.75,  stat = "identity") +
  scale_x_date(breaks = c((date(c("2020-06-01", "2020-12-01", "2021-06-01", "2021-12-01", "2022-06-01", "2022-12-01")))),
               limits = c((date(c("2020-06-01", "2023-01-01")))),
               date_labels = "%m-%Y") +
  #scale_x_date(date_labels = "%m-%Y") +
  geom_hline(aes(yintercept = mintens), linetype = "dotted", size = 1.5) +
  geom_text(aes(x = date(c("2022-06-01")), y = 8, label = paste0("mean intensity: ", mintens)), color = "black", size = 6) +
  facet_grid(hemix~wave, scales = "free_x") +
  theme_bw(base_size = 16) + 
  labs(title = "", x = "Epidemic onset date", y = "RSV Intensity") +
  theme(legend.text = element_text(size = 10), legend.position = "right", legend.title = element_text(size = 11)) +
  guides(size = "none", color = guide_legend(title = "Country")) +
  theme(strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16), strip.background = element_rect(fill = "gray80")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

#plot the onset-intensity relationship
ggsave(here("output", "sfig12_onsetIntens.png"),
       plot = (A),
       width = 18, height = 9, unit="in", dpi = 300)
