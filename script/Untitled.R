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

