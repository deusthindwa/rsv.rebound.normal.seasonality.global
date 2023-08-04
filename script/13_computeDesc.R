#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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
