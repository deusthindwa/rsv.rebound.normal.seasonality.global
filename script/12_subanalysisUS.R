#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Distinct global disruptions in respiratory syncytial virus seasonal timing following COVID-19 pandemic

#====================================================================
#TIME SERIES OF RSV DYNAMICS IN NON-TEMEPRATE COUNTRY
#====================================================================

country_ts <-
  rsv_all %>%
  dplyr::group_by(country, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

#create empty lists
X <- list()

#weekly seasonal RSV dynamics for non-temperate countries
for (i in c("United States North East", "United States South", "United States West", "United States Mid West")) {
  X[[i]] <-    
    country_ts %>%
    filter(country == i) %>%
    mutate(newDate = max(date, na.rm = TRUE), 
           newCases = cases[which.max(date == newDate)])
}

A <-
  bind_rows(X) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1.2) + 
  geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) + 
  labs(title = "", x = "Reporting date", y = "RSV cases") +
  guides(color = guide_legend(title = "")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'American Typewriter') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

#====================================================================
#WEEKLY RSV DYNAMICS IN NON-TEMEPRATE COUNTRY
#====================================================================

#create empty lists
Y <- list()

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23; wkno2 = 24:53; wkno = c(24:53, 1:23)

#weekly seasonal RSV dynamics for tenperate countries
for (i in c("United States North East", "United States South", "United States West", "United States Mid West")) {
  
  Y[[i]] <-
    rsv_all %>%
    dplyr::group_by(country, yr) %>% 
    mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
                            wk %in% wkno1 & yr == 2018 ~ "2017/18",
                            wk %in% wkno2 & yr == 2018 ~ "2018/19",
                            wk %in% wkno1 & yr == 2019 ~ "2018/19",
                            wk %in% wkno2 & yr == 2019 ~ "2019/20",
                            wk %in% wkno1 & yr == 2020 ~ "2019/20",
                            wk %in% wkno2 & yr == 2020 ~ "2020/21",
                            wk %in% wkno1 & yr == 2021 ~ "2020/21",
                            wk %in% wkno2 & yr == 2021 ~ "2021/22",
                            wk %in% wkno1 & yr == 2022 ~ "2021/22",
                            wk %in% wkno2 & yr == 2022 ~ "2022/23",
                            wk %in% wkno1 & yr == 2023 ~ "2022/23",
                            TRUE ~ NA_character_)) %>%
    ungroup() %>%
    
    #compute the cases by covid period
    dplyr::mutate(covid = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19)",
                                  if_else(seas == "2021/22", "2021/22", 
                                          if_else(seas == "2022/23", "2022/23", NA_character_)))) %>%
    dplyr::filter(!is.na(covid), country == i) %>%
    dplyr::group_by(country, wk, covid) %>%
    dplyr::mutate(cases = mean(cases, rm.na = TRUE)) %>%
    dplyr::ungroup() %>%
    group_by(country) %>%
    dplyr::mutate(yr = as.factor(yr), 
                  wk = factor(wk, levels(factor(wk))[c(wkno)]),
                  cases = round(cases, digits = 0),
                  newDate = max(date, na.rm = TRUE),
                  newWk = wk[which.max(date == newDate)],
                  newCases = cases[which.max(date == newDate)]) %>%
    ungroup()
}

B <-
bind_rows(Y) %>%
  ggplot(aes(x = wk, y = cases, group = covid, color = covid)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
  scale_colour_brewer(palette = 7, direction = 1) + 
  labs(title = "", x = "Reporting week", y = "") +
  guides(color = guide_legend(title = "")) +
  scale_x_discrete(breaks = seq(1, 52, 6)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'American Typewriter') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

#====================================================================
#RSV ONSET BY UNITED STATES REGION
#====================================================================

#merge the RSV onset datasets
rsv_onset <-
  rows_append(rsv_onset_temp, rsv_onset_trop) %>% 
  left_join(climate %>% select(country, clim_zone)) %>%
  left_join(rsv_all %>% select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
  mutate(covper = if_else(covper == "2021/22", "y2021",
                          if_else(covper == "2022/23", "y2022",
                                  if_else(covper == "2021", "y2021",
                                          if_else(covper == "2022", "y2022", 
                                                  if_else(covper == "preCOVID-19", "precov", NA_character_))))))

#only consider countries with all the data e.g., preCovid, 2021, and 2022
rsv_onset <- 
  rsv_onset %>%
  left_join(
    rsv_onset %>%
      group_by(country) %>%
      tally()) %>%
  filter(n == 3)

#create a loop to hold data for plotting
scatterXY <- list()

#loop in the specified vector content
for (i in c("North Americas")) {
  
  rsv_onset_cz <-  
    rsv_onset %>% 
    filter(region == i, (country == "United States North East" | country == "United States South" | country == "United States West" | country == "United States Mid West")) %>%
    pivot_wider(names_from = covper, values_from = epiwk) %>%
    fill(precov, .direction = "up")
  
  #reshape the onset datasets for scatter plotting
  scatterXY[[i]] <-
    left_join(
      left_join(
        rsv_onset_cz %>%
          filter(is.na(y2021), is.na(y2022)) %>%
          select(country, precov, l_epiwk, u_epiwk) %>%
          rename("lwk1" = "l_epiwk", "uwk1" = "u_epiwk"),
        
        rsv_onset_cz %>%
          select(country, y2021, l_epiwk, u_epiwk) %>%
          filter(!is.na(y2021)) %>%
          rename("lwk2" = "l_epiwk", "uwk2" = "u_epiwk")),
      
      rsv_onset_cz %>%
        select(country, y2022, l_epiwk, u_epiwk) %>%
        filter(!is.na(y2022)) %>%
        rename("lwk3" = "l_epiwk", "uwk3" = "u_epiwk")) %>% 
    mutate(corr2021 = (1-round(MAPE(y2021, precov), digits = 3))*100,
           corr2022 = (1-round(MAPE(y2022, precov), digits = 3))*100)
}

C <-
  bind_rows(scatterXY, .id = "id") %>%
  mutate(precov = round(precov, digits = 0), y2021 = round(y2021, digits = 0)) %>%
  ggplot(aes(x = precov, y = y2021, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk2, ymax = uwk2), width = 0, size = 1, position = position_dodge(width = 0.5)) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 10, y = 50, label = paste0("α = ", corr2021, "%")), color = "black", size = 6, family = "Times New Roman", fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 52, 6), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(0, 52, 6), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'American Typewriter') +
  labs(x = "", y = "RSV onset in 2021", title = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

D <-
  bind_rows(scatterXY, .id = "id") %>%
  mutate(precov = round(precov, digits = 0), y2022 = round(y2022, digits = 0)) %>%
  ggplot(aes(x = precov, y = y2022, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), shape = 4, stroke = 2) +
  geom_errorbar(aes(ymin = lwk3, ymax = uwk3), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 10, y = 50, label = paste0("α = ", corr2022, "%")), color = "black", size = 6, family = "Times New Roman", fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 52, 6), limits = c(0,52)) +
  scale_y_continuous(breaks = seq(0, 52, 6), limits = c(0,52)) +
  theme_bw(base_size = 14, base_family = 'American Typewriter') +
  labs(x = "PreCOVID-19 mean RSV onset (2017-19)", y = "RSV onset in 2022", title ="") +
  theme(legend.position = c(0.7, 0.2), legend.title = element_blank(), legend.key.size = unit(1.5, 'lines')) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

#====================================================================
#====================================================================

#combined plots
ggsave(here("output", "S3Fig_dynRSV_US.png"),
       plot = (A | B | (C/D) | plot_layout(ncol = 3, width = c(2, 2, 1))),
       width = 25, height = 13, unit="in", dpi = 300)
