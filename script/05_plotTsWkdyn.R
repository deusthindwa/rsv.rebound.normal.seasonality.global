#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES OF RSV DYNAMICS
#====================================================================

#RSV time series dynamics dataset
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
Y <- list()

#time series dynamics for sub/tropical countries
for (i in c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", "Peru", "South Africa")) {
X 
  X[[i]] <-    
  country_ts %>%
    dplyr::filter(country == i) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE),
                  newCases = cases[which.max(date == newDate)])
}

#time series dynamics for temperate countries
for (i in c("Brazil", "Canada", "Denmark", "France", "Germany", "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) {
  
  Y[[i]] <-
    country_ts %>%
    dplyr::filter(country == i) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE), 
                  newCases = cases[which.max(date == newDate)])
}

#time series plots
A <-
  bind_rows(Y) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1.2) + 
  geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) + 
  labs(title = "", x = "Reporting date", y = "RSV cases (2-weeks moving average)") +
  guides(color = guide_legend(title = "")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 5, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

B <-
  bind_rows(X) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line(size = 1.2) + 
  geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) + 
  labs(title = "", x = "Reporting date", y = "") +
  guides(color = guide_legend(title = "")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

#combined plots and saving
ggsave(here("output", "fig1_tsdynSupp.png"),
       plot = (A | B | plot_layout(ncol = 2, width = c(2.3, 1))),
       width = 25, height = 13, unit="in", dpi = 300)

#====================================================================
#WEEKLY DYNAMICS OF RSV
#====================================================================

#weekly dynamics dataset
rsv_weekly <-
  rsv_all %>%
  dplyr::filter(country %in% c("Argentina", "Australia", "Brazil", "Canada", "Colombia", "Costa Rica", "Denmark", "France", "Germany", "Hungary", "Iceland", "India", "Ireland", "Japan",
                               "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Paraguay", "Peru", "Portugal", "Qatar", "Scotland", "South Africa", "Spain", "Sweden", "United States")) %>%
  dplyr::left_join(climate)

#create empty lists
X <- list()
Y <- list()

#weekly seasonal RSV dynamics for each year
wkno1 = 1:23; wkno2 = 24:53; wkno = c(24:53, 1:23)

#weekly seasonal dynamics for sub/tropical countries
for (i in c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", "Peru", "South Africa")) {
  
  X[[i]] <-    
    rsv_weekly %>%
    dplyr::mutate(covid = if_else(date < "2020-01-01", " PreCOVID (2017-19) mean cases", 
                                  if_else(year(date) == 2020, NA_character_, 
                                          if_else(year(date) == 2021, "2021",
                                                  if_else(year(date) == 2022, "2022",
                                                          if_else(year(date) == 2023, "2023",
                                                                  if_else(year(date) == 2024, "2024",
                                                                          if_else(year(date) == 2025, "2025",
                                                                                  if_else(year(date) == 2026, "2026", "2027")))))))),
                  seas = yr) %>%
    dplyr::filter(!is.na(covid), country == i) %>%
    dplyr::group_by(country, wk, covid) %>%
    dplyr::mutate(cases = round(mean(cases, rm.na = TRUE), digits = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE),
                  newWk = wk[which.max(date == newDate)],
                  newCases = cases[which.max(date == newDate)]) %>%
    dplyr::ungroup()
  
}

#weekly seasonal dynamics for temperate countries
for (i in c("Brazil", "Canada", "Denmark", "France", "Germany", "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) {
  
  Y[[i]] <-
    rsv_weekly %>%
    dplyr::group_by(country, yr) %>% 
    dplyr::mutate(seas = case_when(wk %in% wkno2 & yr == 2017 ~ "2017/18",
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
    dplyr::ungroup() %>%
    
    #compute the cases by covid period
    dplyr::mutate(covid = if_else(seas == "2017/18" | seas == "2018/19" | seas == "2019/20", " PreCOVID (2017-19) mean cases",
                                  if_else(seas == "2021/22", "2021/22", 
                                          if_else(seas == "2022/23", "2022/23", NA_character_)))) %>%
    dplyr::filter(!is.na(covid), country == i) %>%
    dplyr::group_by(country, wk, covid) %>%
    dplyr::mutate(cases = mean(cases, rm.na = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(yr = as.factor(yr), 
                  wk = factor(wk, levels(factor(wk))[c(wkno)]),
                  cases = round(cases, digits = 0),
                  newDate = max(date, na.rm = TRUE),
                  newWk = wk[which.max(date == newDate)],
                  newCases = cases[which.max(date == newDate)]) %>%
    dplyr::ungroup()
}

#weekly plots
C <-
  bind_rows(Y) %>%
  ggplot(aes(x = wk, y = cases, group = covid, color = covid)) +
  geom_line(size = 1.5) +
  geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
  scale_colour_brewer(palette = 7, direction = 1) + 
  labs(title = "", x = "Reporting week", y = "Weekly RSV cases") +
  guides(color = guide_legend(title = "")) +
  scale_x_discrete(breaks = seq(1, 52, 6)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 5, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

D <-
  bind_rows(X) %>%
  ggplot(aes(x = wk, y = cases, color = covid)) +
  geom_line(size = 1.5) + 
  geom_point(aes(x = newWk, y = newCases, color = covid), size = 2) +
  scale_colour_brewer(palette = 7, direction = 1) + 
  labs(title = "", x = "Reporting week", y = "") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = seq(1, 52, 6)) +
  facet_wrap(.~ country, ncol = 2, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16))

#combined plots and saving
ggsave(here("output", "fig1_wkdynMain.png"),
       plot = (C | D | plot_layout(ncol = 2, width = c(2.3, 1))),
       width = 25, height = 13, unit="in", dpi = 300)

#====================================================================

#delete all temporary datasets
rm(list = grep("rsv_all|climate", ls(), value = TRUE, invert = TRUE))
