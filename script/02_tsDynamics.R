#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#TIME SERIES OF RSV DYNAMICS
#====================================================================

#RSV time series dynamics dataset
country_ts <-
  rsv_dtw %>%
  dplyr::group_by(country, date, wk) %>%
  dplyr::summarise(cases = mean(cases, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cases = round(zoo::rollmean(cases, k = 3, fill = 0, align = 'right'))) %>%
  dplyr::ungroup()

#create empty lists
X <- list()

#time series dynamics for all included countries
for (i in c("Argentina", "Australia",  "Paraguay", "Peru", "South Africa", "Brazil","Colombia")){
  X[[i]] <- 
    country_ts %>%
    dplyr::filter(country == i) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE),
                  newCases = cases[which.max(date == newDate)],
                  hemi = "Southern Hemisphere")
}

#time series dynamics for all included countries
for (i in c("Costa Rica", "India", "Canada", "Denmark", "France", "Germany", "Hungary", "Japan", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")){
  X[[i]] <- 
    country_ts %>%
    dplyr::filter(country == i) %>%
    dplyr::mutate(newDate = max(date, na.rm = TRUE),
                  newCases = cases[which.max(date == newDate)],
                  hemi = "Northern Hemisphere")
} 

A <-
bind_rows(X) %>%
  ggplot(aes(x = date, y = cases)) +
  #geom_point(aes(x = newDate, y = newCases), color = "red", size = 2.5) +
  theme_bw(base_size = 11, base_family = "Lato", base_line_size = 1.5) + 
  labs(title = "", x = "Reporting date", y = "") +
  guides(color = guide_legend(title = "")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(.~ country, ncol = 4, scales = "free_y") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3)) +
  theme(legend.position = "bottom", legend.text=element_text(size = 13), strip.text.x = element_text(size = 16)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2017-02-01'), xmax = date('2017-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2018-02-01'), xmax = date('2018-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2019-02-01'), xmax = date('2019-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2020-02-01'), xmax = date('2020-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2021-02-01'), xmax = date('2021-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2022-02-01'), xmax = date('2022-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Southern Hemisphere"), aes(xmin = date('2023-02-01'), xmax = date('2023-07-30'), ymin = 0, ymax = Inf), fill = "gray95") +
  
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2017-10-01'), xmax = date('2018-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2018-10-01'), xmax = date('2019-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2019-10-01'), xmax = date('2020-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2020-10-01'), xmax = date('2021-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2021-10-01'), xmax = date('2022-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2022-10-01'), xmax = date('2023-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_rect(data = bind_rows(X) %>% dplyr::filter(hemi == "Northern Hemisphere"), aes(xmin = date('2023-10-01'), xmax = date('2024-03-28'), ymin = 0, ymax = Inf), fill = "gray95") +
  geom_line(size = 2) + 
  geom_vline(xintercept = date('2020-04-12'), linetype="dashed", color = "red", size = 1.4)
  

#combined plots and saving
ggsave(here("output", "fig2_tsdyn.png"),
       plot = (A),
       width = 25, height = 20, unit="in", dpi = 300)

#====================================================================

#delete all temporary datasets
rm(list = grep("rsv_all|rsv_dtw|climate|stringency", ls(), value = TRUE, invert = TRUE))
