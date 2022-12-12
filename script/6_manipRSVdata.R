#26/11/2022

#=========================================================
#separate sentinel from not defined source/origin of cases 
rsvds %>% 
  dplyr::filter(country %in% c("Cameroon", "Central African Republic", "Côte d'Ivoire", "Madagascar",
                               "Mozambique", "Uganda", "South Africa", 
                               "India",
                               "Australia", "Japan", "Mongolia", "Malaysia",
                               "Oman", "Qatar")) %>%
  group_by(sentin)
  
  
  group_by(region, fluseas, hemi, country, date, yr, wk, sentin, cases) %>%
  tally()
  

#plotting RSV cases in Africa/South East Asia/Middle East/Western pacific
rsvds %>%
  dplyr::filter(country %in% c("Cameroon", "Central African Republic", "Côte d'Ivoire", "Madagascar",
                               "Mozambique", "Uganda", "South Africa", 
                               "India",
                               "Australia", "Japan", "Mongolia", "Malaysia",
                               "Oman", "Qatar")) %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")

#=========================================================

#separate sentinel from not- defined source/orig-in of cases 
rsvds %>%
  dplyr::filter(
    country %in% c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland",
                   "Ireland", "Denmark", "Finland", "Sweden", "Malta", "United Kingdom, England", 
                   "United Kingdom, Northern Ireland", "United Kingdom, Scotland",
                   "Bulgaria", "Belarus", "Russian Federation", "Hungary", "Poland", "Slovakia")) %>%
  group_by(country, sentin) %>%
  tally()

#plotting RSV cases in Europe
rsvds %>%
  dplyr::filter(
    country %in% c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland",
                   "Ireland", "Denmark", "Finland", "Sweden", "Malta", "United Kingdom, England", 
                   "United Kingdom, Northern Ireland", "United Kingdom, Scotland",
                   "Bulgaria", "Belarus", "Russian Federation", "Hungary", "Poland", "Slovakia")) %>%
  mutate(country = if_else(country == "United Kingdom, Northern Ireland", "N Ireland",
                           if_else(country == "United Kingdom, Scotland", "Scotland",
                                   if_else(country == "United Kingdom, England", "England",
                                           if_else(country == "Russian Federation","Russia", country))))) %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")

#=========================================================

#plotting RSV cases in Americas
rsvds %>%
  dplyr::filter(
    country %in% c("Argentina", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Canada", "Colombia", "Costa Rica",
                   "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico",
                   "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay")) %>%
  mutate(country = if_else(country == "Bolivia (Plurinational State of)", "Bolivia",
                           if_else(country == "Dominican Republic", "Dominica", country))) %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")

#=========================================================

#plotting RSV cases in Africa/South East Asia/Middle East/Western pacific
rsvds %>%
  dplyr::filter(country %in% c("Cameroon", "Central African Republic", "Côte d'Ivoire", "Madagascar",
                               "Mozambique", "Uganda", "South Africa", 
                               "India",
                               "Australia", "Japan", "Mongolia", "Malaysia",
                               "Oman", "Qatar")) %>%
  ggplot(aes(x = factor(month(date)), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")

#=========================================================

#plotting RSV cases in Europe
rsvds %>%
  dplyr::filter(
    country %in% c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland",
                   "Ireland", "Denmark", "Finland", "Sweden", "Malta", "United Kingdom, England", 
                   "United Kingdom, Northern Ireland", "United Kingdom, Scotland",
                   "Bulgaria", "Belarus", "Russian Federation", "Hungary", "Poland", "Slovakia")) %>%
  mutate(country = if_else(country == "United Kingdom, Northern Ireland", "N Ireland",
                           if_else(country == "United Kingdom, Scotland", "Scotland",
                                   if_else(country == "United Kingdom, England", "England",
                                           if_else(country == "Russian Federation","Russia", country))))) %>%
  ggplot(aes(x = factor(month(date)), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")

#=========================================================

#plotting RSV cases in Americas
rsvds %>%
  dplyr::filter(
    country %in% c("Argentina", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Canada", "Colombia", "Costa Rica",
                   "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico",
                   "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay")) %>%
  mutate(country = if_else(country == "Bolivia (Plurinational State of)", "Bolivia",
                           if_else(country == "Dominican Republic", "Dominica", country))) %>%
  ggplot(aes(x = factor(month(date)), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato") +
  labs(title = , x = "MMWR Date", y = "RSV cases")
