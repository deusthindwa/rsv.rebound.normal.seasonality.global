#By Deus Thindwa
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

# filter to only have these countries included from African, South East Asia, Western pacific and Middle East
rsv_asmw <- rsvds %>%
  dplyr::filter(country %in% c("Cameroon", "Central African Republic", "Côte d'Ivoire", "Madagascar", "South Africa", #African
                               "India", #South East Asia
                               "Australia", "Japan", "Mongolia", "Malaysia", # Western pacific
                               "Oman", "Qatar")) #Middle East

#Not defined may include sentinel or non-sentinel data
rsv_asmw <- rsv_asmw %>% 
  group_by(region, fluseas, hemi, country, date, wk, mon, yr) %>%
  summarise(cases = sum(cases))

#check for duplicates by country and date
rsv_asmw <- rsv_asmw %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#---------------------------------------------------------------

#filter to only have these countries included from Europe
rsv_euro <- rsvds %>%
  dplyr::filter(
    country %in% c("France", "Germany", "Netherlands", "Spain", "Portugal", "Iceland",
                   "Ireland", "Denmark", "Finland", "Sweden", "United Kingdom, England", 
                   "United Kingdom, Northern Ireland", "United Kingdom, Scotland",
                   "Bulgaria", "Belarus", "Russian Federation", "Hungary", "Poland", "Slovakia")) %>%
  mutate(country = if_else(country == "United Kingdom, Northern Ireland", "NIreland",
                           if_else(country == "United Kingdom, Scotland", "Scotland",
                                   if_else(country == "United Kingdom, England", "England",
                                           if_else(country == "Russian Federation","Russia", country)))))

#combine sentinel and non-sentinel data from 2014 on-wards
rsv_euro <- rsv_euro %>% 
  dplyr::filter(sentin != "NOTDEFINED", yr >= 2014) %>%
  group_by(region, fluseas, hemi, country, date, wk, mon, yr) %>%
  summarise(cases = sum(cases))

#check for duplicates by country and date
rsv_euro <- rsv_euro %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#---------------------------------------------------------------

# filter to only have these countries included from Americas
rsv_amer <- rsvds %>%
  dplyr::filter(
    country %in% c("Argentina", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Canada", "Colombia", "Costa Rica",
                   "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico",
                   "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay")) %>%
  mutate(country = if_else(country == "Bolivia (Plurinational State of)", "Bolivia",
                           if_else(country == "Dominican Republic", "Dominica", country)))

#combine sentinel and non-sentinel data or use undefined source of RSV cases
rsv_amer <- rsv_amer %>% 
  group_by(region, fluseas, hemi, country, date, wk, mon, yr) %>%
  summarise(cases = sum(cases))

#check for duplicates by country and date
rsv_amer <- rsv_amer %>% 
  dplyr::distinct(country, date, .keep_all = TRUE)

#====================================================================

#combine all the datasets for regional seasonal dynamics plotting
rsv_regn <- rsv_asmw %>% 
  bind_rows(rsv_euro) %>%
  bind_rows(rsv_amer)

#combine sentinel and non-sentinel data or use undefined source of RSV cases
rsv_regn <- rsv_regn %>% 
  group_by(region, date, wk, mon, yr) %>%
  summarise(cases = sum(cases))

#check for duplicates by country and date
rsv_regn <- rsv_regn %>% 
  dplyr::distinct(region, date, .keep_all = TRUE) %>%
  mutate(regionx = if_else(region == "AFR", "AFRICA",
                           if_else(region == "AMR", "AMERICAS",
                                   if_else(region == "EUR", "EUROPE",
                                           if_else(region == "SEAR", "SOUTH EAST ASIA", "WESTERN PACIFIC")))))

#====================================================================

#year on year weekly RSV cases by regions
print(
rsv_regn %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ regionx, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Weekly RSV cases by regions", x = "MMWR Date", y = "RSV cases")
)

#year on year monthly RSV cases by regions
print(
rsv_regn %>%
  arrange(date, regionx) %>%
  group_by(date = round_date(date, "month"), regionx) %>%
  summarise(cases = mean(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ regionx, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Monthly RSV cases by regions", x = "MMWR Date", y = "RSV cases")
)

#seasonal RSV dynamics before and after COVID-19 by regions
print(
rsv_regn %>%
  mutate(perioda = if_else(date < "2020-01-01", "PreCOVID-19", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19", NA_character_)),
         mont = month(date, label = TRUE, abbr = TRUE)) %>%
  filter(!is.na(perioda)) %>%
  arrange(date, regionx) %>%
  group_by(perioda, regionx, mont) %>%
  summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  ungroup() %>%
  group_by(perioda, regionx) %>%
  mutate(pcases = mcases/sum(mcases)) %>%
  
  ggplot(aes(x = mont, y = pcases, group = perioda, color = perioda)) +
  geom_line(size = 1) + 
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(. ~ regionx, scales = "free_y") +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
  labs(title = "Seasonal dynamics of RSV cases by regions", x = "Months", y = "RSV cases (%)") + 
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Reporting period"))
)
