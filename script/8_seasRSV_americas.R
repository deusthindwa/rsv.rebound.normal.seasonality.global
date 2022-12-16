#By Deus Thindwa
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#year on year weekly RSV cases in Americas countries
print(
rsv_amer %>%
  ggplot(aes(x = date(date), y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Weekly RSV cases in Americas", x = "MMWR Date", y = "RSV cases")
)

#year on year monthly RSV cases in Americas
print(
rsv_amer %>%
  arrange(date, country) %>%
  group_by(date = round_date(date, "month"), country) %>%
  summarise(cases = mean(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() + 
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 10, base_family = "Lato", base_line_size = 1) +
  labs(title = "Monthly RSV cases in Americas", x = "MMWR Date", y = "RSV cases")
)

#seasonal RSV dynamics before and after COVID-19 in Americas
print(
rsv_amer %>%
  mutate(perioda = if_else(date < "2020-01-01", "PreCOVID-19", 
                           if_else(date >= "2021-01-01" , "PostCOVID-19", NA_character_)),
         mont = month(date, label = TRUE, abbr = TRUE)) %>%
  filter(!is.na(perioda)) %>%
  arrange(date, country) %>%
  group_by(perioda, country, mont) %>%
  summarise(mcases = mean(cases, rm.na = TRUE)) %>%
  ungroup() %>%
  group_by(perioda, country) %>%
  mutate(pcases = mcases/sum(mcases)) %>%
  
  ggplot(aes(x = mont, y = pcases, group = perioda, color = perioda)) +
  geom_line(size = 1) + 
  scale_y_continuous(breaks = seq(0, 1, 0.10), labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(. ~ country, scales = "free_y") +
  theme_bw(base_size = 12, base_family = "Lato", base_line_size = 1) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.3)) +
  labs(title = "Seasonal dynamics of RSV cases in Americas", x = "Months", y = "RSV cases (%)") + 
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "Reporting period"))
)

