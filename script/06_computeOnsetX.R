#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#ONSET TIMING OF RSV CASES IN EACH COUNTRY
#====================================================================

#split the dataset by country/regionUS to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
  dplyr::arrange(country, yr, wk, cases) %>%
  group_by(country) %>%
  mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()),
         seqwk2 = seqwk) %>%
  ungroup() %>%
  select(seqwk, seqwk2, cases, country) %>%
  split(list(.$country))

#delete empty country.yr data frames from list X (they have less than threshold total cases [tcases] throughout the year)
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty list to store GAM models & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv.pred <- list()
deriv2.pred <- list()
rsv_onset2 <- list()

#run the GAM models where high number of knots are automatically selected via cross validation
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = seqwk, bs = "ps", k = 25),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
  
} #+ s(seqwk2, bs = "re")

#iterate for each country, extract fitted case values
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% 
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
 
  deriv.pred[[i]] = data.frame(deriv = diff(tsDS[[i]]$fitcases)/diff(tsDS[[i]]$seqwk)) %>%
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))

  deriv2.pred[[i]] = data.frame(deriv2 = diff(deriv.pred[[i]]$deriv)/diff(deriv.pred[[i]]$seqwk)) %>%
    mutate(seqwk = seq.int(from = 1, by = 1, length.out = n()))
  
#.Machine$double.eps default MinPeakHeight
  rsv_onset2[[i]] = data.frame(gsignal::findpeaks(deriv2.pred[[i]]$deriv2, MinPeakHeight = 0.01, MinPeakDistance = 1, MinPeakWidth = 1, MaxPeakWidth = Inf, DoubleSided = TRUE))
}

#unlist all the datasets
X <- bind_rows(X, .id = "country")
tsDS <- bind_rows(tsDS, .id = "country")
deriv.pred <- bind_rows(deriv.pred, .id = "country")
deriv2.pred <- bind_rows(deriv2.pred, .id = "country")
rsv_onset2 <- bind_rows(rsv_onset2, .id = "country") %>% dplyr::filter(pks > 0)

#only select peak values on second derivative where the positive derivative is positive
rsv_onset2 <- 
  left_join(rsv_onset2, left_join(deriv.pred, deriv2.pred) %>% dplyr::rename("loc" = "seqwk")) %>%
  dplyr::filter(deriv >0)
  
#====================================================================
#====================================================================

#combined dataset
rsv_onset1 <- 
  left_join(deriv2.pred, tsDS) %>%
  left_join(deriv.pred) %>%
  left_join(X) %>%
  select(country, seqwk, cases, fitcases, deriv, deriv2) %>%
  mutate(year = ifelse((seqwk > 0L & seqwk <= 52L), "2017",
                       ifelse((seqwk > 52L & seqwk <= 104), "2018",
                              ifelse((seqwk > 104L & seqwk <= 156L), "2019",
                                     ifelse((seqwk > 156L & seqwk <= 208L), "2020",
                                            ifelse((seqwk > 208L & seqwk <= 260L), "2021",
                                                   ifelse((seqwk > 260L & seqwk <= 312L), "2022",
                                                          ifelse((seqwk > 312L & seqwk <= 364L), "2023", NA_character_))))))))

#generate a dataset for location of maximum growth
rsv_onset2 <-
  rsv_onset2 %>% 
  select(country, baseline, pks, loc, parabol.x.from, parabol.x.to) %>% 
  rename(locL = parabol.x.from, locU = parabol.x.to)

#combine growth 1 and growth 2 for plotting
rsv_onset1 <- rsv_onset1 %>% left_join(rsv_onset2, by = c("country", "seqwk" = "loc"))

#====================================================================

#summaries the growth values for comparison before and after COVID-19
# rsv_onset2 <-
#   rsv_onset2 %>% 
#   dplyr::mutate(difloc = ifelse((loc > 0L & loc <= 52L), loc,
#                                 ifelse((loc > 52L & loc <= 104), subtract(loc, 52),
#                                        ifelse((loc > 104L & loc <= 156L), subtract(loc, 104),
#                                               ifelse((loc > 156L & loc <= 208L), subtract(loc, 156),
#                                                      ifelse((loc > 208L & loc <= 260L), subtract(loc, 208),
#                                                             ifelse((loc > 260L & loc <= 312L), subtract(loc, 260),
#                                                                    ifelse((loc > 312L & loc <= 364L), subtract(loc, 312), NA_integer_))))))),
#                 diflocL = ifelse((locL > 0L & locL <= 52L), locL,
#                                  ifelse((locL > 52L & locL <= 104), subtract(locL, 52),
#                                         ifelse((locL > 104L & locL <= 156L), subtract(locL, 104),
#                                                ifelse((locL > 156L & locL <= 208L), subtract(locL, 156),
#                                                       ifelse((locL > 208L & locL <= 260L), subtract(locL, 208),
#                                                              ifelse((locL > 260L & locL <= 312L), subtract(locL, 260),
#                                                                     ifelse((locL > 312L & locL <= 364L), subtract(locL, 312), NA_integer_))))))),
#                 diflocU = ifelse((locU > 0L & locU <= 52L), locU,
#                                  ifelse((locU > 52L & locU <= 104), subtract(locU, 52),
#                                         ifelse((locU > 104L & locU <= 156L), subtract(locU, 104),
#                                                ifelse((locU > 156L & locU <= 208L), subtract(locU, 156),
#                                                       ifelse((locU > 208L & locU <= 260L), subtract(locU, 208),
#                                                              ifelse((locU > 260L & locU <= 312L), subtract(locU, 260),
#                                                                     ifelse((locU > 312L & locU <= 364L), subtract(locU, 312), NA_integer_))))))),
#                 year = ifelse((loc > 0L & loc <= 52L), "2017",
#                               ifelse((loc > 52L & loc <= 104), "2018",
#                                      ifelse((loc > 104L & loc <= 156L), "2019",
#                                             ifelse((loc > 156L & loc <= 208L), "2020",
#                                                    ifelse((loc > 208L & loc <= 260L), "2021",
#                                                           ifelse((loc > 260L & loc <= 312L), "2022",
#                                                                  ifelse((loc > 312L & loc <= 364L), "2023", NA_character_))))))))
# 
# #====================================================================
# 
# #summarise by precovid vs 2021/22 and 2022/23 seasons
# rsv_onset2 <-
#   rsv_onset2 %>%
#   dplyr::select(country, year, pks) %>%
#   dplyr::mutate(covper = ifelse(year == "2023", "y2023",
#                                 ifelse(year == "2022", "y2022",
#                                        ifelse(year == "2021", "y2021",
#                                               ifelse(year == "2017" | year == "2018" | year == "2019", "precov", NA_character_))))) %>% 
#   dplyr::filter(!is.na(covper), year != "y2023") %>%
#   dplyr::select(everything(), -year) %>%
#   
#   dplyr::mutate(covper2 = ifelse(covper == "precov", 1, seq(1:n()))) %>%
#   dplyr::group_by(country, covper, covper2) %>%
#   dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   
#   dplyr::mutate(covper2 = ifelse(covper == "y2021", 1, seq(1:n()))) %>%
#   dplyr::group_by(country, covper, covper2) %>%
#   dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   
#   dplyr::mutate(covper2 = ifelse(covper == "y2022", 1, seq(1:n()))) %>%
#   dplyr::group_by(country, covper, covper2) %>%
#   dplyr::summarise(pks = mean(pks, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   
#   dplyr::left_join(climate %>% select(country, clim_zone)) %>%
#   dplyr::left_join(rsv_all %>% select(country, hemi, region) %>% distinct(.keep_all = TRUE)) %>%
#   dplyr::select(everything(), -covper2)

#only consider countries with all the data e.g., preCovid, 2021/22, and 2022/23
# rsv_growth2 <- 
#   rsv_growth2 %>% 
#   dplyr::left_join(
#     rsv_growth2 %>%
#       dplyr::group_by(country) %>%
#       tally()) %>%
#   dplyr::ungroup() %>% 
#   dplyr::filter(n == 3)

#rm(list = grep("rsv_all|climate|rsv_onset|rsv_peak|rsv_growth", ls(), value = TRUE, invert = TRUE))





#ggplot plotting of time series and peak values
E <-
  ggplot() +
  geom_point(data = rsv_onset1, aes(x = seqwk, y = cases), size = 2, color = "gray60", shape = 1, stroke = 1) + #original data
  geom_line(data = rsv_onset1, aes(x = seqwk, y = fitcases), size = 0.8) + # simulated GAM fitted model
  geom_line(data = rsv_onset1, aes(x = seqwk, y = deriv2/0.2), size = 0.8, color = "darkred") + # differentiated fitted curve
  geom_line(data = rsv_onset1, aes(x = seqwk, y = deriv/0.1), size = 0.8, color = "darkblue") + # differentiated fitted curve
  scale_y_continuous("Repoted and fitted weekly RSV cases", sec.axis = sec_axis(~. *0.2, name = "Estimated growth rate")) + 
  geom_point(data = rsv_onset1, aes(x = seqwk, y = pks/0.2), color = "darkred", shape = 4, size = 1,  stroke = 2) + #timing of peak and peak value
  geom_errorbarh(data = rsv_onset1, aes(y = pks, xmin = locL, xmax = locU), height = 0, size = 1, color = "darkred") +
  facet_wrap(.~ country, scale = "free_y", ncol=4) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  geom_vline(xintercept = c(52, 104, 156, 208, 260, 312), color = "blue", linetype = "dotted", cex = 0.6, alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 325, 52), limits = c(0, 325)) +
  theme(axis.text.y.right = element_text(color = "darkred"), axis.ticks.y.right = element_line(color = "darkred"), axis.title.y.right = element_text(color = "darkred")) +
  labs(x = "Reporting week (2017-2023) ", y = "", title ="") +
  theme(legend.position = "bottom", legend.title = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "fig2_onsetSupp.png"),
       plot = (E),
       width = 20, height = 22, unit="in", dpi = 300)

