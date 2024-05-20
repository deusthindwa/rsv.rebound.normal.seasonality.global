#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#COMPUTE AIKAKE OR BAYESIAN INFORMATION CRITERION SCORE
#====================================================================

#split the dataset by country to form list of datasets
X <- 
  rsv_all %>%  
  dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
                               "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
                               "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
                               "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
  dplyr::arrange(country, yr, wk, cases) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(seqwk = seq.int(from = week(date), by = 1, length.out = n()),
                time = seq.int(from = 1, by = 1, length.out = n())) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, time, seqwk, cases, country) %>%
  base::split(list(.$country))

#delete empty country data frames from list X
X <- X[unlist(lapply(X, nrow) != 0)]

#create empty dataset list to store GAM models for each country
Gmodels <- list()
ModelCheck <- data.frame(country = rep(NA, 728), nknots = rep(NA, 728), aic = rep(NA, 728), bic = rep(NA, 728))
k = 1

#check robustness of GAM models e.g., evaluate number of knots
#run the GAM models via random effects maximum likelihood (REML)
for (i in names(X)) {
  for (j in 15:45) {#explored number of knots
    Gmodels[[i]] <- gam(cases ~ s(x = time, bs = "ps", k = j),
                        family = poisson,
                        method = "REML",
                        control = list(maxit = 100000),
                        data = X[[i]])
    ModelCheck[k,] <- c(i, j, round(stats::AIC(Gmodels[[i]]), digits = 0), round(stats::BIC(Gmodels[[i]]), digits = 0))
    k = k +1
  }
}
  
#plot number of knots and AIC/BIC scores
A <-
  bind_rows(ModelCheck %>% dplyr::mutate(score = as.numeric(aic), cat = "AIC") %>% dplyr::select(everything(), -bic, -aic),
            ModelCheck %>% dplyr::mutate(score = as.numeric(bic), cat = "BIC") %>% dplyr::select(everything(), -aic, -bic)) %>%
  left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
  dplyr::mutate(hemix = if_else(country %in% c("Colombia", "Costa Rica", "India", "Canada", "Denmark", "France", "Germany", "Hungary", "Japan", "Iceland", "Ireland"), "Northern Hemisphere",
                               if_else(hemi == "Northern hemisphere", "Northern Hemisphere ",
                                       if_else(hemi == "Southern hemisphere", "Southern Hemisphere", NA_character_))),
                nknots = as.integer(nknots)) %>%
  ggplot() +
  geom_line(aes(x = nknots, y = score, color = country, group = country), size = 1.5) +
  scale_x_continuous(breaks = seq(15, 45, 3)) +
  theme_bw(base_size = 14, base_family = "Lato") + 
  facet_grid(hemix ~ cat, scales = "free_y") +
  geom_vline(xintercept = 35, linetype = "dotted", size = 1) +
  labs(title = "", x = "Number of knots", y = "Information Criterion score") +
  theme(strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), strip.background = element_rect(fill = "gray80")) +
  theme(legend.text = element_text(size = 12), legend.position = "right", legend.title = element_text(size = 12)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))

ggsave(here("output", "sfig12_modelCheck.png"),
       plot = A,
       width = 20, height = 15, unit="in", dpi = 300)


# #plot optimal number of knots and minimum AIC/BIC scores
# ModelCheck %>%
#   dplyr::group_by(country) %>%
#   dplyr::summarise(nknots = nknots[which.min(as.numeric(aic))], 
#                    aic = min(as.numeric(aic)), 
#                    bic = min(as.numeric(bic))) %>%
#   dplyr::ungroup() %>%
#   
#   ggplot() +
#   geom_point(aes(x = nknots, y = aic, color = country), shape = 4, stroke = 1) +
#   geom_point(aes(x = nknots, y = bic, color = country), shape = 1, stroke = 1) +
#   scale_y_continuous(limit = c(1000, 5000), breaks = seq(1000, 5000, 500)) +
#   geom_text(aes(x = nknots, y = aic, label = str_sub(country, 1,2)), size = 2, angle = "45", vjust = -0.5, hjust = 1.5, fontface = "bold", position = position_dodge(width = 1)) +
#   theme_bw() +
#   labs(title = "", x = "optimal number of knots", y = "minimum AIC") +
#   theme(legend.position = "none")
