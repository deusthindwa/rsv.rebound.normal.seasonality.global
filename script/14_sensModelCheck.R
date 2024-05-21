#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#COMPUTE AIKAKE OR BAYESIAN INFORMATION CRITERION SCORE
#====================================================================

# #split the dataset by country to form list of datasets
# X <- 
#   rsv_all %>%  
#   dplyr::filter(country %in% c("Argentina", "Australia", "Colombia", "Costa Rica", "India", "Japan", "Paraguay", 
#                                "Peru", "South Africa", "Brazil", "Canada", "Denmark", "France", "Germany", 
#                                "Hungary", "Iceland", "Ireland", "Mexico", "Mongolia", "Netherlands", "Northern Ireland", 
#                                "Oman", "Portugal", "Qatar", "Scotland", "Spain", "Sweden", "United States")) %>%
#   dplyr::arrange(country, yr, wk, cases) %>%
#   dplyr::group_by(country) %>%
#   dplyr::mutate(seqwk = seq.int(from = week(date), by = 1, length.out = n()),
#                 time = seq.int(from = 1, by = 1, length.out = n())) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(date, time, seqwk, cases, country) %>%
#   base::split(list(.$country))
# 
# #delete empty country data frames from list X
# X <- X[unlist(lapply(X, nrow) != 0)]
# 
# #create empty dataset list to store GAM models for each country
# k = 1
# Gmodels1 <- list()
# Gmodels2 <- list()
# Gmodels3 <- list()
# Gmodels4 <- list()
# ModelCheck <- data.frame(country=rep(NA, 728), nknots=rep(NA, 728), aic1=rep(NA, 728), aic2=rep(NA, 728), aic3=rep(NA, 728), aic4=rep(NA, 728))
# 
# #check robustness of GAM models e.g., evaluate number of knots
# #run the GAM models via random effects maximum likelihood (REML)
# for (i in names(X)) {
#   for (j in 15:45) {#explored number of knots
#     Gmodels1[[i]] <- gam(cases ~ s(x = time, bs = "cr", k = j), family = poisson, method = "REML", control = list(maxit = 100000), data = X[[i]]) #intergrated square second derivative cubic spline penalty
#     Gmodels2[[i]] <- gam(cases ~ s(x = time, bs = "bs", k = j), family = poisson, method = "REML", control = list(maxit = 100000), data = X[[i]]) #integrated squared derivative penalties
#     Gmodels3[[i]] <- gam(cases ~ s(x = time, bs = "ps", k = j), family = poisson, method = "REML", control = list(maxit = 100000), data = X[[i]]) #discrete penalty on basis coefficients, no exact interpretation of function shape in the way derivative penalties do
#     Gmodels4[[i]] <- gam(cases ~ s(x = time, bs = "ad", k = j), family = poisson, method = "REML", control = list(maxit = 100000), data = X[[i]]) #smoothing penalty is constructed based on the neighbourhood structure of the geographic units
#     ModelCheck[k,] <- c(i, j, round(stats::AIC(Gmodels1[[i]]), digits = 0), round(stats::AIC(Gmodels2[[i]]), digits = 0), round(stats::AIC(Gmodels3[[i]]), digits = 0), round(stats::AIC(Gmodels4[[i]]), digits = 0))
#     k = k +1
#   }
# }
#   
# #plot number of knots and AIC/BIC scores
# A <-
#   bind_rows(ModelCheck %>% dplyr::mutate(score = as.numeric(aic1), cat = "cr") %>% dplyr::select(everything(), -aic1, -aic2, -aic3, -aic4),
#             ModelCheck %>% dplyr::mutate(score = as.numeric(aic2), cat = "bs") %>% dplyr::select(everything(), -aic1, -aic2, -aic3, -aic4),
#             ModelCheck %>% dplyr::mutate(score = as.numeric(aic3), cat = "ps") %>% dplyr::select(everything(), -aic1, -aic2, -aic3, -aic4),
#             ModelCheck %>% dplyr::mutate(score = as.numeric(aic4), cat = "ad") %>% dplyr::select(everything(), -aic1, -aic2, -aic3, -aic4)) %>%
#   left_join(rsv_all %>% dplyr::select(country, hemi) %>% distinct()) %>%
#   dplyr::mutate(hemix = if_else(country %in% c("Colombia", "Costa Rica", "India", "Canada", "Denmark", "France", "Germany", "Hungary", "Japan", "Iceland", "Ireland"), "Northern Hemisphere",
#                                if_else(hemi == "Northern hemisphere", "Northern Hemisphere ",
#                                        if_else(hemi == "Southern hemisphere", "Southern Hemisphere", NA_character_))),
#                 nknots = as.integer(nknots)) %>%
#   ggplot() +
#   geom_line(aes(x = nknots, y = score, color = country, group = country), size = 1.5) +
#   scale_x_continuous(breaks = seq(15, 45, 3)) +
#   theme_bw(base_size = 14, base_family = "Lato") + 
#   facet_grid(hemix ~ cat, scales = "free_y") +
#   geom_vline(xintercept = 35, linetype = "dotted", size = 1) +
#   labs(title = "", x = "Number of knots", y = "Akaike information Criterion (AIC) score") +
#   theme(strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), strip.background = element_rect(fill = "gray80")) +
#   theme(legend.text = element_text(size = 12), legend.position = "right", legend.title = element_text(size = 12)) +
#   theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))
# 
# ggsave(here("output", "sfig12_modelCheck.png"),
#        plot = A,
#        width = 20, height = 15, unit="in", dpi = 300)

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

#====================================================================
#COMPUTE ONSETS GIVEN DIFFERENT NUMBER OF KNOTS ON THE MODEL
#====================================================================

#create empty list to hold final knot-specific onset dataset
onset <- list()

for (j in c(25, 35, 45)) { #go through different number of knots 25, 35, 45
  
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

#create empty list to store GAM models, derivatives & fitted time series data for each country
Gmodels <- list()
tsDS <- list()
deriv1 <- list()
deriv2 <- list()
onset2 <- list()

#functions to numerically calculate the second derivative
deriv <- function(x, y) diff(y) / diff(x) 
middle_pts <- function(x) x[-1] - diff(x) / 2 

#run the GAM models via random effects maximum likelihood (REML)
for (i in names(X)) {
  Gmodels[[i]] <- gam(cases ~ s(x = time, bs = "ps", k = j),
                      family = poisson,
                      method = "REML",
                      control = list(maxit = 100000),
                      data = X[[i]])
}

#iterate for each country, extract fitted values, compute 1st and 2nd derivatives
for (i in names(X)){
  tsDS[[i]] = data.frame(fitcases = Gmodels[[i]]$fitted.values) %>% #fitted values
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n()))
  
  deriv1[[i]] = data.frame(fderiv = diff(tsDS[[i]]$fitcases)/diff(tsDS[[i]]$seqwk)) %>% 
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n())) #first derivative
  
  deriv2[[i]] = data.frame(sderiv = deriv(middle_pts(tsDS[[i]]$seqwk), deriv(tsDS[[i]]$seqwk, tsDS[[i]]$fitcases))) %>%
    mutate(seqwk = seq.int(from = X[[i]]$seqwk[1], by = 1, length.out = n()),
           date = seq.int(from = X[[i]]$date[1], by = 7, length.out = n())) #second derivative
}

#unlist all the datasets and keep location of maximim second derivative
X <- dplyr::bind_rows(X, .id = "country")
tsDS <- dplyr::bind_rows(tsDS, .id = "country")
deriv1 <- dplyr::bind_rows(deriv1, .id = "country")
deriv2 <- dplyr::bind_rows(deriv2, .id = "country")
onset2 <- 
  deriv2 %>% 
  dplyr::left_join(deriv1) %>% 
  dplyr::filter(fderiv > 0) %>%
  dplyr::select(everything(), -fderiv) %>%
  dplyr::mutate(intcat = factor(cumsum(c(1, abs(seqwk[-length(seqwk)] - seqwk[-1]) > 1))),
                year = factor(year(date))) %>%
  dplyr::filter(year != "2023") %>%
  dplyr::group_by(intcat) %>%
  dplyr::mutate(counter = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(counter != 1) %>%
  
  dplyr::group_by(country, intcat, year) %>%
  dplyr::mutate(max_sderiv = max(sderiv, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(sderiv == max_sderiv) %>%
  
  #fixing issues per observation to ensure accuracy of onset timing
  dplyr::mutate(year = if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa") & year == "2020", "No_Oubreak", year)) %>%
  dplyr::filter(year != "No_Oubreak") %>%
  
  dplyr::mutate(year = if_else(country == "Iceland" & seqwk == 215, "2020", year)) %>%
  dplyr::mutate(year = if_else(country == "Sweden" & seqwk == 106, "2018", year)) %>%
  dplyr::mutate(year = if_else(country == "Netherlands" & year == 2021, "2020", if_else(country == "Netherlands" & year == 2022, "2021", year))) %>%
  dplyr::mutate(year = if_else(country == "Netherlands" & seqwk == 306, "2022", year)) %>%
  dplyr::mutate(sderiv = if_else(country == "South Africa" & seqwk == 261, 0, sderiv)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 53, "2021", year)) %>%
  dplyr::mutate(year = if_else(country == "Brazil" & seqwk == 300, "2021", year)) %>%
  dplyr::mutate(sderiv = if_else(country == "India" & seqwk == 311, 0.0001, sderiv)) %>%
  
  dplyr::group_by(country, year) %>%
  dplyr::filter(max_sderiv == max(sderiv)) %>%
  dplyr::ungroup()

#compute circular timing around 52 weeks
onset2 <-
  onset2 %>% 
  dplyr::mutate(loc = week(date),
                loc = if_else(loc == 53, 52, loc),
                seqwk2 = seqwk) %>% 
  dplyr::select(country, year, seqwk, seqwk2, loc, date)

#create different RSV waves in different countries
onset2 <-  
  onset2 %>% 
  dplyr::mutate(wave = if_else(year == "2017" | year == "2018" | year == "2019", "precov", year)) %>%
  
  dplyr::mutate(wave = if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2020", "wave1",
                               if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2021", "wave2", 
                                       if_else(country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2022", "wave3", wave)))) %>%
  
  dplyr::mutate(wave = if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2021", "wave1", 
                               if_else(!country %in% c("Australia", "Brazil", "Colombia", "France", "Iceland", "South Africa", "Netherlands") & year == "2022", "wave2", wave)))

onset2x <- onset2 #onset2x dataset used for regression

#calculate circular mean for pre-Covid period (2017, 2018, 2019)
onset2 <-
  onset2 %>%
  dplyr::select(country, wave, loc) %>%
  dplyr::group_by(country, wave) %>%
  #dplyr::mutate(loc = (circular::mean.circular((2*pi/52)*(loc-1))/(2*pi/52) + 52) %% 52) %>% #52 weeks per year (circular)
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = wave, values_from = loc) %>%
  dplyr::select(everything(), -row) %>%
  dplyr::filter(!is.na(wave1), !is.na(wave2)) %>%
  data_frame() %>%
  ungroup() %>%
  dplyr::mutate(wave3 = if_else(country == "Netherlands" & is.na(wave3), 45, wave3),
                precov = if_else(country == "South Africa" & precov == 52, 1, precov)) %>%
  
  dplyr::mutate(precov = circular(precov, units = "degrees", template = "geographics", modulo = "pi"),
                wave1 =  circular(wave1, units = "degrees", template = "geographics", modulo = "pi"),
                wave2 = circular(wave2, units = "degrees", template = "geographics", modulo = "pi"),
                wave3 = circular(wave3, units = "degrees", template = "geographics", modulo = "pi")) 

#bootstraping dataset
onsetb <- 
  onset2 %>% 
  dplyr::left_join(rsv_all %>% select(hemi, country) %>% distinct()) %>%
  dplyr::left_join(climate %>% select(country, clim) %>% distinct())

#1)onset timing by overall status (circular correlation coefficient)
onset_overall <-
  onset2 %>%
  dplyr::mutate(w1corr = round((circular::cor.circular(precov, wave1))[1], digits = 2),
                w2corr = round((circular::cor.circular(precov, wave2))[1], digits = 2),
                w3corr = round((circular::cor.circular(precov, wave3))[1], digits = 2),
                
                w1L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w1U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave1)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w2L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w2U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave2)), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                w3L = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                w3U = round(boot.ci(boot(as.data.frame(onsetb %>% dplyr::select(precov, wave3) %>% dplyr::filter(!is.na(wave3))), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                cat = "Overall") %>%
  dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U), w3U = if_else(w3U>1,1.0,w3U),
                w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L), w3L = if_else(w3L< -1,-1.0,w3L))

onset[[j]] = onset_overall

}

#delete empty country data frames from list X
onset <- onset[lengths(onset) > 0]
onset_overall <-
  bind_rows(
    dplyr::bind_rows(onset[[1]]) %>% dplyr::mutate(knot = "25 knots"),
    dplyr::bind_rows(onset[[2]]) %>% dplyr::mutate(knot = "35 knots"),
    dplyr::bind_rows(onset[[3]]) %>% dplyr::mutate(knot = "45 knots"))

O1 <-
  dplyr::bind_rows(onset_overall) %>%
  ggplot(aes(x = precov, y = wave1, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(. ~ knot) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "First wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O2 <-
  dplyr::bind_rows(onset_overall) %>%
  ggplot(aes(x = precov, y = wave2, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(. ~ knot) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Second wave RSV onset timing", title = "") +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O3 <-
  dplyr::bind_rows(onset_overall) %>%
  ggplot(aes(x = precov, y = wave3, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w3corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w3L,",",w3U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(. ~ knot) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  theme_bw(base_size = 14, base_family = 'Lato') +
  labs(x = "", y = "Third wave RSV onset timing", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig12_modelCheck.png"),
       plot = ((O1 + labs(title = "SENSITIVITY ANALYSIS ON RSV ONSET TIMING"))/O2/(O3 + labs(x = "PreCOVID (2017-19) mean onset timing"))),
       width = 16, height = 14, unit = "in", dpi = 300)


#delete unnecessary files
#rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))



