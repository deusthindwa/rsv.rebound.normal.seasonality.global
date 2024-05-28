#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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
    Gmodels[[i]] <- gam(cases ~ s(x = time, bs = "cr", k = j),
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
  conv <- 2*pi/52 #convert to radians over 52 weeks
  
  #calculate circular mean for pre-Covid period (2017, 2018, 2019)
  onset2 <-
    onset2 %>%
    dplyr::select(country, wave, loc) %>%
    dplyr::group_by(country, wave) %>%
    dplyr::mutate(loc = loc*conv) %>% #52 weeks per year (circular)
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = wave, values_from = loc) %>%
    dplyr::select(everything(), -row) %>%
    dplyr::filter(!is.na(wave1), !is.na(wave2)) %>%
    data_frame() %>%
    ungroup()
    # dplyr::mutate(wave3 = if_else(country == "Netherlands" & is.na(wave3), 45, wave3),
    #               precov = if_else(country == "South Africa" & precov == 52, 1, precov)) %>%
    # 
    # dplyr::mutate(precov = circular(precov, units = "degrees", template = "geographics", modulo = "pi"),
    #               wave1 =  circular(wave1, units = "degrees", template = "geographics", modulo = "pi"),
    #               wave2 = circular(wave2, units = "degrees", template = "geographics", modulo = "pi"),
    #               wave3 = circular(wave3, units = "degrees", template = "geographics", modulo = "pi"))
  
  onset[[j]] = onset2
}

#delete empty country data frames from list 'onset'
onset <- onset[lengths(onset) > 0]
onset <-
  dplyr::bind_rows(onset[[1]]) %>% dplyr::rename("precova" = "precov", "wave1a" = "wave1", "wave2a" = "wave2", "wave3a" = "wave3") %>%
  dplyr::left_join(dplyr::bind_rows(onset[[2]]) %>% dplyr::rename("precovb" = "precov", "wave1b" = "wave1", "wave2b" = "wave2", "wave3b" = "wave3")) %>%
  dplyr::left_join(dplyr::bind_rows(onset[[3]]) %>% dplyr::rename("precovc" = "precov", "wave1c" = "wave1", "wave2c" = "wave2", "wave3c" = "wave3"))

onset <-
bind_rows(
  onset %>% dplyr::select(country, precova, precovb, precovc) %>% dplyr::rename("k25"="precova", "k35"="precovb", "k45"="precovc") %>% dplyr::mutate(phase ="Pre-COVID-19"),
  onset %>% dplyr::select(country, wave1a, wave1b, wave1c) %>% dplyr::rename("k25"="wave1a", "k35"="wave1b", "k45"="wave1c") %>% dplyr::mutate(phase ="RSV first wave"),
  onset %>% dplyr::select(country, wave2a, wave2b, wave2c) %>% dplyr::rename("k25"="wave2a", "k35"="wave2b", "k45"="wave2c") %>% dplyr::mutate(phase ="RSV second wave"),
  onset %>% dplyr::select(country, wave3a, wave3b, wave3c) %>% dplyr::rename("k25"="wave3a", "k35"="wave3b", "k45"="wave3c") %>% dplyr::mutate(phase ="RSV third wave") %>% dplyr::filter(!is.na(k25))
)

#onset timing by overall status (circular correlation coefficient)
  onset_overall <- list()
  for (j in c("Pre-COVID-19", "RSV first wave", "RSV second wave", "RSV third wave")) {
    Y <-
      onset %>%
      dplyr::filter(phase == j) %>%
      dplyr::mutate(w1corr = round((circular::cor.circular(k25, k45))[1], digits = 2),
                    w2corr = round((circular::cor.circular(k35, k45))[1], digits = 2),
                    w1L = round(boot.ci(boot(as.data.frame(onset %>% dplyr::select(k25, k45, phase)) %>% dplyr::filter(phase == j), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                    w1U = round(boot.ci(boot(as.data.frame(onset %>% dplyr::select(k25, k45, phase)) %>% dplyr::filter(phase == j), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2),
                    w2L = round(boot.ci(boot(as.data.frame(onset %>% dplyr::select(k35, k45, phase)) %>% dplyr::filter(phase == j), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[2], digits = 2),
                    w2U = round(boot.ci(boot(as.data.frame(onset %>% dplyr::select(k35, k45, phase)) %>% dplyr::filter(phase == j), function(i,d) circular::cor.circular(i[d,1], i[d,2]), R=1000), type = "norm")$normal[3], digits = 2)) %>%
      
      dplyr::mutate(w1U = if_else(w1U>1,1.0,w1U), w2U = if_else(w2U>1,1.0,w2U),
                    w1L = if_else(w1L< -1,-1.0,w1L), w2L = if_else(w2L< -1,-1.0,w2L))
    
    onset_overall[[j]] <- Y
  }

O1 <-
  dplyr::bind_rows(dplyr::bind_rows(onset_overall, .id = "phase")) %>%
  dplyr::mutate(k25 = k25*52/(2*pi), k35 = k35*52/(2*pi), k45 = k45*52/(2*pi)) %>%
  ggplot(aes(x = k25, y = k45, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w1corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w1L,",",w1U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(phase ~ .) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  labs(x = "RSV onset timing (P-spline, knots=25)", y = "RSV onset timing (P-spline, knots=45)", title = "") +
  theme(legend.position = "none", legend.title = element_blank()) +
  theme(strip.text.y = element_blank()) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

O2 <-
  dplyr::bind_rows(onset_overall) %>%
  dplyr::mutate(k25 = k25*52/(2*pi), k35 = k35*52/(2*pi), k45 = k45*52/(2*pi)) %>%
  ggplot(aes(x = k35, y = k45, color = country), position = position_dodge(width = 0.5)) +
  geom_point(size = 4, position = position_dodge(width = 0.5), stroke = 2, shape = 4) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.5) +
  geom_text(aes(x = 42, y = 6, label = paste0("c = ", w2corr)), color = "black", size = 6, fontface = "bold") +
  geom_text(aes(x = 42, y = 2, label = paste0("(", w2L,",",w2U,")")), color = "black", size = 6, fontface = "bold") +
  facet_grid(phase ~ .) +
  scale_x_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  scale_y_continuous(breaks = seq(1, 52, 5), limits = c(0,53)) +
  theme_bw(base_size = 16, base_family = 'Lato') +
  labs(x = "RSV onset timing (P-spline, knots=35)", y = "", title = "") +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme(axis.text.y = element_blank(), strip.text.x = element_text(size = 20)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 2)) 

ggsave(here("output", "sfig13_modelCk.png"),
       plot = (O1 | O2),
       width = 16, height = 14, unit = "in", dpi = 300)

#delete unnecessary files
#rm(list = grep("rsv_all|rsv_dtw|climate|stringency|onset1|onset2|peak1|peak2|growth1|intense1|intense2|growth2|O|P|G|I", ls(), value = TRUE, invert = TRUE))
