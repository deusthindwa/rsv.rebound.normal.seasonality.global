#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# REGRESSION ANALYSIS OF ONSET 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + pop_dens2", "clim_zone2 + pop_dens2", "strin_indx2 + pop_dens2", "med_age2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + hemi2 + pop_dens2", "strin_indx2 + hemi2 + pop_dens2", "med_age2 + hemi2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + med_age2 + hemi2 + pop_dens2", "strin_indx2 + med_age2 + hemi2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

Modonset1 = coxph(Surv(time, event) ~ clim_zone2 + med_age2 + hemi2 + pop_dens2, data = DSonset1, control = coxph.control(iter.max = 1000))
tidy(Modonset1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X1 <- tidy(Modonset1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Onset timing 2021", yr = 2021) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modonset1 = coxph(Surv(time, event) ~ clim_zone2 + med_age2 + hemi2 + pop_dens2 + strin_indx2, data = DSonset1, control = coxph.control(iter.max = 1000))
tidy(Modonset1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF ONSET 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + med_age2", "clim_zone2 + med_age2", "strin_indx2 + med_age2", "pop_dens2 + med_age2", "out_seas212 + med_age2", "intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + intens20212 + med_age2", "clim_zone2 + intens20212 + med_age2", "strin_indx2 + intens20212 + med_age2", "pop_dens2 + intens20212 + med_age2", "out_seas212 + intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + out_seas212 + intens20212 + med_age2", "clim_zone2 + out_seas212 + intens20212 + med_age2", "strin_indx2 + out_seas212 + intens20212 + med_age2", "pop_dens2 + out_seas212 + intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + pop_dens2 + out_seas212 + intens20212 + med_age2", "clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2", "strin_indx2 + pop_dens2 + out_seas212 + intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2", "strin_indx2 + clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("strin_indx2 + hemi2 + clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

Modonset2 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2, data = DSonset2, control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X2 <-tidy(Modonset2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Onset timing 2022", yr = 2022) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modonset2 = coxph(Surv(time, event) ~ hemi2 + clim_zone2 + pop_dens2 + out_seas212 + intens20212 + med_age2 + strin_indx2, data = DSonset2, control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF PEAK 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + med_age2", "clim_zone2 + med_age2", "strin_indx2 + med_age2", "pop_dens2 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + pop_dens2 + med_age2", "clim_zone2 + pop_dens2 + med_age2", "strin_indx2 + pop_dens2 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + hemi2 + pop_dens2 + med_age2", "strin_indx2 + hemi2 + pop_dens2 + med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

Modpeak1 = coxph(Surv(time, event) ~ clim_zone2 + hemi2 + pop_dens2 + med_age2, data = DSpeak1, control = coxph.control(iter.max = 1000))
tidy(Modpeak1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X3 <- tidy(Modpeak1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Peak timing 2021", yr = 2021) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modpeak1 = coxph(Surv(time, event) ~ clim_zone2 + hemi2 + pop_dens2 + med_age2 + strin_indx2, data = DSpeak1, control = coxph.control(iter.max = 1000))
tidy(Modpeak1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF PEAK 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "out_seas212", "strin_indx2", "pop_dens2", "med_age2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + out_seas212", "clim_zone2 + out_seas212", "strin_indx2 + out_seas212", "pop_dens2 + out_seas212", "med_age2 + out_seas212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + hemi2 + out_seas212", "strin_indx2 + hemi2 + out_seas212", "pop_dens2 + hemi2 + out_seas212", "med_age2 + hemi2 + out_seas212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + med_age2 + hemi2 + out_seas212", "strin_indx2 + med_age2 + hemi2 + out_seas212", "pop_dens2 + med_age2 + hemi2 + out_seas212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

Modpeak2 = coxph(Surv(time, event) ~ med_age2 + hemi2 + out_seas212, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X4 <- tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Peak timing 2022", yr = 2022) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modpeak2 = coxph(Surv(time, event) ~ med_age2 + hemi2 + out_seas212 + clim_zone2, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modpeak2 = coxph(Surv(time, event) ~ med_age2 + hemi2 + out_seas212 + strin_indx2, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modpeak2 = coxph(Surv(time, event) ~ med_age2 + hemi2 + out_seas212 + pop_dens2, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF GROWTH 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth1)))
}

for (i in c("hemi2 + out_seas212", "clim_zone2 + out_seas212", "strin_indx2 + out_seas212", "pop_dens2 + out_seas212", "med_age2 + out_seas212")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth1)))
}

for (i in c("clim_zone2 + hemi2 + out_seas212", "strin_indx2 + hemi2 + out_seas212", "pop_dens2 + hemi2 + out_seas212", "med_age2 + hemi2 + out_seas212")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth1)))
}

Modgrowth1 = lm(pks2 ~ hemi2 + out_seas212, data = DSgrowth1)
tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X5 <- tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Growth rate 2021", yr = 2021) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modgrowth1 = lm(pks2 ~ hemi2 + out_seas212 + clim_zone2, data = DSgrowth1)
tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth1 = lm(pks2 ~ hemi2 + out_seas212 + strin_indx2, data = DSgrowth1)
tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth1 = lm(pks2 ~ hemi2 + out_seas212 + pop_dens2, data = DSgrowth1)
tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth1 = lm(pks2 ~ hemi2 + out_seas212 + med_age2, data = DSgrowth1)
tidy(Modgrowth1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF GROWTH 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth2)))
}

for (i in c("hemi2 + strin_indx2", "clim_zone2 + strin_indx2", "pop_dens2 + strin_indx2", "med_age2 + strin_indx2", "out_seas212 + strin_indx2", "intens20212 + strin_indx2")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth2)))
}

for (i in c("hemi2 + out_seas212 + strin_indx2", "clim_zone2 + out_seas212 + strin_indx2", "pop_dens2 + out_seas212 + strin_indx2", "med_age2 + out_seas212 + strin_indx2", "intens20212 + out_seas212 + strin_indx2")) {
  print(AIC(lm(as.formula(paste0("pks2 ~", i)), data = DSgrowth2)))
}

Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X6 <- tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Growth rate 2022", yr = 2022) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2 + hemi2, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2 + clim_zone2, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2 + pop_dens2, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2 + med_age2, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modgrowth2 = lm(pks ~ out_seas212 + strin_indx2 + intens20212, data = DSgrowth2)
tidy(Modgrowth2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF INTENSITY 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "pop_dens2", "med_age2", "strin_indx2", "out_seas212")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens1)))
}

for (i in c("hemi2 + out_seas212", "clim_zone2 + out_seas212", "pop_dens2 + out_seas212", "med_age2 + out_seas212", "strin_indx2 + out_seas212")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens1)))
}

for (i in c("hemi2 + strin_indx2 + out_seas212", "clim_zone2 + strin_indx2 + out_seas212", "pop_dens2 + strin_indx2 + out_seas212", "med_age2 + strin_indx2 + out_seas212")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens1)))
}

Modintensity1 = lm(intensity2 ~ strin_indx2 + out_seas212, data = DSintens1)
tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X7 <- tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Intensity 2021", yr = 2021) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modintensity1 = lm(intensity2 ~ strin_indx2 + out_seas212 + hemi2, data = DSintens1)
tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity1 = lm(intensity2 ~ strin_indx2 + out_seas212 + clim_zone2, data = DSintens1)
tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity1 = lm(intensity2 ~ strin_indx2 + out_seas212 + pop_dens2, data = DSintens1)
tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity1 = lm(intensity2 ~ strin_indx2 + out_seas212 + med_age2, data = DSintens1)
tidy(Modintensity1, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
# REGRESSION ANALYSIS OF INTENSITY 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "strin_indx2", "pop_dens2", "med_age2", "out_seas21", "intens20212", "growth20212")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2)))
}

for (i in c("hemi2 + clim_zone2", "strin_indx2 + clim_zone2", "pop_dens2 + clim_zone2", "med_age2 + clim_zone2", "out_seas21 + clim_zone2", "intens20212 + clim_zone2", "growth20212 + clim_zone2")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2)))
}

for (i in c("hemi2 + pop_dens2 + clim_zone2", "strin_indx2 + pop_dens2 + clim_zone2", "med_age2 + pop_dens2 + clim_zone2", "out_seas21 + pop_dens2 + clim_zone2", "intens20212 + pop_dens2 + clim_zone2", "growth20212 + pop_dens2 + clim_zone2")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2)))
}

for (i in c("strin_indx2 + hemi2 + pop_dens2 + clim_zone2", "med_age2 + hemi2 + pop_dens2 + clim_zone2", "out_seas21 + hemi2 + pop_dens2 + clim_zone2", "intens20212 + hemi2 + pop_dens2 + clim_zone2", "growth20212 + hemi2 + pop_dens2 + clim_zone2")) {
  print(AIC(lm(as.formula(paste0("intensity2 ~", i)), data = DSintens2)))
}

Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)
X8 <- tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::mutate(ds = "Intensity 2022", yr = 2022) %>% dplyr::filter(term != "(Intercept)")

#fit variables that were not statistically significant to main model
Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2 + strin_indx2, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2 + med_age2, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2 + out_seas21, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2 + intens20212, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

Modintensity2 = lm(intensity2 ~ hemi2 + pop_dens2 + clim_zone2 + growth20212, data = DSintens2)
tidy(Modintensity2, exponentiate = FALSE, conf.int = TRUE, conf.level = 0.95)

#================================================================
#================================================================

#combine all datasets
DSregressMv <- 
  dplyr::bind_rows(X1, X2, X3, X4, X5, X6, X7, X8) %>%
  dplyr::group_by(term) %>%
  dplyr::mutate(widthx = 0.1*n()) %>% #fix the problem of differing errorbar width due to different number of terms in the dataset
  dplyr::mutate(term = case_when(term == "clim_zone2Sub-tropical" ~ "Climate Zone \n Subtropical (vs temperate)",
                                 term == "clim_zone2Tropical" ~ "Climate Zone \n Tropical (vs temperate)",
                                 term == "growth20212" ~ "RSV growth in 2021",
                                 term == "growth20222" ~ "RSV growth in 2022",
                                 term == "hemi2Southern hemisphere" ~ "Hemisphere \n Southern (vs Northern)",
                                 term == "intens20212" ~ "RSV intensity in 2021",
                                 term == "med_age2" ~ "Population median age",
                                 term == "out_seas212yes" ~ "RSV out-of-season in 2021, yes",
                                 term == "pop_dens2" ~ "Population density",
                                 term == "region2Africa & SEA" ~ "Region \n Africa/SEA (vs Europe)",
                                 term == "region2Eastern Mediterranean" ~ "Region \n Eastern Mediterranean (vs Europe)",
                                 term == "region2North Americas" ~ "Region \n North Americas (vs Europe)",
                                 term == "region2South Americas" ~ "Region \n South Americas (vs Europe)",
                                 term == "region2Western Pacific" ~ "Region \n Western Pacific (vs Europe)",
                                 term == "strin_indx2" ~ "Contact stringency",
                                 TRUE ~ NA_character_))

A <-
  DSregressMv %>%
  ggplot(aes(y = reorder(term, ds), x = estimate, color = ds, width = widthx)) +
  geom_point(shape = 16, size = 4, stroke = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, size = 1.5, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", cex = 0.6, alpha = 0.8) +
  labs(title = "", x = "Log-scale estimates from univariate regression models with 95%CI", y = "") + 
  facet_grid(.~ ds, scales = "free_x") +
  theme_bw(base_size = 15, base_family = 'Lato') +
  guides(color = guide_legend(title = "")) +
  theme(legend.position = "none", strip.text.x = element_text(size = 16)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))  + 
  scale_color_manual(values=c("#F8766D", "#00BFC4", "#A9A9A9", "#36454F", "#7CAE00", "#C77CFF", "#FFC133", "#7D33FF"))

ggsave(here("output", "fig6_regressMultivariate.png"),
       plot = A,
       width = 21, height = 10, unit="in", dpi = 300)

#save the multivariate regression table
table2021 <- 
DSregressMv %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value, ds, yr) %>%
  dplyr::filter(yr == 2021) %>%
  tidyr::pivot_wider(names_from = ds, values_from = c(estimate, conf.low, conf.high, p.value)) %>%
  dplyr::select(1, 3,7,11, 4,8,12, 5,9,13, 6,10,14)
rio::export(table2021, here("output", "table2021_multivariate.xlsx"))

table2022 <- 
DSregressMv %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value, ds, yr) %>%
  dplyr::filter(yr == 2022) %>%
  tidyr::pivot_wider(names_from = ds, values_from = c(estimate, conf.low, conf.high, p.value)) %>%
  dplyr::select(1, 3,7,11, 4,8,12, 5,9,13, 6,10,14)
rio::export(table2022, here("output", "table2022_multivariate.xlsx"))

rm(X1, X2, X3, X4, X5, X6, X7, X8, A)
