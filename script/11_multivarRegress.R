#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#================================================================
# REGRESSION ANALYSIS OF ONSET 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "pop_dens2", "strin_indx2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

for (i in c("pop_dens2 + hemi2", "pop_dens2 + strin_indx2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

for (i in c("pop_dens2 + hemi2 + strin_indx2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset1, control = coxph.control(iter.max = 1000))))
}

Modonset1 = coxph(Surv(time, event) ~ pop_dens2 + hemi2, data = DSonset1, control = coxph.control(iter.max = 1000))
tidy(Modonset1, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

Modonset1 = coxph(Surv(time, event) ~ pop_dens2 + hemi2 + strin_indx2, data = DSonset1, control = coxph.control(iter.max = 1000))
tidy(Modonset1, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

#================================================================
# REGRESSION ANALYSIS OF ONSET 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "clim_zone2", "pop_dens2", "med_age2", "out_seas212", "intens20212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("med_age2 + hemi2", "med_age2 + clim_zone2", "med_age2 + pop_dens2", "med_age2 + out_seas212", "med_age2 + intens20212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("out_seas212 + med_age2 + hemi2", "out_seas212 + med_age2 + clim_zone2", "out_seas212 + med_age2 + pop_dens2", "out_seas212 + med_age2 + intens20212")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("out_seas212 + med_age2 + intens20212 + hemi2", "out_seas212 + med_age2 + intens20212 + clim_zone2", "out_seas212 + med_age2 + intens20212 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

for (i in c("out_seas212 + med_age2 + intens20212 + hemi2 + clim_zone2", "out_seas212 + med_age2 + intens20212 + hemi2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSonset2, control = coxph.control(iter.max = 1000))))
}

Modonset2 = coxph(Surv(time, event) ~ out_seas212 + med_age2 + intens20212 + hemi2, data = DSonset2, control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

Modonset2 = coxph(Surv(time, event) ~ out_seas212 + med_age2 + intens20212 + hemi2 + clim_zone2, data = DSonset2, control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

Modonset2 = coxph(Surv(time, event) ~ out_seas212 + med_age2 + intens20212 + hemi2 + pop_dens2, data = DSonset2, control = coxph.control(iter.max = 1000))
tidy(Modonset2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

#================================================================
# REGRESSION ANALYSIS OF PEAK 2021
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("clim_zone2", "strin_indx2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

for (i in c("clim_zone2 + strin_indx2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak1, control = coxph.control(iter.max = 1000))))
}

Modpeak1 = coxph(Surv(time, event) ~ clim_zone2 + strin_indx2, data = DSpeak1, control = coxph.control(iter.max = 1000))
tidy(Modpeak1, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

#================================================================
# REGRESSION ANALYSIS OF PEAK 2022
#================================================================

#fit the models iteratively & store model fitted values
for (i in c("hemi2", "out_seas212", "strin_indx2", "pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + out_seas212", "hemi2 + strin_indx2", "hemi2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

for (i in c("hemi2 + strin_indx2 + out_seas212", "hemi2 + strin_indx2 + pop_dens2")) {
  print(AIC(coxph(as.formula(paste0("Surv(time, event) ~", i)), data = DSpeak2, control = coxph.control(iter.max = 1000))))
}

Modpeak2 = coxph(Surv(time, event) ~ hemi2 + strin_indx2, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

Modpeak2 = coxph(Surv(time, event) ~ hemi2 + strin_indx2 + out_seas212, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)

Modpeak2 = coxph(Surv(time, event) ~ hemi2 + strin_indx2 + pop_dens2, data = DSpeak2, control = coxph.control(iter.max = 1000))
tidy(Modpeak2, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)
