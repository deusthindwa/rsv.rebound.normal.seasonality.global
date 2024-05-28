#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

#====================================================================
#====================================================================

#load a package "pacman" used for for installing and loading other packages
if(!require(pacman)) install.packages("pacman")

#use pacman to load packages for analysis
pacman::p_load(char = c("lubridate", "tidyverse", "dplyr", "tidyr", "broom", "rio", "scales", "boot", "magrittr",  "mvtnorm", "zoo", "stringr", "survminer", "TSclust", "CircStats",
                        "patchwork", "PropCIs", "reshape2","purrr", "minqa", "ggridges", "timetk", "ggbreak", "ggpubr", "gridExtra", "readr", "survival", "dtw", "cluster",
                        "curl", "archive", "jsonlite", "janitor", "ggh4x", "distcrete", "epitrix", "mgcv", "pspline.inference", "RCurl", "XML", "psych", "ie2misc", "ggdendro", 
                        "rlist", "tsibble", "htmlwidgets", "plotly", "utils", "MLmetrics", "circular", "gsignal", "moderndive", "knitr", "dtwclust", "dendextend", "here"))

#set seed for entire session to ensure reproducibility using a task call
addTaskCallback(function(...) {set.seed(12345); TRUE})

#turn off the task call for set seed if needed
#removeTaskCallback(1)

#====================================================================
#====================================================================

#load directly from "reproduce" folder
#this ensures that results will be reproducible since data area not coming from updated sources
rsv_all <- rio::import(here("data", "reproduce", "rsv_all.csv")) %>% dplyr::mutate(hemi = if_else(country == "Colombia", "Southern hemisphere", hemi)) #Colombia reclassified as Southern Hemi
rsv_dtw <- rio::import(here("data", "reproduce", "rsv_dtw.csv")) %>% dplyr::mutate(hemi = if_else(country == "Colombia", "Southern hemisphere", hemi)) #Colombia reclassified as Southern Hemi
stringency <- rio::import(here("data", "reproduce", "stringency.csv"))
climate <- rio::import(here("data", "reproduce", "climate.csv"))

#plot RSV time series and weekly dynamics
source("script/05_tsDynamics.R")

#Compute RSV onset timing
source("script/06_onset.R")

#compute RSV peak timing 
source("script/07_peak.R")

#compute RSV growth rates
source("script/08_growth.R")

#compute intensity
source("script/09_intensity.R")

#compute regression models
source("script/10_regressB.R")

#time series dynamic time warping and classification
source("script/11_dtw.R")

#descriptive stats
source("script/12_describe.R")
