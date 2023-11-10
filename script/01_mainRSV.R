#Authors: Deus & Dan
#Date: 01/03/2023
#Title: Rebound to normal RSV dynamics post COVID-19 suppression

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

#set directory for downloaded datasets
#source("script/02_fileCache.R")

#set archiving path for downloaded datasets
#source("script/03_runIfExpired.R")

#load RSV case time series datasets (from WHO)
#source("script/04_loadCases.R")

#alternative to reading the datasets from various sources above, load directly from "reproduce" folder
#ensure that results will be reproducible since data not coming from updated sources
rsv_all <- rio::import(here("data", "reproduce", "rsv_all.csv"))
rsv_dtw <- rio::import(here("data", "reproduce", "rsv_dtw.csv"))
stringency <- rio::import(here("data", "reproduce", "stringency.csv"))
climate <- rio::import(here("data", "reproduce", "climate.csv"))

#plot RSV time series and weekly dynamics
source("script/05_timeSeries.R")

#Compute RSV onset timing
source("script/06_onset.R")

#compute RSV peak timing 
source("script/07_peak.R")

#compute RSV growth rates
source("script/08_growth.R")

#compute intensity
source("script/09_intensity.R")

#compute univariate regression models
source("script/10_regression.R")

#compute multivariate regression models
source("script/11_multivariate.R")

#time series dynamic time warping and classification
source("script/12_dynTimeWarp.R")

#descriptive stats
source("script/13_computeDesc.R")
