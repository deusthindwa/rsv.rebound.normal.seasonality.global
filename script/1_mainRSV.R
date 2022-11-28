#By Deus Thindwa
#18/11/2022
#global reemergence of RSV onset, duration and peak

#====================================================================

#load packages for analysis
pacman::p_load(char = c("lubridate", "tidyverse", "dplyr", "here", "rio", 
                        "scales", "boot", "magrittr",  "mvtnorm", "zoo", 
                        "patchwork", "PropCIs", "reshape2","purrr", "minqa", 
                        "ggridges", "timetk", "ggbreak", "ggpubr", "gridExtra", 
                        "curl", "archive", "jsonlite", "here"))

#====================================================================

#run archiving file
source(here("script", "2_fileCache.R"))

#run archiving file
#source(here("script", "3_gitSetup.R"))

#run archiving file
source(here("script", "4_runIfExpired.R"))

#load RSV dataset from Jason format
source(here("script", "5_loadRSVdata.R"))

#data manipulation to get needed settings
source(here("script", "6_manipRSVdata.R"))








