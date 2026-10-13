# Jennifer Selgrath
# MPA Watch Intercept Survey

# Goal : Driver file to run code for mpa watch intercept survey data analysis

# ------------------
library(tidyverse)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

# review data and remove unneeded columns
source("./bin/data_cleaning.R")
# input:  ./data/mwis_phase2_20260921.csv
# output: ./results/mwis_p2_clean1.csv

# clean zip and country data
source("./bin/data_cleaning2.R")
# input:  ./results/mwis_p2_clean1.csv
# output: ./results/mwis_p2_clean2.csv