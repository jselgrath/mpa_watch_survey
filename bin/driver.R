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
#         ./doc/country_count.csv

# separate metadata, responses, and no-responses
source("./bin/data_cleaning3.R")
# input: ./results/mwis_p2_clean2.csv
# output:  ./results/mwis_p2_clean3_metadata.csv
#         ./results/mwis_p2_clean3_maindata_all.csv
#         ./results/mwis_p2_clean3_maindata_responses.csv
#         ./results/mwis_p2_clean3_maindata_non_responses.csv

# summarize no-responses
source("./bin/non-responses.R")
# input: ./results/mwis_p2_clean3_maindata_non_responses.csv
# output:  ./doc/survey_non_response.png
#         ./results/mwis_p2_clean3_maindata_non_responses2.csv # includes categories


# summarize no-responses
source("./bin/map_zip.R")
# input: ./gis_data/zip_codes_ca/california_zip_codes4.shp
#         ./results/mwis_p2_clean3_maindata_responses.csv
#         ./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp
# output:  ./doc/mwis_map_zip_codes.png


# summarize no-responses
source("./bin/map_program.R")
# input: ./gis_data/zip_codes_ca/california_zip_codes4.shp
#         ./results/mwis_p2_clean3_maindata_responses.csv
#         ./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp
# output:  ./doc/mwis_map_program2.png


