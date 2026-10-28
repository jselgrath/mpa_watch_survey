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

# create categories for residence info: state, international, etc
# TO DO - add variable for local in oregon near Tolowa
source("./bin/data_cleaning3.R")
# input: ./results/mwis_p2_clean2.csv
# output:  ./results/mwis_p2_clean3_zip.csv


# separate metadata, responses, and no-responses
source("./bin/data_cleaning4.R")
# input: ./results/mwis_p2_clean3.csv
# output:  ./results/mwis_p2_clean4_metadata.csv
#         ./results/mwis_p2_clean4_maindata_all.csv
#         ./results/mwis_p2_clean4_maindata_responses.csv
#         ./results/mwis_p2_clean4_maindata_non_responses.csv

# -------------------

# summarize no-responses
source("./bin/non-responses.R")
# input: ./results/mwis_p2_clean3_maindata_non_responses.csv
# output:  ./doc/survey_non_response.png
#         ./results/mwis_p2_clean3_maindata_non_responses2.csv # includes categories


# map of responses by zip (california)
source("./bin/map_zip.R")
# input: ./gis_data/zip_codes_ca/california_zip_codes4.shp
#         ./results/mwis_p2_clean3_maindata_responses.csv
#         ./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp
# output:  ./doc/mwis_map_zip_codes.png


# map of respondents zip by program (california)
source("./bin/map_program.R")
# input:    ./gis_data/zip_codes_ca/california_zip_codes4.shp
#           ./results/mwis_p2_clean3_maindata_responses.csv
#           ./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp
# output:   ./doc/mwis_map_program2.png

# map of respondents zip by state 
source("./bin/map_national.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:   ./doc/mwis_map_us_states.png

# map of respondents' states by program (national) (q2)
source("./bin/map_national2.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:   ./doc/mwis_map_us_program_faceted.png    # better output
#           ./doc/mwis_map_us_program2.png          # simpler output

# map of respondents zip by country - no US 
source("./bin/map_international.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:   ./doc/mwis_map_international.png

# map of respondents zip by country - with US
# update US N to match manually
source("./bin/map_international2.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:   ./doc/mwis_map_international2.png

# ---------------------------
# where started day (q1) 
# origin - percent CA, US, International (bar graph)
source("./bin/origin_home.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:   ./doc/home_location_DETAIL.png
#           ./doc/origin_DETAIL.png

# activities (q3/q4 - clean write ins and create combined categories)
source("./bin/purpose_activities1.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# output:  ./results/mwis_p2_clean5.csv

# activities (q3/q4 - graph)
source("./bin/purpose_activities2.R")
# input:    ./results/mwis_p2_clean5.csv
# output:   ./doc/activities_all_q3.png
#           ./doc/activities_most_important_q4.png

# frequency of visit CA beaches (q5)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# challenges (q6)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# MPA awareness (q7)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# MPA location (q8)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# MPA info sources (q9)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# management priority (q10)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output:  

# management priority - long form (may skip this till later) (q11)
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output: 

# non-english speaking languages
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output: 

# time of day, weekday vs weekend
# formatting of date in gemini
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output: 

# season
source("./bin/NAME.R")
# input:    ./results/mwis_p2_clean5.csv
# output: 


# languages
source("./bin/languages.R")
# input:    ./results/mwis_p2_clean4_maindata_responses.csv
# updat to mwis_p2_clean5.csv
# output:   ./doc/languages_DETAIL.png  # detail = all, program, etc
#           ./doc/language_non_english_DETAIL.png
#           ./doc/language_no_english_at_home_DETAIL.png