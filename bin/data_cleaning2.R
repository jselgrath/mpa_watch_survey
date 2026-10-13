# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : clean zip codes and country codes

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

d1<-read_csv("./results/mwis_p2_clean1.csv")%>%
  
  ##Removes leading and trailing white spaces across entire dataset
  mutate(across(where(is.character), str_trim))%>%
  
  #remove datasets where progress is less than 90% finished
  filter(progress>90)%>% 

##Replaces "" or "Choose not to answer" with NA across the entire dataset
  mutate(across(where(is.character), ~ if_else(.x %in% c("", "Choose not to answer"), NA_character_, .x)))%>%
  glimpse()

# evaluate zip code responses ---------------
cbind(d1$q2_home_zip_country,d1$q2_zip_code,d1$q2_other_country)

###Cleans up Zipcode data, transforming those who provided it in long-from
d2 <- d1%>% 
  mutate(q2_home_other = q2_zip_code)%>%
  mutate(q2_zip_code = as.numeric(q2_zip_code))%>% 
  # mutate(q2_zip_code = str_replace(q2_zip_code, "-\\d+$", ""))%>%
  glimpse()

# view(cbind(d2$q2_home_zip_country,d2$q2_zip_code,d2$q2_other_country))

d2%>%
  mutate(zip2=as.numeric(q2_other_country))%>%
  filter(zip2>1)%>%
  select(response_id,q2_home_zip_country,q2_zip_code,q2_other_country,zip2)%>%
  select(response_id) #%>%
  # view()




# fixes for zip entered in country column-------------
target_ids <- c(
  # "R_Cbt2KixnuxlOwmC", # sweden
  "R_PsDbRLTZlcgKqlv",  # nevada
  # "R_upNOevULSXUHCSe", # russia
  "R_gqqILwWFhvpw9fz",  # midway, ca
  "R_5K24Mpxoj0BOUnA",  # georgia
  "R_1aQbkekZVNhHmQh") # checked this one - could not find others in photos on first look

d2b <- d2 %>%
  mutate(
    q2_home_zip_country = if_else(response_id %in% target_ids, "Zip code", q2_home_zip_country),
    q2_zip_code         = if_else(response_id %in% target_ids, as.numeric(q2_other_country), q2_zip_code),
    q2_other_country    = if_else(response_id %in% target_ids, NA, q2_other_country)
  )

# check zips
d2b%>%
  filter(response_id=="R_5K24Mpxoj0BOUnA")%>%
  glimpse()




# country fixes ------------------

# look at list
d2b%>%
  mutate(zip2=as.numeric(q2_other_country))%>%
  filter(!is.na(q2_other_country))%>%
  select(response_id,q2_home_zip_country,q2_zip_code,q2_other_country)%>%
  select(q2_other_country)#%>%
  # view()

# fix for zip code entered wrong - identified manually with list above
id_country_map <- c(
  "R_Cbt2KixnuxlOwmC"= "Sweden",
  "R_upNOevULSXUHCSe"="Russia")

d2c <- d2b %>%
  mutate(q2_other_country = case_when(
      response_id %in% names(id_country_map) ~ id_country_map[response_id],TRUE ~ q2_other_country),
    q2_zip_code         = if_else(response_id %in% names(id_country_map), NA, q2_zip_code),
    q2_home_zip_country = if_else(response_id %in% names(id_country_map), "Or non-US country", q2_home_zip_country))%>%
  glimpse()

# check
d2c%>%
  filter(response_id=="R_upNOevULSXUHCSe")%>%
  glimpse()

# update for spelling and zip codes for other countries


d2d <- d2c %>%
    mutate(
      q2_other_country_orig=q2_other_country,
      q2_other_country = str_trim(q2_other_country) %>% str_to_upper(), # Standardize whitespace and case
      q2_other_country = case_when( # 1. Match UK Postcode pattern (e.g., SW1A 2AA, SWIA 2AA typo, EC1A 1BB, etc.)
       str_detect(q2_other_country, "^[A-Z]{1,2}[0-9I][A-Z0-9]?\\s?[0-9][A-Z]{2}$") ~ "United Kingdom",
      
      # Known country typos & region overrides
      q2_other_country %in% c("MEXICO")                 ~ "Mexico",
      q2_other_country %in% c("U.K.", "UNITED KINGDOM") ~ "United Kingdom",
      q2_other_country == "PANEMA"                      ~ "Panama",
      q2_other_country == "QUEBEC, CANADA"              ~ "Canada",
      q2_other_country == "NLL IEY"                     ~ NA_character_, # Unresolvable - check in paper surveys
      TRUE ~ str_to_title(q2_other_country))) %>% # Standardize rest to Title Case
    glimpse()
  # select(-clean_input)

ctry<-d2d %>% 
  count(q2_other_country, name = "count")%>%
  glimpse()

countries<-d2d%>%
  select(q2_other_country)%>%
  unique()%>%
  glimpse()
unique(countries)

write_csv(d2d,"./results/mwis_p2_clean2.csv")
write_csv(ctry,"./doc/country_count.csv")