# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : to separate out data types and categorize non-responses

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

d1<-read_csv("./results/mwis_p2_clean3.csv")%>%
  glimpse()

# select out metadata
d2<-d1%>%
  select(progress:m11_notes,
         img_p1_id:img_p2_type)%>%
  glimpse()

# select survey findings
d3<-d1%>%
  select(response_id,m1_survey_date,
         q1_2_participation:q17_comment,
         m2_1_1_start_hour,m2_3_1_start_ampm, m2_1_2_end_hour,m2_3_2_end_ampm,progress,m3_program,m4_site_transect,q2_home_other:residence_detail)%>%
  glimpse()


# participation -----------
unique(d3$q1_2_participation)

# filter for people who participated
d4<-d3%>%
  filter(q1_2_participation=="They participated in the survey")%>%
  glimpse()

d4_no<-d3%>%
  filter(q1_2_participation!="They participated in the survey")%>%
  glimpse()
# view(d4_no)  

unique(d4_no$q1_3_non_response_reason)

write_csv(d2,"./results/mwis_p2_clean4_metadata.csv")
write_csv(d3,"./results/mwis_p2_clean4_maindata_all.csv")
write_csv(d4,"./results/mwis_p2_clean4_maindata_responses.csv")
write_csv(d4_no,"./results/mwis_p2_clean4_maindata_non_responses.csv")