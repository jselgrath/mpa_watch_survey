# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : remove unneeded columns, clean names
# note: phase II is only in english

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

d1<-read_csv("./data/mwis_phase2_20260921.csv")%>%
  # remove columns with NA or repeated/unnecessary information
  select(-recipientLastName,-recipientFirstName,recipientEmail,-externalDataReference,-startDate,-endDate,-status,-ipAddress,-duration,-recipientEmail,-userLanguage,-Q_RecaptchaScore)%>%
  glimpse()

# graph data to look at finished by progress
d1 %>%
  slice(-1) %>% 
  mutate(progress=as.numeric(progress))%>%
  ggplot(aes(x = progress, fill = finished)) +  geom_bar() +
  labs(x = "Progress (%)", y = "Count", fill = "Finished") +
  theme_minimal()

unique(d1$distributionChannel) # anonymous, preview
unique(d1$userLanguage)
unique(d1$recipientEmail)
unique(d1$externalDataReference)

unique(d1$finished) #True, False
unique(d1$recordedDate) # date of data entry
unique(d1$locationLatitude) # geographic location of data entry



# new column names -------------------------------

# first row has info for columns 
raw_headers <- unlist(d1[1, ], use.names = FALSE)
print(raw_headers)

# clean strings into column headers
clean_headers <- raw_headers %>%
  # Remove long sub-question instructions/prompts after dashes or asterisks
  str_remove_all("- Selected Choice|- Text|\\*NOTE:.*") %>%
  # Simplify file upload metadata tags
  str_replace_all("Q3\\.1_(.*)", "img_page1_\\1") %>%
  str_replace_all("Q3\\.2_(.*)", "img_page2_\\1") %>%
  # Convert to clean snake_case
  make_clean_names() %>%
  # Trim long names to stay concise (keeps first 40 chars)
  str_trunc(width = 40, ellipsis = "") %>%
  # Remove trailing underscores left by truncation
  str_remove("_+$") %>%
  # Ensure every column starts with "q_"
  # str_replace("^(?!q_)", "q_")%>%
  glimpse()

# view(clean_headers)

# clean headers with old and new
# clean_headers2 <- c(
#   "progress"                       = "progress", 
#   "finished"                       = "finished", 
#   "date_entered"                   = "recorded_date", 
#   "response_id"                    = "response_id", 
#   "latitude_entry"                 = "location_latitude_location_latitude", 
#   "longitude_entry"                = "location_longitude_location_longitude", 
#   "distribution_channel"           = "q_distribution_channel_distribution_channe", 
#   
#   # metadata about survey
#   "m1_survey_date"                 = "m1_date_mm_dd_yyyy", 
#   "m2_1_1_start_hour"              = "m2_number_1_1_please_list_your_survey_st", 
#   "m2_1_2_end_hour"                = "m2_number_1_2_please_list_your_survey_st", 
#   "m2_2_1_start_min"               = "m2_number_2_1_please_list_your_survey_st", 
#   "m2_2_2_end_min"                 = "m2_number_2_2_please_list_your_survey_st", 
#   "m2_3_1_start_ampm"              = "m2_number_3_1_please_list_your_survey_st", 
#   "m2_3_2_end_ampm"                = "m2_number_3_2_please_list_your_survey_st", 
#   "m3_program"                     = "m3_what_program_are_you_collecting_inter", 
#   "m4_site_transect"               = "m4_site_transect_name", 
#   "m5_surveyors"                   = "m5_surveyor_name_s_first_and_last_you_ca", # first last - can be more than one
#   "m6_encoder"                     = "m6_first_and_last_name_of_person_enterin", 
#   "m7_visitor_no"                  = "m7_estimated_total_number_of_visitors_at", 
#   "m8_surveys_no"                  = "m8_number_of_surveys_administered_during", 
#   "m9_survey_declines"             = "m9_number_of_non_responses_declines", 
#   "m10_decline_reason"             = "m10_if_you_had_non_respondents_please_li", 
#   "m11_notes"                      = "m11_notes_e_g_implementation_challenges", 
#   
#   # respondent metadata
#   "q1_2_participation"             = "q_q1_2_did_the_person_you_approached_parti", 
#   "q1_3_non_response_reason"       = "q_q1_3_if_the_participant_gave_a_reason_fo", 
#   "q1_4_time_completion_min_sec"   = "q_q1_4_time_it_took_to_complete_this_indiv", 
#   "q1_origin"                      = "q_q1_did_you_start_your_day_from_home_or_a", 
#   "q1_2_origin_text"               = "q_q1_2_text_did_you_start_your_day_from_ho", 
#   "q2_q2_home_zip_country"         = "q_q2_q2_where_is_home_for_you_please_provi", 
#   "q2_1_q2_home_zip_country2"      = "q_q2_1_text_q2_where_is_home_for_you_pleas", 
#   "q2_2_q2_home_zip_country3"      = "q_q2_2_text_q2_where_is_home_for_you_pleas", 
#   "q3_purpose"                     = "q_q3_q3_what_is_the_primary_purpose_of_you", 
#   "q3_purpose_text"                = "q_q3_1_text_q3_what_is_the_primary_purpose", 
#   "q4_purpose_most_important"      = "q_q4_q4_from_the_list_of_activities_above", 
#   "q4_purpose_most_important_text" = "q_q4_1_text_q4_from_the_list_of_activities", 
#   "q5_frequency"                   = "q_q5_q5_how_often_do_you_spend_time_in_oce", 
#   "q6_difficulties"                = "q_q6_q6_is_there_anything_that_makes_being", 
#   "q6_difficulties_text"           = "q_q6_9_text_q6_is_there_anything_that_make", 
#   "q7_mpa_awareness"               = "q_q7_q7_prior_to_this_survey_were_you_awar", 
#   "q8_mpa_location"                = "q_q8_q8_based_on_your_knowledge_do_you_kno", 
#   "q9_information_source"          = "q_q9_q9_if_you_responded_yes_or_no_to_the", 
#   "q9_information_source_text"     = "q_q9_1_text_q9_if_you_responded_yes_or_no", 
#   "q10_ocean_management"           = "q_q10_q10_how_important_is_it_to_have_ocea", 
#   "q11_mpa_value_source"           = "q_q11_q11_thinking_about_your_previous_res", 
#   "q12_year_born"                  = "q_q12_q12_what_year_were_you_born_if_the_p", 
#   "q13_gender"                     = "q_q13_q13_how_do_you_identify_your_gender", 
#   "q13_gender_text"                = "q_q13_5_text_q13_how_do_you_identify_your", 
#   "q14_race"                       = "q_q14_q14_how_do_you_identify_your_race_et", 
#   "q14_race_text"                  = "q_q14_1_text_q14_how_do_you_identify_your", 
#   
#   "q15_language"                   = "q15_q15_what_language_s_do_you_speak_at", 
#   "q15_language_text"              = "q15_1_text_q15_what_language_s_do_you_sp", 
#   "q16_results"                    = "q16_q16_would_you_like_to_receive_the_re", 
#   "q17_comment"                    = "q17_q17_is_there_anything_else_you_would", 
#   
#   # images of survey
#   "img_id_p1"                      = "img_page1_id_please_upload_a_jpeg_of_the", 
#   "img_name_p1"                    = "img_page1_name_please_upload_a_jpeg_of_t", 
#   "img_size_p1"                    = "img_page1_size_please_upload_a_jpeg_of_t", 
#   "img_type_p1"                    = "img_page1_type_please_upload_a_jpeg_of_t", 
#   "img_id_p2"                      = "img_page2_id_please_upload_a_jpeg_of_the", 
#   "img_name_p2"                    = "img_page2_name_please_upload_a_jpeg_of_t", 
#   "img_size_p2"                    = "img_page2_size_please_upload_a_jpeg_of_t", 
#   "img_type_p2"                    = "img_page2_type_please_upload_a_jpeg_of_t"
# )



clean_headers_list <- c(
  "progress", 
  "finished", 
  "date_entered", 
  "response_id", 
  "latitude_entry", 
  "longitude_entry", 
  "distribution_channel", 
  
  # metadata about survey
  "m1_survey_date", 
  "m2_1_1_start_hour", 
  "m2_1_2_end_hour", 
  "m2_2_1_start_min", 
  "m2_2_2_end_min", 
  "m2_3_1_start_ampm", 
  "m2_3_2_end_ampm", 
  "m3_program", 
  "m4_site_transect", 
  "m5_surveyors", 
  "m6_encoder", 
  "m7_visitor_no", 
  "m8_surveys_no", 
  "m9_survey_declines", 
  "m10_decline_reason", 
  "m11_notes", 
  
  # respondent metadata
  "q1_2_participation", 
  "q1_3_non_response_reason", 
  "q1_4_time_completion_min_sec", 
  "q1_origin", 
  "q1_2_origin_text", 
  "q2_home_zip_country", 
  "q2_zip_code", 
  "q2_other_country", 
  "q3_purpose", 
  "q3_purpose_text", 
  "q4_purpose_most_important", 
  "q4_purpose_most_important_text", 
  "q5_frequency", 
  "q6_difficulties", 
  "q6_difficulties_text", 
  "q7_mpa_awareness", 
  "q8_mpa_location", 
  "q9_information_source", 
  "q9_information_source_text", 
  "q10_ocean_management", 
  "q11_mpa_value_source", 
  "q12_year_born", 
  "q13_gender", 
  "q13_gender_text", 
  "q14_race", 
  "q14_race_text", 
  "q15_language", 
  "q15_language_text", 
  "q16_results", 
  "q17_comment", 
  
  # images of survey
  "img_p1_id", 
  "img_p1_name", 
  "img_p1_size", 
  "img_p1_type", 
  "img_p2_id", 
  "img_p2_name", 
  "img_p2_size", 
  "img_p2_type"
)


# update names & fix NA (change all "N/A" to NA)
d2 <- d1 %>%
  setNames(clean_headers_list) %>%
  slice(-1) %>%
  mutate(across(everything(), ~ ifelse(. == "N/A", NA_character_, .)))%>% 
  glimpse()


write_csv(d2,"./results/mwis_p2_clean1.csv")

