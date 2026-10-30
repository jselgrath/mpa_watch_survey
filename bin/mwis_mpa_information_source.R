# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa info sources
# NOT FINISHED
# ------------------

library(tidyverse)
library(scales)
library(viridis)
library(ggplot2)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean6.csv")%>%
mutate(
    q9_information_source = q9_information_source %>%
      str_replace_all("\\s*,\\s*", ",") %>%
      str_trim())%>%
  glimpse()

unique(d1$q9_information_source)         
unique(d1$q9_information_source_text)   

# -------------------------
# 1. Clean whitespace & recode 'Internet' to 'Internet/Maps' in structured selections
d2 <- d1 %>%
  mutate(
    q9_information_source2 = q9_information_source %>%
      str_replace_all("\\s*,\\s*", "; ") %>%
      str_replace_all("\\bInternet\\b", "Internet/Maps") %>%
      str_trim()
  )

# 2. Recode write-in responses
recode_info_text <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(NA_character_)
  
  txt <- tolower(x)
  tags <- character(0)
  
  # Non-responses / Unsure / Deductions
  if (str_detect(txt, "don't know|not sure|didn't know|did not know|unsure|skipped|no response|declined|not known|no reason")) {
    return("Unsure / Did not know")
  }
  
  # --- Existing survey categories ---
  if (str_detect(txt, "word of mouth|someone told|told them|co-workers|people like ourselves|friends w/")) {
    tags <- c(tags, "Family/friends")
  }
  if (str_detect(txt, "grew up|growing up|remember|remembers|own knowledge|personal knowledge|our knowledge|i knew|i know|previous visit|exploring")) {
    tags <- c(tags, "Local knowledge")
  }
  if (str_detect(txt, "signage|sign") && !str_detect(txt, "no sign|lack of sign|didn't see|didn't look")) {
    tags <- c(tags, "Signage")
  }
  # Combined Internet/Maps category
  if (str_detect(txt, "dfw website|website|map|navigation")) {
    tags <- c(tags, "Internet/Maps")
  }
  if (str_detect(txt, "film festival|maritime museum")) {
    tags <- c(tags, "Outreach event")
  }
  
  # --- Work / Volunteer / Docent ---
  # Includes professionals in education (educator, professor)
  # Includes commercial recreation workers/guides (guide, kayak shop, worker)
  if (str_detect(txt, "educator|professor|worker|volunteer|docent|biologist|costeau|cousteau|channelkeeper|laguna ocean|surfrider|oil rig|worked on|work in past|research")) {
    tags <- c(tags, "Work/Volunteer")
  }
  
  # --- Education / Academic ---
  # General coursework, students, majors (excluding educators/professors)
  if (str_detect(txt, "school|university|college|class|classes|major|student|bren|diving course") && 
      !str_detect(txt, "educator|professor")) {
    tags <- c(tags, "Education/Academic")
  }
  
  # --- Tours / Commercial recreation ---
  # Participants/general recreation (excluding guides/shop staff)
  if (str_detect(txt, "kayak|tour|fishing|pilot|guide|docent") && 
      !str_detect(txt, "worker")) { # guide|kayak shop|
    tags <- c(tags, "Tours/Docents")
  }
  
  # --- Other specific coastal/environmental context ---
  if (str_detect(txt, "state beach|national park|national seashore|pt reyes|campground|context clues|fences|restricted|oil well|oil use|tidepool|sea life|seals")) {
    tags <- c(tags, "Context clues/Observation")
  }
  if (str_detect(txt, "survey|this survey|asking me|surveyor|you asked|survey is being conducted|survey regarding|survey before")) {
    tags <- c(tags, "Survey interaction")
  }
  if (str_detect(txt, "tolowa")) {
    tags <- c(tags, "Traditional/Tribal knowledge")
  }
  if (str_detect(txt, "news|tv|reading")) {
    tags <- c(tags, "News/Media")
  }
  if (str_detect(txt, "no sign|lack of sign|reading")) {
    tags <- c(tags, "No Information")
  }
  # Fallback for remaining cases
  if (length(tags) == 0) {
    if (str_detect(txt, "no sign|lack of sign|guessing|power of deduction|assuming|assumption|inferred")) {
      tags <- "Context clues / Observation"
    } else {
      tags <- "Other"
    }
  }
  
  paste(unique(tags), collapse = "; ")
}

# 3. Apply write-in recoding
d3 <- d2 %>%
  mutate(q9_information_source_text2 = purrr::map_chr(q9_information_source_text, recode_info_text))

# 4. function - merge structured options and recoded write-ins
combine_sources <- function(structured, write_in) {
  s_items <- if (!is.na(structured) && str_trim(structured) != "") {
    str_trim(str_split(structured, ";\\s*")[[1]])
  } else {
    character(0)
  }
  
  w_items <- if (!is.na(write_in) && str_trim(write_in) != "") {
    str_trim(str_split(write_in, ";\\s*")[[1]])
  } else {
    character(0)
  }
  
  # If concrete write-in exists, drop placeholder "Other"
  if (length(w_items) > 0) {
    s_items <- s_items[s_items != "Other"]
  }
  
  # Combine, deduplicate, and drop blanks/NAs
  all_items <- unique(c(s_items, w_items))
  all_items <- all_items[!is.na(all_items) & all_items != ""]
  
  if (length(all_items) == 0) return(NA_character_)
  paste(all_items, collapse = "; ")
}

# 5. Build final unified column
d4 <- d3 %>%
  mutate(q9_info_sources = purrr::map2_chr(q9_information_source2, q9_information_source_text2, combine_sources))

# Verification check for reclassified entries
d4 %>%
  # filter(str_detect(tolower(q9_information_source_text), "educator|professor|kayak|guide|shop|map|website")) %>%
  filter(!is.na(q9_information_source_text)) %>%
  select(q9_information_source,q9_information_source_text, q9_information_source_text2, q9_info_sources) %>%
  arrange(q9_info_sources)%>%
  print(n = 100)


write_csv(d4,"./results/mwis_p2_clean7.csv")