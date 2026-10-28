# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: clean and organize barriers/difficulties
# ------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean5.csv")%>%
   glimpse()

# check out difficulties
unique(d1$q6_difficulties)       
unique(d1$q6_difficulties_text) # 85 write in responses

# lookup table for categories ------------
barrier_lookup <- c(
  "Transportation"        = "Transportation/Parking",
  "Physically accessing"  = "Physical/Terrain access",
  "Lack of clean"         = "Amenities/Cleanliness",
  "Travel"                = "Distance",
  "Lack of time"          = "Lack of time",
  "Cost"                  = "Cost",
  "Physical safety"       = "Physical safety",
  "Feeling unwelcome"     = "Feeling unwelcome",
  "Nothing to report"     = "No barriers",
  "Other"                 = "Other"
)

# function to clean categories ---------
clean_responses <- function(text) {
  if (is.na(text) || text == "Decline to respond") return(NA_character_)
  
  # Split on commas that are not inside parentheses
  items <- str_split(text, ",\\s*(?![^()]*\\))")[[1]]
  items <- str_trim(items)
  
  # Map each item to its short label
  cleaned_items <- map_chr(items, function(item) {
    matched_idx <- which(str_detect(item, names(barrier_lookup)))
    if (length(matched_idx) > 0) barrier_lookup[matched_idx[1]] else NA_character_
  })
  
  # Drop unmatched/NA elements and remove duplicate tags within the same row
  cleaned_items <- unique(cleaned_items[!is.na(cleaned_items)])
  if (length(cleaned_items) == 0) return(NA_character_)
  
  paste(cleaned_items, collapse = "; ")
}

# apply fxn --------------
d2 <- d1 %>%
  mutate(q6_difficulties2 = map_chr(q6_difficulties, clean_responses)) %>%
  glimpse()


# -------------------------
# write in barriers -------
# -------------------------
recode_barrier_text <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(NA_character_)
  
  txt <- tolower(x)
  tags <- character(0)
  
  # --- Existing categories (standardized to match barrier_lookup exactly) ---
  if (str_detect(txt, "gasoline|park|traffic|entrance|transit|transient|bikers|drive|driving")) {
    tags <- c(tags, "Transportation/Parking")
  }
  if (str_detect(txt, "walking|limitation|stairs|transporting painting")) {
    tags <- c(tags, "Physical/Terrain access")
  }
  if (str_detect(txt, "trash|litter|cleanliness|food|costco")) {
    tags <- c(tags, "Amenities/Cleanliness")
  }
  if (str_detect(txt, "prices|\\bcosts?\\b") && !str_detect(txt, "cost of pollution")) { #exclude 'cost of pollution'
    tags <- c(tags, "Cost")
  }
  if (str_detect(txt, "distance|far")) {
    tags <- c(tags, "Distance")
  }
  if (str_detect(txt, "drugs|aggressive dogs")) {
    tags <- c(tags, "Physical safety")
  }
  
  # --- New categories ---
  if (str_detect(txt, "weather|cold|wind|sun|sunburn|cancer|water is cold|tide|pollution|sewage|dirty water")) {
    tags <- c(tags, "Environmental safety/conditions")
  }
  if (str_detect(txt, "rocks|tar|seaweed|flies|sand")) {
    tags <- c(tags, "Natural environment")
  }
  if (str_detect(txt, "crowd|busy|too many people|too many humans|lack of space")) {
    tags <- c(tags, "Overcrowding")
  }
  if (str_detect(txt, "regulation|rules|restriction|ticket|not allowed|no dogs")) {
    tags <- c(tags, "Rules & regulations")
  }
  if (str_detect(txt, "disrespectful|smoking|music|bonfire|waste|rude|sea lions")) {
    tags <- c(tags, "Feeling protected/Visitor behavior")
  }
  if (str_detect(txt, "spain|kid|child|daughter")) {
    tags <- c(tags, "Lack of time")
  }
  # Do not trigger 'Interest' if the comment mentions sea lions
  if (str_detect(txt, "doesn't like|don't like") && !str_detect(txt, "sea lion")) {
    tags <- c(tags, "Interest")
  }
  
  # Fallback for anything else (or lone keywords like "sand")
  if (length(tags) == 0) {
    if (str_detect(txt, "sand")) {
      tags <- "Physical/Terrain access"
    } else {
      tags <- "Other"
    }
  }
  
  paste(unique(tags), collapse = "; ")
}


# Apply function -------------
d3 <- d2 %>%
  mutate(q6_difficulties_text2 = purrr::map_chr(q6_difficulties_text, recode_barrier_text))


# function: Combine cleaned categories and write in ----------------------
combine_barriers <- function(structured, write_in) {
  # Collect and trim structured items
  s_items <- if (!is.na(structured) && str_trim(structured) != "") {
    str_trim(str_split(structured, ";\\s*")[[1]])
  } else {
    character(0)
  }
  
  # Collect and trim write-in items
  w_items <- if (!is.na(write_in) && str_trim(write_in) != "") {
    str_trim(str_split(write_in, ";\\s*")[[1]])
  } else {
    character(0)
  }
  
  # If a recoded write-in exists, remove the generic "Other" tag
  if (length(w_items) > 0) {
    s_items <- s_items[s_items != "Other"]
  }
  
  # Merge, trim, and deduplicate
  all_items <- unique(c(s_items, w_items))
  all_items <- all_items[!is.na(all_items) & all_items != ""]
  
  # If any actual barrier or "Other" exists, remove "No barriers"
  if (any(all_items != "No barriers")) {
    all_items <- all_items[all_items != "No barriers"]
  }
  
  if (length(all_items) == 0) return(NA_character_)
  
  paste(all_items, collapse = "; ")
}

# Apply to create the final unified q6_barriers column
d4 <- d3 %>%
  mutate(q6_barriers = map2_chr(q6_difficulties2, q6_difficulties_text2, combine_barriers)) %>%
  glimpse()

# check -----------
d4 %>%
  select(q6_difficulties2, q6_difficulties_text, q6_difficulties_text2, q6_barriers) %>%
  filter(!is.na(q6_difficulties_text) | str_detect(q6_difficulties2, "Nothing to report|No barriers")) %>%
  filter(!is.na(q6_difficulties_text))%>%
  print(n = 150)

# save -------------------
write_csv(d4,"./results/mwis_p2_clean6.csv")