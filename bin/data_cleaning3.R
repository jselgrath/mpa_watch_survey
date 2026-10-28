# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : create categories from home zip codes
# THINK ABOUT OREGON AND TOLOWA!!

# ------------------
library(tidyverse)
library(janitor)
library(zipcodeR)

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

# ---------------------------------------------------------
# 1. Reference Dictionaries
# ---------------------------------------------------------
data("zip_code_db")

po_zip_lookup <- zip_code_db %>%
  select(zipcode, state) %>%
  distinct() %>%
  rename(zip_clean = zipcode, db_state = state)

# Full state name to 2-letter abbreviation
state_name_map <- setNames(state.abb, tolower(state.name))
state_name_map["district of columbia"] <- "DC"
state_name_map["washington d.c."]     <- "DC"
state_name_map["washington dc"]       <- "DC"

# Known cities / regions -> State
city_state_map <- c(
  "goleta"          = "CA",
  "west covina"     = "CA",
  "orange county"   = "CA",
  "point reyes"     = "CA",
  "auburn"          = "CA",
  "silicon valley"  = "CA",
  "silicon vallet"  = "CA",
  "buellton"        = "CA",
  "san francisco"   = "CA",
  "sacramento"      = "CA",
  "san diego"       = "CA",
  "newport beach"   = "CA",
  "laguna beach"    = "CA",
  "la county"       = "CA",
  "los angeles"     = "CA",
  "malibu"          = "CA",
  "topanga"         = "CA",
  "bolinas"         = "CA",
  "las vegas"       = "NV",
  "portland"        = "OR"
)

# ---------------------------------------------------------
# 2. Ingest & Clean Pipeline (Single Execution)
# ---------------------------------------------------------
d1 <- read_csv("./results/mwis_p2_clean2.csv") %>%
  glimpse()

d2<-d1%>%
  mutate(
    origin_clean   = str_trim(q1_2_origin_text),
    origin_lower   = str_to_lower(origin_clean),
    home_other_raw = str_trim(q2_home_other),
    home_other_low = str_to_lower(home_other_raw),

    # 1. Unhoused status
    is_unhoused = str_detect(
      coalesce(home_other_low, ""), 
      "houseless|no home|unhoused|homeless|van life|vanlife"
    ),

    # 2. Explicit refusals / non-responses in q2
    is_refusal = str_detect(
      coalesce(home_other_low, ""), 
      "^n/?a$|did not resp|declined|not known|unknown|unsure|^00000$"
    ),

    # 3. Handle multi-zips and typos
    extracted_home_zip = str_extract(home_other_raw, "^[0-9]{5}"),

    q2_zip_clean = case_when(
      !is.na(extracted_home_zip)                ~ as.numeric(extracted_home_zip),
      q2_zip_code %in% c(0)                     ~ NA_real_,
      q2_zip_code == 8400                       ~ 8400,  # Will pad to 08400 -> NJ
      q2_zip_code == 9291                       ~ 92691, # Mission Viejo, CA typo
      q2_zip_code == 8907                       ~ 98907, # Yakima, WA
      q2_zip_code == 902109                     ~ 90210, # Beverly Hills 6-digit typo
      q2_zip_code == 943117                     ~ 94301, # Palo Alto 6-digit typo
      q2_zip_code == 911739                     ~ 91107, # Pasadena 6-digit typo
      q2_zip_code == 915105                     ~ 91505, # Burbank 6-digit typo
      q1_2_origin_text == "Washington D.C."     ~ 20007,
      q1_2_origin_text %in% c("90265", "90290") ~ as.numeric(q1_2_origin_text),
      str_detect(origin_lower, "bolinas")       ~ 94924,
      TRUE                                      ~ q2_zip_code
    ),

    # Pad numeric zip to 5-character string (handles leading 0 for 08400)
    zip_clean = if_else(
      !is.na(q2_zip_clean) & q2_zip_clean > 0 & q2_zip_clean <= 99950,
      str_pad(as.character(as.integer(q2_zip_clean)), width = 5, side = "left", pad = "0"),
      NA_character_
    )
  ) %>%
  left_join(po_zip_lookup, by = "zip_clean") %>%
  mutate(
    # Fallback state by USPS 2-digit prefix for unlisted / PO box ZIPs
    zip_prefix2 = str_sub(zip_clean, 1, 2),
    prefix_state = case_when(
      zip_prefix2 %in% c("90", "91", "92", "93", "94", "95", "96") ~ "CA",
      zip_prefix2 %in% c("89")                                       ~ "NV",
      zip_prefix2 %in% c("85", "86")                                 ~ "AZ",
      zip_prefix2 %in% c("97")                                       ~ "OR",
      zip_prefix2 %in% c("98", "99")                                 ~ "WA",
      zip_prefix2 %in% c("07", "08")                                 ~ "NJ",
      TRUE                                                           ~ NA_character_
    ),

    # Case-insensitive 2-letter state code extractor (e.g., "Portland, Or" -> "OR")
    home_state_abbrev = if_else(
      !is_refusal,
      str_to_upper(str_extract(home_other_raw, "(?i)(?<=,\\s?)[A-Z]{2}\\b|\\b[A-Z]{2}$")),
      NA_character_
    ),

    # Full state names from home_other
    home_state_named = if_else(!is_refusal, state_name_map[home_other_low], NA_character_),

    # City matching (substring search so "Silicon Valley..." matches "silicon valley")
    home_city_matched = if_else(
      !is_refusal,
      city_state_map[str_extract(home_other_low, paste(names(city_state_map), collapse = "|"))],
      NA_character_
    ),
    origin_city_matched = city_state_map[str_extract(origin_lower, paste(names(city_state_map), collapse = "|"))],
    origin_state_abbrev = str_extract(origin_clean, "\\b[A-Z]{2}\\b"),
    origin_as_zip_state = po_zip_lookup$db_state[match(origin_clean, po_zip_lookup$zip_clean)],

    # International determination
    international = case_when(
      is_unhoused ~ "domestic",
      !is.na(q2_other_country) & str_trim(q2_other_country) != "" ~ "international",
      TRUE ~ "domestic"
    ),

    # -----------------------------------------------------
    # Resolve State (Cascading Priority)
    # -----------------------------------------------------
    state_resolved = case_when(
      international == "international" ~ NA_character_,
      is_unhoused                      ~ NA_character_,
      !is.na(db_state)                 ~ db_state,
      !is.na(prefix_state)             ~ prefix_state,
      !is.na(home_state_abbrev)        ~ home_state_abbrev,
      !is.na(home_state_named)         ~ unname(home_state_named),
      !is.na(home_city_matched)        ~ unname(home_city_matched),
      # Fallback to origin info if home was declined/empty:
      !is.na(origin_as_zip_state)      ~ origin_as_zip_state,
      !is.na(origin_city_matched)      ~ unname(origin_city_matched),
      !is.na(origin_state_abbrev)      ~ origin_state_abbrev,
      TRUE                             ~ NA_character_
    ),

    # Residence classification
    residence = case_when(
      is_unhoused                                         ~ "unhoused",
      international == "international"                    ~ "international",
      toupper(state_resolved) %in% c("CA", "CALIFORNIA")  ~ "california",
      !is.na(state_resolved)                              ~ "state_other",
      is_refusal                                          ~ "declined_or_unknown",
      TRUE                                                ~ NA_character_
    ),

    # Residence detail
    residence_detail = case_when(
      is_unhoused                      ~ "unhoused",
      international == "international" ~ as.character(q2_other_country),
      !is.na(state_resolved)           ~ state_resolved,
      is_refusal                       ~ "declined_or_unknown",
      TRUE                             ~ NA_character_
    )
  ) %>%
  select( #, origin_lower, is_unhoused, q2_zip_clean, 
    -zip_clean, -db_state, -zip_prefix2, -prefix_state,
    -home_other_raw, -home_other_low,  -is_refusal,
    -extracted_home_zip, -home_state_abbrev,
    -home_state_named, -home_city_matched, -origin_city_matched,
    -origin_state_abbrev, -origin_as_zip_state
  )%>%
  glimpse()

# ---------------------------------------------------------
# 4. Verification Check
# ---------------------------------------------------------
d2 %>%
  select(q1_origin, q1_2_origin_text, q2_zip_code, international, state_resolved, residence, residence_detail) %>%
  arrange(residence_detail) %>%
  print(n = 20)

d2%>%
  select(q1_origin:q2_other_country,q2_home_other:residence_detail)%>%
  arrange(residence_detail)%>%
  filter(is.na(residence_detail) & !is.na(q2_home_other))%>%
  print(n = 50)


write_csv(d2,"./results/mwis_p2_clean3.csv")

