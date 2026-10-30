# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa value source - clean responses - not done yet!
# -----------------

library(tidyverse)
library(stringr)
library(purrr)
library(tidytext)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean7.csv")%>%
   glimpse()

unique(d1$q11_mpa_value_source)
d1$q11_mpa_value_source[1001:1367]

#Gemini: next question is for the variable q11_mpa_value_source. its a write in so I need to think about how to classify responses. what do you suggest? here are responses. I also want to clean formatting errors.

# need to work with these classifications!

# -----------------------------------------------------------------
# 1. Clean Text 
# -----------------------------------------------------------------
f_clean_mpa_text <- function(x) {
  x_clean <- x %>%
    str_replace_all("[\r\n]+", " ") %>%
    str_replace_all("[\"“”'’]", "") %>%
    str_replace_all("\\s{2,}", " ") %>%
    str_trim()
  
  # NA replacement for standard non-content entries
  na_patterns <- "^(?i)(na|n/a|none|null|\\.|-)$"
  x_clean[str_detect(x_clean, na_patterns)] <- NA_character_
  x_clean
}

# -----------------------------------------------------------------
# 2. Categorize
# -----------------------------------------------------------------
f_classify_mpa_values <- function(txt_vec) {
  t <- tolower(txt_vec)
  
  non_response_pattern <- "^(declined?|pass|nothing|no|dont know|dont have|not sure|n/a|na|unsure|unclear|not interested|none|blasé|blase|low priority|other issues seem more important)"
  
  # Anti-regulation / negative sentiment regex
  anti_reg_pattern <- paste0(
    "\\b(too restrict|less restrict|overprotect|overregulat|overhyped|distracts|unnecessary|",
    "not big on regulat|policing and jails|privatize|unnecessary interference|",
    "prevents fishing|takes away from fishing|limits on fishing|limit fishing|conflict of interest|",
    "balance the needs of humans|do not want fishing taken away|too much management)\\b"
  )
  
  # Neutral/Positive Fishing & Food regex
  fishing_food_pattern <- "\\b(seafood|fisher|fisheries|fishing|food|commercial|tacos?|fishermen)\\b"

  # Category Regex Dictionary
  themes <- list(
    # --- NEW THEMES ---
    "Intrinsic Value of Nature"      = "\\b(was here before|right to exist|creation|god's creation|sacred|nature needs|not home for us|nature itself|intrinsic|non-human|untouched)\\w*",
    "Local Identity & Personal Roots" = "\\b(grew up|born and raised|lived here|local|beach people|my home|in my dna|my life|raised by|entire life|living here)\\w*",
    "Love of Ocean & Nature"         = "\\b(love the ocean|love nature|love animals|love the beach|love the coast|ocean lover|passionate about|happy place|favorite place|love going|love exploring|love it)\\w*",
    
    # --- EXISTING THEMES ---
    "Tribal & Cultural Values"       = "\\b(trib|tek|indigenous|ancest)\\w*",
    "Climate Change Mitigation"     = "\\b(climate|global warming|carbon|co2|absorbs heat|sea level rise|erosion|flooding)\\b",
    "Clean/Unpolluted Environment"  = "\\b(clean|trash|plastic|litter|pollut|oil|spill|debris|sewage|waste|tar|runoff)\\w*",
    "Biodiversity & Marine Life"    = "\\b(wildlife|animal|species|biodivers|sea life|marine life|creature|bird|seal|dolphin|lobster|plover|coral|extinct|tide pool|nurser)\\w*",
    "Ecosystem Health"              = "\\b(ecosystem|balance|lungs of|filter|interconnect|vital|habitat|delicate|nature intended|circle of life|kelp|oxygen)\\w*",
    "Future Generations"            = "\\b(future generation|generations|children|kids|grandchild|grandchildren|next generation|later generation|years to come)\\b",
    "Economy & Tourism"             = "\\b(economy|jobs|tourism|economic)\\b",
    "Human Impact"                  = "\\b(overdevelop|development|human impact|tragedy of the commons|exploit|destroy|overfished|extinct|people messing|decrease of birds|little nature|disrespectful|people are stupid|overharvest|over-harvest|dwindl|threat|encroach|damag|overuse|abuse)\\w*",
    "Governance & Regulation"       = "\\b(regulat|enforc|rules?|governance|management|monitor|mpas?|restrictions?|laws?|legislat)\\w*",
    "Recreation"                    = "\\b(scuba|surf|surfer|swim|swimming|sailing|kayak|beachgoer|leisure|divers?)\\w*",
    "Wellness, Beauty & Spirituality"= "\\b(enjoy|relax|mental health|calm|beauty|scenery|sacred|god|spiritual|therapy|soul|peace|beautiful)\\w*",
    "Relational Values & Stewardship"= "\\b(honor|common sense|steward|stewardship|relationship|obligation|responsibility|protect|conserve|preserve|need to help|take care|connection|sustain us|have to save|feel committed|wellbeing|ecosystem drives the rest of us|it keeps us safe|ocean supports us|ethically|empath|essential services)\\w*",
    "Successful Management"         = "\\b(success|bounce back|worked|drastically improved|higher abundance|protects? wildlife|overfished without|started mpas earlier)\\w*"
  )
  
  purrr::map_chr(seq_along(txt_vec), function(i) {
    curr <- t[i]
    if (is.na(curr) || str_trim(curr) == "") return(NA_character_)
    
    # 1. Handle pure non-responses
    if (str_detect(curr, non_response_pattern) && !str_detect(curr, "because|so|but|important|protect|love")) {
      return("Unsure/Non-response")
    }
    
    matched_tags <- character(0)
    
    # 2. Check anti-regulation FIRST
    if (str_detect(curr, anti_reg_pattern)) {
      matched_tags <- c(matched_tags, "Skeptical/Anti-Regulation")
    }
    
    # 3. Check Food & Fishing ONLY if phrase doesn't contain anti-regulation sentiments
    if (str_detect(curr, fishing_food_pattern) && !str_detect(curr, anti_reg_pattern)) {
      matched_tags <- c(matched_tags, "Food & Fishing")
    }
    
    # 4. Check remaining thematic categories
    for (theme_name in names(themes)) {
      if (str_detect(curr, themes[[theme_name]])) {
        matched_tags <- c(matched_tags, theme_name)
      }
    }
    
    matched_tags <- unique(matched_tags)
    
    if (length(matched_tags) == 0) {
      return("General Conservation")
    }
    
    paste(matched_tags, collapse = "; ")
  })
}


# -----------------------------------------------------------------
# 3. Run functions
# -----------------------------------------------------------------
d2 <- d1 %>%
  mutate(
    q11_mpa_value2   = f_clean_mpa_text(q11_mpa_value_source),
    q11_mpa_value3  = f_classify_mpa_values(q11_mpa_value2)
  )


# checking ------------
d2 %>%
  select(q11_mpa_value2, q11_mpa_value3) %>%
  filter(!is.na(q11_mpa_value3)) %>%
  # head(25)
  view()

# check unclassified comments
# Filter for unclassified / fallback comments
unclassified_comments <- d3 %>%
  filter(q11_mpa_value_source3 == "General Conservation") %>%
  select(q11_mpa_value_source2) %>%
  distinct()

# View total count and top examples
cat("Number of unique comments in 'General Conservation':", nrow(unclassified_comments), "\n\n")

# Preview the first 30 comments
head(unclassified_comments, 30)

# checking ------------
# Find top frequent words in the General Conservation category
unclassified_comments %>%
  unnest_tokens(word, q11_mpa_value_source2) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  head(20)



# save-------------
# write_csv(d3,"./results/mwis_p2_clean8.csv")