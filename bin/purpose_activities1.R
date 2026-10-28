# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: clean activities
# ------------------

library(tidyverse)
library(stringr)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()

# -------------------------------------------------------------------------
# 1. Standardized Activity Label Lookup Mapping
# -------------------------------------------------------------------------
activity_labels <- c(
  "Fishing or collecting food"                        = "Fishing/Collecting Food",
  "Group/Family gatherings or activities"             = "Family/Group Activities",
  "Meditation/Reading/Relaxing/Art"                   = "Meditation/Relaxing/Art/Health",
  "Swimming/Bodysurfing"                              = "Swimming/Bodysurfing",
  "Festivals"                                         = "Festivals",
  "Bicycling/Roller skating/Skateboarding"            = "Cycling/Skating",
  "Paid work"                                         = "Paid Work",
  "Beach games/Sports/Yoga"                           = "Beach Games/Sports/Yoga",
  "Walking/Running/Hiking"                            = "Walking/Running/Hiking",
  "Enjoying views/sunsets"                            = "Enjoying Views/Sunsets",
  "Surfing"                                           = "Surfing",
  "Cultural or religious ceremonies"                  = "Cultural/Religious Ceremonies",
  "Nature Observing/Photographing/Education/Research" = "Nature Observation",
  "Paddleboarding/Kiteboarding/Kayaking/Canoeing"     = "Paddle/Kayak/Canoe",
  "Snorkeling/Scuba Diving"                           = "Snorkeling/Scuba",
  "Volunteering"                                      = "Volunteering",
  "Sailing/Boating"                                   = "Sailing/Boating",
  "Another activity"                                  = "Another Activity"
)

# -------------------------------------------------------------------------
# 2. Write-in Mapping (Allows Multiple Categories per Entry)
# -------------------------------------------------------------------------
text_to_category_multi <- c(
  # Walking/Running/Hiking & Dogs
  "WALKING PETS"                                = "Walking/Running/Hiking",
  "walking pets"                                = "Walking/Running/Hiking",
  "Walking my dogs"                             = "Walking/Running/Hiking",
  "Walking dog"                                 = "Walking/Running/Hiking",
  "dog"                                         = "Walking/Running/Hiking",
  "Walk dogs, exercise"                         = "Walking/Running/Hiking",
  "exercise/walk dog"                           = "Walking/Running/Hiking",
  "Hiking"                                      = "Walking/Running/Hiking",
  "hiking"                                      = "Walking/Running/Hiking",
  "hiking and biking"                           = "Walking/Running/Hiking,Cycling/Skating",
  "Hiking, getting lunch (after hike)"          = "Walking/Running/Hiking,Family/Group Activities",
  "exercise"                                    = "Walking/Running/Hiking",
  
  # Nature Observation
  "Birding"                                     = "Nature Observation",
  "tidepools"                                   = "Nature Observation",
  "Tidepools"                                   = "Nature Observation",
  "tidepooling"                                 = "Nature Observation",
  "Tidepooling"                                 = "Nature Observation",
  "Humpback whale watching, therapy"            = "Nature Observation,Meditation/Relaxing/Art/Health",
  "Nature"                                      = "Nature Observation",
  "Enjoy nature"                                = "Nature Observation",
  "Be outside in nature"                        = "Nature Observation",
  "Looking for Agates (not collecting)"         = "Nature Observation",
  "beach combing"                               = "Nature Observation",
  "collecting shells"                           = "Nature Observation",
  "research"                                    = "Nature Observation",
  "Learning"                                    = "Nature Observation",
  "school"                                      = "Nature Observation",
  "Photos"                                      = "Nature Observation",
  
  # Family/Group Activities & Celebrations & Dining
  "Family"                                      = "Family/Group Activities",
  "Enjoying nature with kids/fam"               = "Family/Group Activities,Nature Observation",
  "Kid Activity"                                = "Family/Group Activities",
  "kid playing"                                 = "Family/Group Activities",
  "Playing"                                     = "Family/Group Activities",
  "Picking up grandson in volleyball camp"      = "Family/Group Activities",
  "hanging out"                                 = "Family/Group Activities",
  "Just hanging out"                            = "Family/Group Activities",
  "social"                                      = "Family/Group Activities",
  "visiting"                                    = "Family/Group Activities",
  "Visiting"                                    = "Family/Group Activities",
  "visiting friends"                            = "Family/Group Activities",
  "Friend Visiting"                             = "Family/Group Activities",
  "Visit"                                       = "Family/Group Activities",
  "Birthday"                                    = "Family/Group Activities",
  "birthday"                                    = "Family/Group Activities",
  "Celebrating Birthday"                        = "Family/Group Activities",
  "Celebrating a birthday"                      = "Family/Group Activities",
  "Celebrating Bday"                            = "Family/Group Activities",
  "Individuals birthday"                        = "Family/Group Activities",
  "celebration"                                 = "Family/Group Activities",
  "celebrating anniversary, looking at pretty views" = "Family/Group Activities,Enjoying Views/Sunsets",
  "Anniversary, eating on the beach"            = "Family/Group Activities",
  "Vacation"                                    = "Family/Group Activities",
  "vacation"                                    = "Family/Group Activities",
  "vacationing"                                 = "Family/Group Activities",
  "on vacation"                                 = "Family/Group Activities",
  "Summer"                                      = "Family/Group Activities",
  "Labor day weekend"                           = "Family/Group Activities",
  "Long weekend"                                = "Family/Group Activities",
  "Father's Day weekend"                        = "Family/Group Activities",
  "Fun"                                         = "Family/Group Activities",
  "fun"                                         = "Family/Group Activities",
  "PLEASURE"                                    = "Family/Group Activities",
  "camping"                                     = "Family/Group Activities",
  "Camping"                                     = "Family/Group Activities",
  "camping, enjoy oceans"                       = "Family/Group Activities,Enjoying Views/Sunsets",
  "Eat at a restaurant"                         = "Family/Group Activities",
  "Lunch"                                       = "Family/Group Activities",
  "eating"                                      = "Family/Group Activities",
  "coffee"                                      = "Family/Group Activities",
  "Bear Flag Poke"                              = "Family/Group Activities",
  "breakfast with a view"                       = "Enjoying Views/Sunsets,Family/Group Activities",
  
  # Meditation/Relaxing/Art/Health
  "Relaxing"                                    = "Meditation/Relaxing/Art/Health",
  "tanning"                                     = "Meditation/Relaxing/Art/Health",
  "Tanning"                                     = "Meditation/Relaxing/Art/Health",
  "Distracting my mind"                         = "Meditation/Relaxing/Art/Health",
  "physical/ mental health, physical therapy, trash collection" = "Meditation/Relaxing/Art/Health,Volunteering",
  "therapy"                                     = "Meditation/Relaxing/Art/Health",
  "\"Breathing in the planet\""                 = "Meditation/Relaxing/Art/Health",
  "cool down"                                   = "Meditation/Relaxing/Art/Health",
  "living a life on the beach"                  = "Meditation/Relaxing/Art/Health",
  "picnic/ get away from civilization. Nature is nothingness compared to human development. Going back to nature" = "Family/Group Activities,Meditation/Relaxing/Art/Health",
  "painting with club"                          = "Meditation/Relaxing/Art/Health",
  "painting class"                              = "Meditation/Relaxing/Art/Health",
  
  # Enjoying Views/Sunsets
  "Watching sunset"                             = "Enjoying Views/Sunsets",
  "Watching sunset on beach"                    = "Enjoying Views/Sunsets",
  "Enjoying beach"                              = "Enjoying Views/Sunsets",
  "seeing ocean"                                = "Enjoying Views/Sunsets",
  "nice weather"                                = "Enjoying Views/Sunsets",
  "hot and enjoying weather"                    = "Enjoying Views/Sunsets",
  "Passing by, got out to see view"             = "Enjoying Views/Sunsets",
  
  # Beach Games/Sports/Yoga
  "Yoga"                                        = "Beach Games/Sports/Yoga",
  "Yoga, stretch, guitar"                       = "Beach Games/Sports/Yoga,Meditation/Relaxing/Art/Health",
  "Pickleball"                                  = "Beach Games/Sports/Yoga",
  "dancing"                                     = "Beach Games/Sports/Yoga",
  "Junior lifeguards tournament"                = "Beach Games/Sports/Yoga",
  
  # Volunteering
  "Beach cleanups"                              = "Volunteering",
  "cleaning the beach (individually)"           = "Volunteering",
  "Metal Detecting (for safety of visitors)"    = "Volunteering",
  "Help out the elderly"                        = "Volunteering",
  "Not littering"                               = "Volunteering",
  
  # Paddle/Kayak/Canoe
  "Kayaking"                                    = "Paddle/Kayak/Canoe",
  "kitesurfing"                                 = "Paddle/Kayak/Canoe",
  
  # Festivals
  "Terranea Resort Food and Wine Festival"      = "Festivals",
  "Morgan Horse Show"                           = "Festivals",
  
  # Cultural/Religious Ceremonies
  "ceremony"                                    = "Cultural/Religious Ceremonies",
  "mystical communication"                      = "Cultural/Religious Ceremonies",
  
  # Paid Work
  "Coffee Importer"                             = "Paid Work",
  "Business+Pleasure"                           = "Paid Work,Family/Group Activities",
  
  # Cycling/Skating
  "Biking"                                      = "Cycling/Skating",
  
  # Another Activity
  "Transitioning Moving"                        = "Another Activity",
  "Transitioning moving"                        = "Another Activity",
  "on the way home"                             = "Another Activity"
)

# -------------------------------------------------------------------------
# 3. Text Pre-Processing Function for Menu Choices
# -------------------------------------------------------------------------
clean_raw_activities <- function(x) {
  x %>%
    str_replace_all("Bicycling,\\s*roller skating,\\s*skateboarding,\\s*etc\\.?", "Bicycling/Roller skating/Skateboarding") %>%
    str_replace_all("Relaxing,\\s*reading,\\s*sun-bathing,\\s*meditation", "Meditation/Reading/Relaxing/Art") %>%
    str_replace_all("Observing or photographing nature or wildlife,\\s*outdoor education", "Nature Observing/Photographing/Education/Research") %>%
    str_replace_all("Festivals with music and(/or)? food(\\s*\\(e\\.g\\., earth day\\))?", "Festivals") %>%
    str_replace_all("Beach games or sports(\\s*\\(e\\.g\\., frisbee, volleyball, yoga\\))?", "Beach games/Sports/Yoga") %>%
    str_replace_all("Group or family gatherings or activities(\\s*\\(e\\.g\\., family outing, bbq\\))?", "Group/Family gatherings or activities") %>%
    str_replace_all("Volunteering(\\s*\\(e\\.g\\., beach clean-ups\\))?", "Volunteering") %>%
    str_replace_all("Sailing/Boating(\\s*\\(engine powered\\))?", "Sailing/Boating") %>%
    str_replace_all("Stand up paddleboarding/kite or sailboarding/kayaking", "Paddleboarding/Kiteboarding/Kayaking/Canoeing") %>%
    str_replace_all("Cultural or religious practices or ceremonies", "Cultural or religious ceremonies") %>%
    str_replace_all("Driving or sitting in your car to enjoy the views/sunsets", "Enjoying views/sunsets") %>%
    str_replace_all("Walking or running", "Walking/Running/Hiking") %>%
    str_replace_all("Swimming or bodysurfing", "Swimming/Bodysurfing") %>%
    str_replace_all("Fishing or collecting food\\s+", "Fishing or collecting food") %>%
    str_replace_all("Other,\\s*please specify:?", "Another activity") %>%
    str_replace_all("None of the above", "Another activity") %>%
    str_replace_all("\\s*\\([^)]*\\)", "") %>%
    str_trim()
}

# -------------------------------------------------------------------------
# 4. Helper Function: Combine Standard Selections & Categorized Write-Ins
# -------------------------------------------------------------------------
merge_purpose_categories <- function(raw_purpose, raw_text, lookup_map, labels_map) {
  # Clean base choice
  cleaned_base <- clean_raw_activities(raw_purpose)
  
  # Map text write-in
  trimmed_text <- str_trim(raw_text)
  mapped_text <- unname(lookup_map[trimmed_text])
  
  # Vectorized processing row by row
  purrr::map2_chr(cleaned_base, mapped_text, function(base, text_cats) {
    if (is.na(base) && is.na(text_cats)) return(NA_character_)
    
    # 1. Parse base categories
    base_vec <- character(0)
    if (!is.na(base) && base != "") {
      raw_items <- str_split(base, ",")[[1]] %>% str_trim()
      base_vec <- unname(labels_map[raw_items])
      # Fallback to item itself if already standardized
      base_vec[is.na(base_vec)] <- raw_items[is.na(base_vec)]
    }
    
    # 2. Parse mapped write-in categories
    text_vec <- character(0)
    if (!is.na(text_cats) && text_cats != "") {
      text_vec <- str_split(text_cats, ",")[[1]] %>% str_trim()
    }
    
    # 3. Combine both
    all_cats <- c(base_vec, text_vec)
    
    # 4. If a substantive category was added from write-in, remove the placeholder "Another Activity"
    has_substantive <- any(!all_cats %in% c("Another Activity", "Another activity", ""))
    if (has_substantive) {
      all_cats <- all_cats[!all_cats %in% c("Another Activity", "Another activity")]
    }
    
    # 5. Deduplicate and collapse
    unique_cats <- unique(all_cats[all_cats != "" & !is.na(all_cats)])
    
    if (length(unique_cats) == 0) return(NA_character_)
    paste(unique_cats, collapse = ",")
  })
}

# -------------------------------------------------------------------------
# 5. Build Combined Columns & Clean Dataset
# -------------------------------------------------------------------------
d2 <- d1 %>%
  mutate(
    # Clean write-in text classification columns
    q3_purpose_text_category = unname(text_to_category_multi[str_trim(q3_purpose_text)]),
    q4_purpose_most_important_text_category = if ("q4_purpose_most_important_text" %in% names(.)) {
      unname(text_to_category_multi[str_trim(q4_purpose_most_important_text)])
    } else {
      NA_character_
    },
    
    # Unified, categorized combinations
    q3_purpose2 = merge_purpose_categories(
      raw_purpose = q3_purpose,
      raw_text    = q3_purpose_text,
      lookup_map  = text_to_category_multi,
      labels_map  = activity_labels
    ),
    
    q4_purpose_most_important2 = if ("q4_purpose_most_important_text" %in% names(.)) {
      merge_purpose_categories(
        raw_purpose = q4_purpose_most_important,
        raw_text    = q4_purpose_most_important_text,
        lookup_map  = text_to_category_multi,
        labels_map  = activity_labels
      )
    } else {
      merge_purpose_categories(
        raw_purpose = q4_purpose_most_important,
        raw_text    = NA_character_,
        lookup_map  = text_to_category_multi,
        labels_map  = activity_labels
      )
    }
  )

# Verify updated columns
d2 %>%
  select(q3_purpose, q3_purpose_text, q3_purpose2) %>%
  filter(!is.na(q3_purpose_text)) %>%
  head(15)

# -------------------------------------------------------------------------
# 6. Save the Enriched Dataset
# -------------------------------------------------------------------------
write_csv(d2, "./results/mwis_p2_clean5.csv")
