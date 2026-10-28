# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : to categorize and graph non-responses

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)
library(colorspace)


# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

d1<-read_csv("./results/mwis_p2_clean3_maindata_non_responses.csv")%>%
  glimpse()

unique(d1$q1_3_non_response_reason)

# create categories for refusals --------------

# Function to categorize non-response reasons 
# this list developed interactively with gemini from full list - will need to update codes for new data
categorize_reason <- function(reason) {
  if (is.na(reason)) return("No Reason Provided")
  
  # Normalize text for matching
  r <- str_to_lower(trimws(reason))
  
  case_when(
    # No Reason Provided (Data artifacts, system strings, missing responses)
    r %in% c("n/a", "\"n/a\"", "na", "none", "no reason", "no reason given", "none given", "810627", "no") | 
      str_detect(r, "^replace with previous") ~ "No Reason Provided",
    
    # Already Took Survey
    str_detect(r, "already (completed|participated|done)|yesterday|repeat") ~ "Previously Participated",
    
    # Survey Logistics (Length, eligibility, privacy)
    str_detect(r, "under 18|not 18|too young|minor") |
      str_detect(r, "5 minute|too long|length of survey|confidentiality|demographic|privacy|not allowed|knowledgable") ~ "Survey Logistics",
    
    # Language Barrier / English Proficiency
    str_detect(r, "english|language|spanish|spansih|italian|french|german|translate") ~ "Language Barrier",
    
    # Childcare & Family Responsibilities
    str_detect(r, "kid|child|daughter|son|family|honeymoon|tantrum") ~ "Family Responsibilities",
    
    # Active Activities
    str_detect(r, "surf|swim|water|exercise|workout|walk|photo|bird|falcon|tidepool|skimboard|football|dog|podcast|meditat") ~ "Busy with Activity",
    
    # Out-of-Town / Tourist / Non-Local
    str_detect(r, "tourist|visiting|out of town|not from|not living|vacation") ~ "Tourist",
    
    # Relaxing
    str_detect(r, "relax|chill|sunbath|read|enjoy|unwind|alone time|beach time") ~ "Relaxing",
    
    # Other Activities (Eating, work, phone calls, etc.)
    str_detect(r, "eat|phone|work|study|game|call|hear|injured|headphones|special needs") ~ "Other Activities/Situation",
    
    # Time Constraints / In a Rush
    str_detect(r, "time|rush|hurry|busy|leave|leaving|en route|somewhere to be|move") ~ "Time",
    
    # General Refusal / Lack of Interest
    str_detect(r, "not interest|don't want|didn't want|heart said no|no thank|ignored") ~ "General Refusal",
    
    # Default fallback
    TRUE ~ "General Refusal"
  )
}

# Apply to your dataframe (d4_no)
d2 <- d1%>%
  mutate(reason_category = sapply(q1_3_non_response_reason, categorize_reason))%>%
  glimpse()

# Check distribution across categories
d3<-table(d2$reason_category) %>% 
  as_tibble(.name_repair = ~ c("reason_category", "count"))

# graph -------------
ggplot(d2, aes(x = fct_infreq(reason_category), fill = reason_category)) +  geom_bar(show.legend = FALSE) +
  scale_fill_discrete_sequential(palette = "Viridis") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Survey Non-Response Reasons",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 14, hjust = 0.5, vjust = 0.5, color = "black"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave("./doc/survey_non_response.png", width = 10, height = 7, dpi = 300)

# save
write_csv(d2,"./results/mwis_p2_clean3_maindata_non_responses2.csv")
write_csv(d3,"./doc/non_response_counts.csv")
