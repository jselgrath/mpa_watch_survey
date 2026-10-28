# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: graph languages and languages spoken in non english households
# ------------------

library(tidyverse)
library(colorspace)
library(viridis)

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()


# =========================================================================
# FUNCTION 1: Plot All Languages (Including English)
# =========================================================================
plot_all_languages <- function(d, subset.name = "all_data") {
  
  todaydate <- format(Sys.Date(), "%Y%m%d")
  
  # 1. Filter valid responses
  d_valid <- d %>%
    filter(
      !is.na(q15_language),
      str_trim(q15_language) != "",
      !str_detect(q15_language, regex("declined", ignore_case = TRUE))
    )
  
  n_total <- nrow(d_valid)
  
  # 2. Unnest, trim, recode dialect variants, and remove "Other"
  d_langs <- d_valid %>%
    select(response_id, q15_language) %>%
    separate_rows(q15_language, sep = ",") %>%
    mutate(
      Language = str_replace_all(q15_language, "\u00A0", " ") %>%
        str_squish(),
      Language = recode(
        Language,
        "Malay (Malaysian)"   = "Malay",
        "Malay (Malyasian)"   = "Malay",
        "Romanian (Moldavan)" = "Romanian",
        "Romanian (Moldovan)" = "Romanian",
        "Farsi (Persian)"     = "Farsi"
      )
    ) %>%
    filter(Language != "", Language != "Other")
  
  # 3. Count and order: Count (ascending), ties sorted A->Z (descending for coord_flip)
  langs_all <- d_langs %>%
    count(Language, name = "Count") %>%
    arrange(Count, desc(Language)) %>%
    mutate(Language = fct_inorder(Language))
  
  # 4. Generate Plot
  plt_all <- ggplot(langs_all, aes(x = Language, y = Count)) +
    geom_bar(stat = "identity", fill = viridis(3, option = "mako")[2]) +
    geom_text(
      aes(label = Count),
      hjust = -0.2,
      size = 3.5,
      color = "grey20"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0("What language do you speak at home? (N = ", n_total, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, face = "bold", color = "black", hjust = 1),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # 5. Save Plot
  out_filename <- paste0("./doc/languages_", subset.name, ".png")
  ggsave(
    filename = out_filename,
    plot = plt_all,
    units = "in",
    width = 4.5,
    height = max(4, nrow(langs_all) * 0.35 + 1.5),
    dpi = 300
  )
  
  return(plt_all)
}


# =========================================================================
# FUNCTION 2: Plot Non-English Household Languages
# =========================================================================
plot_non_english_languages <- function(d, subset.name = "all_data") {
  
  todaydate <- format(Sys.Date(), "%Y%m%d")
  
  # 1. Filter valid responses
  d_valid <- d %>%
    filter(
      !is.na(q15_language),
      str_trim(q15_language) != "",
      !str_detect(q15_language, regex("declined", ignore_case = TRUE))
    )
  
  # 2. Unnest, trim, recode, and drop English & Other
  d_langs <- d_valid %>%
    select(response_id, q15_language) %>%
    separate_rows(q15_language, sep = ",") %>%
    mutate(
      Language = str_replace_all(q15_language, "\u00A0", " ") %>%
        str_squish(),
      Language = recode(
        Language,
        "Malay (Malaysian)"   = "Malay",
        "Malay (Malyasian)"   = "Malay",
        "Romanian (Moldavan)" = "Romanian",
        "Romanian (Moldovan)" = "Romanian",
        "Farsi (Persian)"     = "Farsi"
      )
    ) %>%
    filter(
      Language != "", 
      Language != "Other",
      !str_detect(Language, regex("^english$", ignore_case = TRUE))
    )
  
  # Return early if no non-English responses exist
  if (nrow(d_langs) == 0) {
    message("No non-English responses found for subset: ", subset.name)
    return(NULL)
  }
  
  # Number of participants who selected at least one non-English language
  n_non_eng_respondents <- d_langs %>%
    distinct(response_id) %>%
    nrow()
  
  # 3. Count and order: Count (ascending), ties sorted A->Z (descending for coord_flip)
  langs_non_eng <- d_langs %>%
    count(Language, name = "Count") %>%
    arrange(Count, desc(Language)) %>%
    mutate(Language = fct_inorder(Language))
  
  # 4. Generate Plot
  plt_non_eng <- ggplot(langs_non_eng, aes(x = Language, y = Count)) +
    geom_bar(stat = "identity", fill = viridis(3, option = "mako")[2]) +
    geom_text(
      aes(label = Count),
      hjust = -0.2,
      size = 3.5,
      color = "grey20"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0("Non-English-speaking\nhousehold languages (N = ", n_non_eng_respondents, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, face = "bold", color = "black", hjust = 1),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # 5. Save Plot
  out_filename <- paste0("./doc/language_non_english", subset.name, ".png")
  ggsave(
    filename = out_filename,
    plot = plt_non_eng,
    units = "in",
    width = 4.5,
    height = max(4, nrow(langs_non_eng) * 0.35 + 1.5),
    dpi = 300
  )
  
  return(plt_non_eng)
}



# =========================================================================
# FUNCTION: Non-English-Only Households (Excludes any English speakers)
# =========================================================================
plot_non_english_only_languages <- function(d, subset.name = "all_data") {
  
  todaydate <- format(Sys.Date(), "%Y%m%d")
  
  # 1. Filter to valid responses AND exclude ANY record containing "English"
  d_non_eng_only <- d %>%
    filter(
      !is.na(q15_language),
      str_trim(q15_language) != "",
      !str_detect(q15_language, regex("declined", ignore_case = TRUE)),
      # Key filter: Exclude if English is mentioned anywhere in the response
      !str_detect(q15_language, regex("english", ignore_case = TRUE))
    )
  
  # Number of respondents/households where NO English is spoken at home
  n_households <- nrow(d_non_eng_only)
  
  # Return early if no strictly non-English households exist
  if (n_households == 0) {
    message("No non-English-only households found for subset: ", subset.name)
    return(NULL)
  }
  
  # 2. Unnest, trim, recode, and drop "Other"
  d_langs <- d_non_eng_only %>%
    select(response_id, q15_language) %>%
    separate_rows(q15_language, sep = ",") %>%
    mutate(
      Language = str_replace_all(q15_language, "\u00A0", " ") %>%
        str_squish(),
      Language = recode(
        Language,
        "Malay (Malaysian)"   = "Malay",
        "Malay (Malyasian)"   = "Malay",
        "Romanian (Moldavan)" = "Romanian",
        "Romanian (Moldovan)" = "Romanian",
        "Farsi (Persian)"     = "Farsi"
      )
    ) %>%
    filter(Language != "", Language != "Other")
  
  # 3. Count and order: Count (ascending), ties sorted A->Z (descending for coord_flip)
  langs_summary <- d_langs %>%
    count(Language, name = "Count") %>%
    arrange(Count, desc(Language)) %>%
    mutate(Language = fct_inorder(Language))
  
  # 4. Generate Plot
  plt_non_eng_only <- ggplot(langs_summary, aes(x = Language, y = Count)) +
    geom_bar(stat = "identity", fill = viridis(3, option = "mako")[2]) +
    geom_text(
      aes(label = Count),
      hjust = -0.2,
      size = 3.5,
      color = "grey20"
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0("Households where English is not spoken\n(N = ", n_households, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9, face = "bold", color = "black", hjust = 1),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  # 5. Save Plot
  out_filename <- paste0("./doc/language_no_english_at_home_", subset.name, ".png")
  ggsave(
    filename = out_filename,
    plot = plt_non_eng_only,
    units = "in",
    width = 4.5,
    height = max(4, nrow(langs_summary) * 0.35 + 1.5),
    dpi = 300
  )
  
  return(plt_non_eng_only)
}


# =========================================================================
# RUN THE FUNCTIONS
# =========================================================================

# 1. Run for entire survey dataset
p_all     <- plot_all_languages(d1, subset.name = "all_data")
p_all

p_non_eng <- plot_non_english_languages(d1, subset.name = "all_data")
p_non_eng

p_no_english <- plot_non_english_only_languages(d1, subset.name = "all_data")
p_no_english
