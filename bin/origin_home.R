# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: graph origin data
# ------------------

library(tidyverse)
library(colorspace)
library(viridis)
library(scales)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")



# survey data ------
d1 <- read_csv("./results/mwis_p2_clean4_maindata_responses.csv") %>%
  glimpse()


# FUNCTION -----------------------------
summarize.origin.location <- function(d, subset.name = "all_data", local.zips = NULL) {
  
  # =========================================================================
  # PART 1: Day Origin (Home vs Another Location) - Vertical Bars
  # =========================================================================
  d_origin <- d %>%
    filter(!is.na(q1_origin), str_trim(q1_origin) != "") %>%
    mutate(
      origin = case_when(
        str_detect(q1_origin, regex("^home", ignore_case = TRUE)) ~ "Home",
        TRUE ~ "Another location"
      )
    )
  
  n_origin <- nrow(d_origin)
  
  if (n_origin > 0) {
    fixed_origin_order <- c("Home", "Another location")
    
    origin_summary <- d_origin %>%
      count(origin, name = "Count") %>%
      complete(origin = factor(fixed_origin_order, levels = fixed_origin_order), fill = list(Count = 0)) %>%
      mutate(
        prop = Count / sum(Count),
        label = paste0(comma(Count), "\n(", percent(prop, accuracy = 0.1), ")"),
        origin = factor(origin, levels = fixed_origin_order)
      )
    
    plt_origin_bar <- ggplot(origin_summary, aes(x = origin, y = Count, fill = origin)) +
      geom_bar(stat = "identity", width = 0.55) +
      geom_text(
        aes(label = label),
        vjust = -0.3,
        size = 3.5,
        lineheight = 0.9,
        color = "grey20"
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.22)),
        labels = comma
      ) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_viridis_d(option = "mako", begin = 0.4, end = 0.75) +
      coord_cartesian(clip = "off") +
      labs(
        title = paste0("Visit origin (N = ", comma(n_origin), ")"),
        x = NULL,
        y = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold", margin = margin(b = 12)),
        axis.text.x = element_text(size = 12,  color = "black"),
        axis.text.y = element_text(size = 12, color = "grey30"),
        axis.title.y = element_text(size = 14, color = "grey30", margin = margin(r = 6)),
        axis.ticks.y = element_line(color = "grey80"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(15, 12, 10, 10)
      )
    
    ggsave(
      filename = paste0("./doc/origin_", subset.name, ".png"),
      plot = plt_origin_bar,
      units = "in",
      width = 4.8,
      height = 3.4,
      dpi = 300
    )
  }
  
  # =========================================================================
  # PART 2: Where is Home? (California, Other States, International) - Vertical Bars
  # =========================================================================
  d_loc <- d %>%
    mutate(
      loc = case_when(
        # 1. International
        str_detect(international, regex("international", ignore_case = TRUE)) |
          !is.na(q2_other_country) ~ "International",
        
        # 2. California (includes local county and any other CA responses)
        (!is.null(local.zips) & (q2_zip_clean %in% as.numeric(local.zips))) |
          (q2_zip_clean >= 90001 & q2_zip_clean <= 96162) |
          str_detect(residence, regex("california", ignore_case = TRUE)) ~ "California",
        
        # 3. Other US States
        !is.na(q2_zip_clean) |
          str_detect(residence, regex("state_other", ignore_case = TRUE)) ~ "Other States",
        
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(loc))
  
  n_loc <- nrow(d_loc)
  cat("\n=== WHERE IS HOME BREAKDOWN (", subset.name, ") ===\n", sep = "")
  print(table(d_loc$loc))
  
  if (n_loc > 0) {
    # Fixed left-to-right hierarchy on the horizontal x-axis
    fixed_order <- c("California", "Other States", "International")
    
    home_summary <- d_loc %>%
      count(loc, name = "Count") %>%
      complete(loc = factor(fixed_order, levels = fixed_order), fill = list(Count = 0)) %>%
      mutate(
        prop = Count / sum(Count),
        label = paste0(comma(Count), "\n(", percent(prop, accuracy = 0.1), ")"),
        loc = factor(loc, levels = fixed_order)
      )
    
    plt_home_bar <- ggplot(home_summary, aes(x = loc, y = Count, fill = loc)) +
      geom_bar(stat = "identity", width = 0.55) +
      geom_text(
        aes(label = label),
        vjust = -0.3,
        size = 3.5,
        lineheight = 0.9,
        color = "grey20"
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.22)),
        labels = comma
      ) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_viridis_d(option = "mako", begin = 0.35, end = 0.85, direction = -1) +
      coord_cartesian(clip = "off") +
      labs(
        title = paste0("Where is home? (N = ", comma(n_loc), ")"),
        x = NULL,
        y = "Number of Respondents"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold", margin = margin(b = 12)),
        axis.text.x = element_text(size = 14,  color = "black"),
        axis.text.y = element_text(size = 12, color = "grey30"),
        axis.title.y = element_text(size = 14, color = "grey30", margin = margin(r = 6)),
        axis.ticks.y = element_line(color = "grey80"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(15, 12, 10, 10)
      )
    
    ggsave(
      filename = paste0("./doc/home_", subset.name, ".png"),plot = plt_home_bar, units = "in",width = 5.2,height = 3.5, dpi = 300)
  }
  
  return(list(
    origin_bar = plt_origin_bar,
    home_bar   = plt_home_bar
  ))
}

# run function -----------
plots <- summarize.origin.location(d = d1, subset.name = "all_data")
plots
