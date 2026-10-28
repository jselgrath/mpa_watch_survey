# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: graph activities
# ------------------

library(tidyverse)
library(viridis)
library(scales)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean5.csv")%>%
  glimpse()

unique(d1$q3_purpose2)
unique(d1$q4_purpose_most_important2)

# =========================================================================
# PART 1: ALL ACTIVITIES ENGAGED IN (q3_purpose2)
# =========================================================================

# 1. Reshape to long format: one row per respondent per activity
activities_all_long <- d1 %>%
  filter(!is.na(q3_purpose2), q3_purpose2 != "") %>%
  select(response_id, m3_program, q3_purpose2) %>%
  separate_rows(q3_purpose2, sep = ",") %>%
  mutate(activity = str_trim(q3_purpose2)) %>%
  filter(activity != "")

# Verification inspection
cat("\n=== ALL ACTIVITIES BREAKDOWN ===\n")
activities_all_long %>%
  count(activity, sort = TRUE) %>%
  print(n = 25)

# 2. Total unique respondents answering q3
n_q3_respondents <- d1 %>%
  filter(!is.na(q3_purpose2), q3_purpose2 != "") %>%
  pull(response_id) %>%
  n_distinct()

# 3. Summarize counts and percentages
q3_summary <- activities_all_long %>%
  count(activity, name = "Count") %>%
  mutate(
    pct = Count / n_q3_respondents,
    label = paste0(comma(Count), "\n(", percent(pct, accuracy = 0.1), ")"),
    activity = fct_reorder(activity, Count, .desc = TRUE)
  )

# 4. Plot: All Activities
plt_all_activities <- ggplot(q3_summary, aes(x = activity, y = Count, fill = Count)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(
    aes(label = label),
    vjust = -0.25,
    size = 2.8,
    lineheight = 0.85,
    color = "grey20"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = comma
  ) +
  scale_fill_viridis_c(option = "mako", begin = 0.35, end = 0.85, direction = 1) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Activities Engaged in by Survey Respondents (N = ", comma(n_q3_respondents), ")"),
    subtitle = "Percentages reflect proportion of total respondents (multi-selection allowed)",
    x = "Activity",
    y = "No. Respondents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 14)),
    axis.text.x = element_text(
      size = 9,      face = "bold",      color = "black",      angle = 90,      hjust = 1,  vjust = 0.5),
    axis.text.y = element_text(size = 12, color = "grey30"),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", color = "grey30", margin = margin(r = 8)),
    axis.ticks.y = element_line(color = "grey80"),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(15, 15, 10, 15)
  )

plt_all_activities

ggsave(  filename = "./doc/activities_all_q3.png",  plot = plt_all_activities,  width = 11,  height = 5.5,  dpi = 300)


# =========================================================================
# PART 2: MOST IMPORTANT ACTIVITY (q4_purpose_most_important2)
# =========================================================================

# 1. Reshape to long format: handles cases where a write-in created dual categories
activities_most_long <- d1 %>%
  filter(!is.na(q4_purpose_most_important2), q4_purpose_most_important2 != "") %>%
  select(response_id, m3_program, q4_purpose_most_important2) %>%
  separate_rows(q4_purpose_most_important2, sep = ",") %>%
  mutate(activity = str_trim(q4_purpose_most_important2)) %>%
  filter(activity != "")

# Verification inspection
cat("\n=== MOST IMPORTANT ACTIVITY BREAKDOWN ===\n")
activities_most_long %>%
  count(activity, sort = TRUE) %>%
  print(n = 25)

# 2. Total unique respondents answering q4
n_q4_respondents <- d1 %>%
  filter(!is.na(q4_purpose_most_important2), q4_purpose_most_important2 != "") %>%
  pull(response_id) %>%
  n_distinct()

# 3. Summarize counts and percentages
q4_summary <- activities_most_long %>%
  count(activity, name = "Count") %>%
  mutate(
    pct = Count / n_q4_respondents,
    label = paste0(comma(Count), "\n(", percent(pct, accuracy = 0.1), ")"),
    activity = fct_reorder(activity, Count, .desc = TRUE)
  )

# 4. Plot: Most Important Activity
plt_most_important <- ggplot(q4_summary, aes(x = activity, y = Count, fill = Count)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(
    aes(label = label),
    vjust = -0.25,
    size = 2.8,
    lineheight = 0.85,
    color = "grey20"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = comma
  ) +
  scale_fill_viridis_c(option = "mako", begin = 0.35, end = 0.85, direction = 1) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Most Important Activity for Survey Respondents (N = ", comma(n_q4_respondents), ")"),
    subtitle = "Primary activity motivating the visit to the coast",
    x = "Activity",
    y = "No. Respondents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, color = "grey40", margin = margin(b = 14)),
    axis.text.x = element_text(
      size = 9,
      face = "bold",
      color = "black",
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 8.5, color = "grey30"),
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, face = "bold", color = "grey30", margin = margin(r = 8)),
    axis.ticks.y = element_line(color = "grey80"),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(15, 15, 10, 15)
  )

plt_most_important

ggsave(  filename = "./doc/activities_most_important_q4.png",  plot = plt_most_important,  width = 11,  height = 5.5,  dpi = 300)