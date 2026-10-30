# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa location
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
   glimpse()

unique(d1$q8_mpa_location)

d2 <- d1 %>%
  mutate(
    q8_mpa_location2 = case_when(
      q8_mpa_location == "Yes, I am at an MPA"    ~ "At MPA",
      q8_mpa_location == "I am not sure"          ~ "Not sure",
      q8_mpa_location == "No, I am not at an MPA" ~ "Not at MPA",
      TRUE                                        ~ NA_character_),
    q8_mpa_location2 = factor(q8_mpa_location2,   levels = c("At MPA", "Not sure", "Not at MPA")))

table(d2$q8_mpa_location2, useNA = "always") # 1 NA

# graph ---------------------
# Total valid responses (excluding NA)
total_valid <- d2 %>%
  filter(!is.na(q8_mpa_location2)) %>%
  nrow()

# Summarize and generate bar plot
d2 %>%
  filter(!is.na(q8_mpa_location2)) %>%
  count(q8_mpa_location2, name = "count") %>%
  mutate(
    pct = count / total_valid,
    label = paste0(count, "\n(", percent(pct, accuracy = 0.1), ")")
  ) %>%
  ggplot(aes(x = q8_mpa_location2, y = count, fill = q8_mpa_location2)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 4.2,
    lineheight = 0.9
  ) +
scale_fill_viridis_d(
  option = "viridis",
  begin = 0.2,   # avoids extreme dark purple for better contrast
  end = 0.85,    # avoids ultra-pale yellow on white backgrounds
  direction = 1  # use -1 to reverse the gradient
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Awareness of Current Location Being within an MPA",
    subtitle = paste0("Total valid respondents: N = ", total_valid),
    x = "Awareness Location is at a MPA",
    y = "No. Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "grey35", hjust = 0.5, margin = margin(b = 12)),
    axis.text = element_text(color = "black", size = 12),
    axis.title.x = element_text(size = 14,margin = margin(t = 10)),
    axis.title.y = element_text(size = 14,margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./doc/mpa_location_q8.png",  width = 11,  height = 5.5,  dpi = 300)