# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa awareness
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

unique(d1$q7_mpa_awareness)

table(d1$q7_mpa_awareness, useNA = "always")


# Calculate total valid responses
total_valid <- d1 %>%
  filter(!is.na(q7_mpa_awareness), q7_mpa_awareness != "Decline to respond") %>%
  nrow()

# Summarize and plot
d1 %>%
  filter(!is.na(q7_mpa_awareness)) %>%  # remove NAs
  count(q7_mpa_awareness, name = "count") %>%
  mutate(
    pct = count / total_valid,
    label = paste0(count, "\n(", percent(pct, accuracy = 0.1), ")")
  ) %>%
  ggplot(aes(x = q7_mpa_awareness, y = count)) +
  geom_col(fill = "#1b7837", width = 0.55) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 4,
    lineheight = 0.9
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Awareness of Marine Protected Areas (MPAs)",
    subtitle = paste0("Total respondents: N = ", total_valid),
    x = NULL,
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "grey35", hjust = 0.5, margin = margin(b = 12)),
    axis.text = element_text(color = "black", size = 11),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./doc/mpa_awareness_all_q7.png",  width = 11,  height = 5.5,  dpi = 300)