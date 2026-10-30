# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa info sources
# ------------------

library(tidyverse)
library(scales)
library(viridis)
library(ggplot2)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean7.csv")%>%
  glimpse()

d1$q9_info_sources 

# -------------------------
# Total number of respondents who provided information sources
total_respondents <- d1 %>%
  filter(!is.na(q9_info_sources)) %>%
  nrow()

# 1. Unnest multi-select items, trim whitespace, and tally counts
info_summary <- d1 %>%
  filter(!is.na(q9_info_sources)) %>%
  separate_longer_delim(q9_info_sources, delim = regex(";\\s*")) %>%
  mutate(q9_info_sources = str_trim(q9_info_sources)) %>%
  filter(q9_info_sources != "") %>%
  count(q9_info_sources, name = "count", sort = TRUE) %>%
  mutate(
    pct = count / total_respondents,
    label = paste0(count, " (", percent(pct, accuracy = 0.1), ")")
  )

# 2. Horizontal bar chart with Viridis palette
ggplot(info_summary, aes(x = count, y = fct_reorder(q9_info_sources, count), fill = count)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  # Continuous Viridis gradient along frequency
  scale_fill_viridis(
    option = "viridis",   # Options: "viridis", "mako", "plasma", "cividis"
    begin = 0.25,        # Avoids extreme dark tones
    end = 0.85,          # Avoids overly pale yellows against white
    direction = 1
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18))  # Extra right-side margin for labels
  ) +
  labs(
    title = "Sources of Information Regarding Marine Protected Areas",
    subtitle = paste0("Total respondents: N = ", total_respondents, " (categories are not mutually exclusive)"),
    x = "Number of Respondents",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 12)),
    axis.text = element_text(color = "black", size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("./doc/mwis_info_source.png",  width = 11,  height = 5.5,  dpi = 300)
