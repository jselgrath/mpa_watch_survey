# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: mpa value source - graph - need to finish cleaning
# -----------------

library(tidyverse)
library(scales)
library(viridis)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean8.csv")%>%
   glimpse()

unique(d1$q11_mpa_value_source3)
unique(d1$q11_mpa_value_source)
d1$q11_mpa_value_source[1001:1367]

# Total number of respondents with valid values (excluding NA and Unsure/Non-response)
total_respondents <- d1 %>%
  filter(!is.na(q11_mpa_value_source3), 
         q11_mpa_value_source3 != "Unsure/Non-response") %>%
  nrow()

# 1. Unnest multi-select items, trim, and calculate frequencies
mpa_values_summary <- d1 %>%
  # Filter out missing and non-responses
  filter(!is.na(q11_mpa_value_source3), 
         q11_mpa_value_source3 != "Unsure/Non-response") %>%
  # Separate multi-theme strings by semicolon
  separate_longer_delim(q11_mpa_value_source3, delim = regex(";\\s*")) %>%
  mutate(q11_mpa_value_source3 = str_trim(q11_mpa_value_source3)) %>%
  filter(q11_mpa_value_source3 != "") %>%
  # Tally mentions
  dplyr::count(q11_mpa_value_source3, name = "count", sort = TRUE) %>%
  mutate(
    pct = count / total_respondents,
    label = paste0(count, " (", percent(pct, accuracy = 0.1), ")")
  )

# View summary table in console
print(mpa_values_summary)

# 2. Horizontal Bar Plot
ggplot(mpa_values_summary, aes(x = count, y = fct_reorder(q11_mpa_value_source3, count), fill = count)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.9,
    color = "black"
  ) +
  # Colorblind-safe palette (mako fits oceanographic & MPA themes well)
  scale_fill_viridis(
    option = "mako",
    begin = 0.35,
    end = 0.85,
    direction = 1
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)) # Room for text labels on the right
  ) +
  labs(
    title = "Perceived Values & Reasons for Ocean Management and MPAs",
    subtitle = paste0("Total respondents: N = ", total_respondents, " (categories are not mutually exclusive)"),
    x = "Number of Mentions",
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

ggsave("./doc/mwis_value_source_not_final.png",  width = 11,  height = 5.5,  dpi = 300)
