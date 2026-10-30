# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: ocean management
# ------------------

library(tidyverse)
library(plyr)
library(dplyr)
library(likert)
library(scales)


#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean7.csv")%>%
   mutate(q10_ocean_management=if_else(q10_ocean_management=="Very Important","Very important",q10_ocean_management))%>%
  filter(!is.na(q10_ocean_management)) %>%
   glimpse()

unique(d1$q10_ocean_management)

# 1. Calculate proportions and create diverging x values (pct2)
d2 <- d1 %>%
  filter(!is.na(q10_ocean_management)) %>%
  dplyr::count(q10_ocean_management) %>%
  dplyr::rename(value = q10_ocean_management) %>%
  mutate(
    pct = n / sum(n),
    pct2 = case_when(
      value == "Very unimportant" ~ -pct,
      value == "Unimportant"      ~ -pct,
      value == "Neutral"          ~ 0,
      value == "Important"        ~ pct,
      value == "Very important"   ~ pct
    ),
    opinion2 = "Importance of ocean management"
  ) %>%
  glimpse()

# Set factor levels for stacking order
d2$value <- factor(
  d2$value,
  levels = c("Very important", "Important", "Neutral", "Unimportant", "Very unimportant")
)

# 2. Diverging Bar Graph
ggplot(d2, aes(y = opinion2, x = pct2, fill = value)) + 
  geom_col(width = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c(
      "Very important"   = "#002F70",
      "Important"        = "#879FDB",
      "Neutral"          = "grey60",
      "Unimportant"      = "#DA8A8B",
      "Very unimportant" = "#5F1415"
    ),
    breaks = c("Very unimportant", "Unimportant", "Neutral", "Important", "Very important"),
    name = ""
  ) +
  scale_x_continuous(
    limits = c(-1, 1),
    labels = function(x) percent(abs(x)) # Displays negative values as positive percentages
  ) +
  labs(
    title = "Importance of Ocean Management",
    x = "% of Respondents",
    y = ""
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    axis.text.y = element_blank(),   # Removes label text on y-axis
    axis.ticks.y = element_blank(),  # Removes tick marks on y-axis
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


ggsave("./doc/ocean_management_q10.png",  width = 11,  height = 5.5,  dpi = 300)