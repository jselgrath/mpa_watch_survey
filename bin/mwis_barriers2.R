# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: graph barriers/difficulties
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

# Total number of respondents answering the barriers question
total_respondents <- d1 %>%
  filter(!is.na(q6_barriers)) %>%
  nrow()

# 1. Unbundle categories into individual rows & tally frequencies
barrier_summary <- d1 %>%
  filter(!is.na(q6_barriers)) %>%
  # Separate multi-category strings into separate rows
  separate_longer_delim(q6_barriers, delim = regex(";\\s*")) %>%
  mutate(q6_barriers = str_trim(q6_barriers)) %>%
  filter(q6_barriers != "") %>%
  count(q6_barriers, name = "count", sort = TRUE) %>%
  mutate(
    pct = count / total_respondents,
    # Label combining count and percentage: e.g., "45 (32.1%)"
    label = paste0(count, " (", percent(pct, accuracy = 0.1), ")")
  )

print(barrier_summary)
write_csv(barrier_summary,"./doc/barrier_summary.csv")

# graph ------------
ggplot(barrier_summary, aes(x = count, y = fct_reorder(q6_barriers, count))) +
  geom_col(fill = "#2b6cb0", width = 0.7) +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 3.8
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)) # Extra headroom for labels
  ) +
  labs(
    title = "Frequency of Reported Barriers & Difficulties",
    subtitle = paste0("Total respondents: N = ", total_respondents, " (categories are not mutually exclusive)"),
    x = "Number of Respondents",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(color = "grey35", margin = margin(b = 12)),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("./doc/barriers_all_q6.png",  width = 11,  height = 5.5,  dpi = 300)
