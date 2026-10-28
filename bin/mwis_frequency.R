# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: frequency of visits
# ------------------

library(tidyverse)
library(viridis)
library(ggplot2)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")


# survey data ------
d1<-read_csv("./results/mwis_p2_clean5.csv")%>%
  glimpse()

# shorter categories & relevel factor
d1$q5_frequency[d1$q5_frequency=="Several times per week or more"]<-"> Several times/week"
d1$q5_frequency[d1$q5_frequency=="Several times per month"]<-"Several times/month"
d1$q5_frequency[d1$q5_frequency=="Once every couple months"]<-"Several times/year"
d1$q5_frequency[d1$q5_frequency=="Once a year"]<-"Once/year"
d1$q5_frequency[d1$q5_frequency=="Less than once a year (i.e., rarely or never)"]<-"Less than\nonce/year"
d1$q5_frequency[d1$q5_frequency=="Decline to respond"]<-NA

d1$q5_frequency<-factor(d1$q5_frequency)
d1$q5_frequency<-factor(d1$q5_frequency,levels=c("Less than\nonce/year","Once/year","Several times/year","Several times/month","> Several times/week"))
print(summary(d1$q5_frequency))


# Counts ------------------
d1 %>%
  filter(!is.na(q5_frequency)) %>%
  ggplot(aes(x = q5_frequency)) +
  geom_bar(fill=viridis(3,option="mako")[2], width = 0.65) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size = 4.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    # title = "Frequency of Use (MWIS Survey)",
    x = "Frequency of Ocean/Coastal Visits",
    y = "No. Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    # plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 12)),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14,margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./doc/frequency_count_all_q5.png",  width = 11,  height = 5.5,  dpi = 300)



# percents

d1 %>%
  filter(!is.na(q5_frequency)) %>%
  count(q5_frequency) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = q5_frequency, y = pct)) +
  geom_col(fill=viridis(3,option="mako")[2], width = 0.6) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    vjust = -0.5,
    size = 4.2
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    # title = "Frequency of Use",
    x = "Frequency of Ocean/Coastal Visits",
    y = "% Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14,margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./doc/frequency_pct_all_q5.png",  width = 11,  height = 5.5,  dpi = 300)