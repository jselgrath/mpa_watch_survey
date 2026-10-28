# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: Map respondent distribution Internationally (by country)
# ------------------
library(tidyverse)
library(sf)
# library(tigris)
library(colorspace)
library(scales)
library(lwgeom)
library(ggspatial) # For north arrow and scale bar
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

# survey data ------
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()


# Count International respondents
intl_counts <- d1 %>%
  filter(!is.na(residence_detail)) %>%
  filter(!str_detect(international, regex("domestic", ignore_case = TRUE))) %>%
  mutate(
    iso3 = countrycode(residence_detail, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(!is.na(iso3)) %>%
  count(iso3, name = "count")

# Count Domestic (US) respondents
us_count <- d1 %>%
  filter(str_detect(international, regex("domestic", ignore_case = TRUE))) %>%
  summarize(count = n()) %>%
  mutate(iso3 = "USA")

# Combine US and international counts
all_country_counts <- bind_rows(intl_counts, us_count)

# 2. Global Boundaries (Robinson Projection)
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  st_transform(crs = 'ESRI:54030')

# 3. Join & Bin (Adjust the top bin threshold to match your domestic sample size)
world_map_data <- world_sf %>%
  # Fallback in case iso_a3 is -99 in rnaturalearth:
  mutate(iso_a3_clean = if_else(iso_a3 == "-99", adm0_a3, iso_a3)) %>%
  left_join(all_country_counts, by = c("iso_a3_clean" = "iso3")) %>%
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1                ~ "1",
      count >= 2  & count <= 5  ~ "2–5",
      count >= 6  & count <= 15 ~ "6–15",
      count >= 16 & count <= 50 ~ "16–50",
      count > 50                ~ ">1,300"
    ),
    count_cat = factor(
      count_cat,
      levels = c("0", "1", "2–5", "6–15", "16–50", ">1,300")  # Matched to ">1,300"
    )
  )

sunset_colors_global <- sequential_hcl(5, "SunsetDark", rev = TRUE)

# 4. Plot Global Map
p_global <- ggplot() +
  geom_sf(
    data = world_map_data,
    aes(fill = count_cat),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      ">1,300" = sunset_colors_global[5],                     # Matched to ">1,300"
      "16–50"  = sunset_colors_global[4],
      "6–15"   = sunset_colors_global[3],
      "2–5"    = sunset_colors_global[2],
      "1"      = sunset_colors_global[1],
      "0"      = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">1,300", "16–50", "6–15", "2–5", "1", "0"),
    na.value = "grey92"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.12, 0.38),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

p_global

ggsave("./doc/mwis_map_international2.png", p_global, width = 11, height = 6.5, dpi = 300)