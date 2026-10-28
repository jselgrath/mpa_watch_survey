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

# 1. Extract and count international respondents
# If residence_detail contains both states and foreign countries:
intl_counts <- d1 %>%
  filter(!is.na(residence_detail)) %>%
  # Exclude US records (adjust patterns to match your encoding)
  filter(!str_detect(international, regex("domestic", ignore_case = TRUE))) %>%
  mutate(
    # Standardize country names to ISO3 codes to ensure join accuracy
    iso3 = countrycode(residence_detail, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(!is.na(iso3)) %>%
  count(iso3, name = "count")

# 2. Get global administrative boundaries
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  # Remove Antarctica for a balanced map layout
  filter(continent != "Antarctica") %>%
  # Project to Robinson projection (standard for global survey cartography)
  st_transform(crs = 'ESRI:54030')

# 3. Join counts to global map
world_map_data <- world_sf %>%
  left_join(intl_counts, by = c("iso_a3" = "iso3")) %>%
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1                ~ "1",
      count >= 2 & count <= 5   ~ "2–5",
      count >= 6 & count <= 15  ~ "6–15",
      count > 15                ~ ">15"
    ),
    count_cat = factor(
      count_cat,
      levels = c("0", "1", "2–5", "6–15", ">15")
    )
  )

sunset_colors_intl <- sequential_hcl(4, "SunsetDark", rev = TRUE)

# 4. Plot Global Map
p_intl <- ggplot() +
  geom_sf(
    data = world_map_data,
    aes(fill = count_cat),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      ">15"  = sunset_colors_intl[4],
      "6–15" = sunset_colors_intl[3],
      "2–5"  = sunset_colors_intl[2],
      "1"    = sunset_colors_intl[1],
      "0"    = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">15", "6–15", "2–5", "1", "0")
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

p_intl 

ggsave("./doc/mwis_map_international.png", p_intl, width = 11, height = 6.5, dpi = 300)
