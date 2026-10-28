# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal: Map respondent distribution across the US (by state) and Internationally (by country)
# ------------------
library(tidyverse)
library(sf)
library(tigris)
library(colorspace)
library(scales)
library(lwgeom)
library(ggspatial) # For north arrow and scale bar

# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

# Ensure tigris outputs sf objects and suppress message chatter
options(tigris_use_cache = TRUE)

# survey data ------
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()

# 2. Summarize counts by US state
# Clean state abbreviations/names to match standard 2-letter codes or full names
state_counts <- d1 %>%
  filter(!is.na(state_resolved)) %>%
  mutate(state_clean = str_trim(state_resolved)) %>%
  count(state_clean, name = "count")

# 3. Load and prepare US spatial polygons (with AK/HI shifted)
us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() %>%
  # Filter out overseas territories if not needed (e.g., Guam, Virgin Islands)
  filter(!STUSPS %in% c("GU", "VI", "MP", "AS"))

# Standardize join key: checks whether your data uses 2-letter (STUSPS) or full name (NAME)
if (any(state_counts$state_clean %in% us_states$STUSPS)) {
  us_map_data <- us_states %>%
    left_join(state_counts, by = c("STUSPS" = "state_clean"))
} else {
  us_map_data <- us_states %>%
    left_join(state_counts, by = c("NAME" = "state_clean"))
}

# 4. Bin state counts (adjust breaks to match your distribution)
us_map_binned <- us_map_data %>%
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1                ~ "1",
      count >= 2  & count <= 5  ~ "2–5",
      count >= 6  & count <= 20 ~ "6–20",
      count >= 21 & count <= 50 ~ "21–50",
      count > 50                ~ ">50"
    ),
    count_cat = factor(
      count_cat,
      levels = c("0", "1", "2–5", "6–20", "21–50", ">50")
    )
  )

sunset_colors_us <- sequential_hcl(5, "SunsetDark", rev = TRUE)

# 5. Plot US Map
p_us <- ggplot() +
  geom_sf(
    data = us_map_binned,
    aes(fill = count_cat),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = c(
      ">50"   = sunset_colors_us[5],
      "21–50" = sunset_colors_us[4],
      "6–20"  = sunset_colors_us[3],
      "2–5"   = sunset_colors_us[2],
      "1"     = sunset_colors_us[1],
      "0"     = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">50", "21–50", "6–20", "2–5", "1", "0")
  ) +
  # annotate(
  #   "text",
  #   x = I(0.05), y = I(0.92),
  #   # label = "MPA Watch Intercept Survey Respondents by US State",
  #   size = 5.5,
  #   fontface = "bold",
  #   color = "grey30",
  #   hjust = 0
  # ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.92, 0.25),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )+# North Arrow
  annotation_north_arrow(
    location = "br", 
    which_north = "true",
    pad_x = unit(0.5, "in"), 
    pad_y = unit(0.2, "in"),
    height = unit(0.4, "in"),
    width = unit(0.2, "in"),
    style = north_arrow_orienteering(text_size = 6)
  ) +
  
  # Scale Bar
  annotation_scale(
    location = "br",
    pad_x = unit(1, "in"),
    pad_y = unit(0.3, "in"),
    unit_category = "imperial",
    width_hint = 0.18,
    # text_col = "transparent"
  ) 
p_us
ggsave("./doc/mwis_map_us_states.png", p_us, width = 10, height = 6.5, dpi = 300)
