# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : to map homes based on codes where respondents were surveyed

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)
library(colorspace)
library(sf)
library(scales)
library(ggspatial) # For north arrow and scale bar
library(ggnewscale)
library(ggpattern)


# folder with images of surveys: https://drive.google.com/drive/folders/1_VaxieKnc8wnmfNI453iRzqFFOsmfuaA?usp=drive_link
#----------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

#zip codes in CA ------
d0<-st_read("./gis_data/zip_codes_ca/california_zip_codes4.shp")%>%
  mutate(zip_code=as.numeric(zip_code))%>%
  glimpse()

plot(d0[1])

d00<-st_read("./gis_data/mpa_ca_state_124/ca_state_mpas_124.shp")%>%
  select(name=NAME,ccr=CCR,full_name=FULLNAME,type=Type,ccr_int=CCR_Int,short_name=SHORTNAME,acres=Acres,hectares=Hectares)%>%
  st_transform(crs = st_crs(d0))%>%
  glimpse()
st_crs(d00)


# survey data ------
d1 <- read_csv("./results/mwis_p2_clean3_maindata_responses.csv") %>% 
  mutate(
    m3_program = recode(
      m3_program,
      "Orange County Coastkeeper"   = "OC Coastkeeper",
      "Santa Barbara Channelkeepers" = "SB Channelkeeper",
      "TDN" = "Tolowa Dee-ni"
    )
  ) %>% 
  glimpse()



# summarize site codes > counts ------------
# program, could update to site transect
# if make zip codes numeric drop leading 0s - need for join
site_counts <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = as.numeric(str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0"))) %>% 
  group_by(zip_clean, m3_program) %>% 
  summarize(n = n(), .groups = "drop")
# view(site_counts)


# check on how many zip include multiple programs
# currently 38 zip codes
zip_overlap_summary <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0")) %>% 
  group_by(zip_clean) %>% 
  summarize(
    total_responses = n(),
    num_programs = n_distinct(m3_program),
    program_list = paste(unique(m3_program), collapse = ", "),
    .groups = "drop"
  )%>%
  # filter(num_programs>1)%>% # to only list zips with multiple responses
  glimpse()


# EXPLORING OVERLAP OF ZIP CODES --------------------------------

# 1: Overall Overlap Rate ---
cat("\n=== OVERALL OVERLAP SUMMARY ===\n")
zip_overlap_summary %>% 
  summarize(
    total_zips = n(),
    single_program_zips = sum(num_programs == 1),
    multi_program_zips = sum(num_programs > 1),
    pct_overlapping = mean(num_programs > 1) * 100
  ) %>% 
  print()

# 7% overlap


# 2: Distribution of Programs per ZIP ---
cat("\n=== ZIP CODE COUNT BY NUMBER OF PROGRAMS ===\n")
zip_overlap_summary %>% 
  count(num_programs, name = "number_of_zips") %>% 
  mutate(percentage = round(number_of_zips / sum(number_of_zips) * 100, 2)) %>% 
  print()

# max two programs per zip

# 3: List of Overlapping ZIP Codes ---
cat("\n=== MULTI-PROGRAM ZIP CODES DETAILED ===\n")
multi_zips <- zip_overlap_summary %>% 
  filter(num_programs > 1)

print(multi_zips)

# 4: Pairwise Program Overlap Matrix ---
# Shows how many ZIPs each pair of programs shares
cat("\n=== PAIRWISE PROGRAM OVERLAP MATRIX ===\n")
program_zip_pairs <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0")) %>% 
  distinct(zip_clean, m3_program)%>%
  glimpse()


# Cross-join programs to find overlapping ZIP counts
pairwise_matrix <- program_zip_pairs %>% 
  inner_join(program_zip_pairs, by = "zip_clean", suffix = c("_A", "_B")) %>% 
  count(m3_program_A, m3_program_B, name = "shared_zips") %>% 
  pivot_wider(names_from = m3_program_B, values_from = shared_zips, values_fill = 0)

print(pairwise_matrix)




# WRANGLING FOR MAPS ---------------

# Summarize programs per ZIP code --------------
zip_program_categories <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = as.numeric(str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0"))) %>% 
  group_by(zip_clean) %>% 
  summarize(
    total_responses = n(),
    num_programs = n_distinct(m3_program),
    # Creates "Program A" for single, or "Program A / Program B" for overlap
    program_category = paste(sort(unique(m3_program)), collapse = " / "),
    .groups = "drop"
  )


# frequency of each category
zip_program_categories %>% 
  count(program_category, sort = TRUE)




# MAPPING ------------------

# Join with shapefile
map_data_programs <- d0 %>% 
  left_join(zip_program_categories, by = c("zip_code" = "zip_clean")) %>% 
  mutate(
    program_category = replace_na(program_category, "No Survey"),
    program_category = factor(program_category) %>% fct_relevel("No Survey", after = Inf)
  )

# 2. Extract active program levels (excluding "No Survey")
active_levels <- levels(map_data_programs$program_category)
program_levels <- setdiff(active_levels, "No Survey")

# 3. Generate colors using the Batlow palette from colorspace
program_colors <- sequential_hcl(length(program_levels), palette = "Batlow")
names(program_colors) <- program_levels

# Add 'grey96' explicitly for "No Survey"
all_colors <- c(program_colors, "No Survey" = "grey96")


#------------------------------------------------------------------
# MAP 1: ALL OVERLAP ZIP CATEGORIES  ------------------
#------------------------------------------------------------------

ggplot(map_data_programs) +
  geom_sf(aes(fill = program_category), color = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = all_colors,
    name = "Survey Program",
    na.value = "grey92"
  ) +
  
  # add MPAS 
  # Reset the fill scale so MPAs get their own distinct legend box
  new_scale_fill() +
  geom_sf(
    data = d00, 
    aes(fill = "MPAs"), 
    color = alpha("#00588B", 0.5),
    linewidth = 0.2,
    alpha = 0.5 # Makes MPAs pop less
  ) +
  scale_fill_manual(
    values = c("MPAs" = alpha("#009B95", 0.5)),
    name = NULL # Keeps legend clean without a second title header
  ) +
  # North Arrow
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    pad_x = unit(1.4, "in"), 
    pad_y = unit(0.65, "in"),
    height = unit(0.5, "in"),
    width = unit(0.3, "in"),
    style = north_arrow_orienteering(text_size = 6)
  ) +
  
  # Scale Bar
  annotation_scale(
    location = "bl",
    pad_x = unit(1, "in"),
    pad_y = unit(0.3, "in"),
    unit_category = "imperial",
    width_hint = 0.18,
  ) +
    # Map Title
  annotate(
    "text", 
    x = I(0.46), y = I(0.82), 
    label = "MPA Watch\nIntercept\nSurvey", 
    size = 6.5, 
    fontface = "bold",
    color = "grey30", 
    hjust = 0
  ) +
  
  theme_void() +
  theme(
    legend.position = c(0.15, 0.25),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.box = "vertical" # Stacks MPA box 
  )

ggsave("./doc/mwis_map_program1.png", width = 9, height = 9, dpi = 300)





#------------------------------------------------------------------
# MAP 2: GENERIC "MULTIPLE PROGRAMS" CATEGORY ------------------
#------------------------------------------------------------------

# 1. Wrangle data with a generic "Multiple Programs" label
zip_program_grouped <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = as.numeric(str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0"))) %>% 
  group_by(zip_clean) %>% 
  summarize(
    total_responses = n(),
    num_programs = n_distinct(m3_program),
    # Collapses any multi-program ZIP into "Multiple Programs"
    program_category = if_else(num_programs > 1, "Multiple Programs", first(m3_program)),
    .groups = "drop"
  )

# 2. Join with shapefile & set factor order ("Multiple Programs" then "No Survey" at the bottom)
map_data_grouped <- d0 %>% 
  left_join(zip_program_grouped, by = c("zip_code" = "zip_clean")) %>% 
  mutate(
    program_category = replace_na(program_category, "No Survey"),
    program_category = factor(program_category) %>% 
      fct_relevel("Multiple Programs", "No Survey", after = Inf)
  )

# 3. Extract active program levels
active_levels_g <- levels(map_data_grouped$program_category)
program_levels_g <- setdiff(active_levels_g, c("Multiple Programs", "No Survey"))

# 4. Generate colors using Batlow palette for single programs, plus custom shades for special categories
program_colors_g <- sequential_hcl(length(program_levels_g), palette = "Batlow")
names(program_colors_g) <- program_levels_g

# Define full color vector including "Multiple Programs" (dark charcoal/grey) and "No Survey"
all_colors_g <- c(
  program_colors_g, 
  "Multiple Programs" = "grey60", 
  "No Survey"          = "grey92")



# Plot  Map with fewer groups -----------------
ggplot(map_data_grouped) +
  geom_sf(aes(fill = program_category), color = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = all_colors_g,
    name = "Survey Program",
    na.value = "grey92"
  ) +
  
  # add MPAS 
  # Reset the fill scale so MPAs get their own distinct legend box
  new_scale_fill() +
  geom_sf(
    data = d00, 
    aes(fill = "MPAs"), 
    color = alpha("#00588B", 0.5),
    linewidth = 0.2,
    alpha = 0.5 # Makes MPAs pop less
  ) +
  scale_fill_manual(
    values = c("MPAs" = alpha("#009B95", 0.5)),
    name = NULL # Keeps legend clean without a second title header
  ) +
  # North Arrow
  annotation_north_arrow(
    location = "bl", 
    which_north = "true",
    pad_x = unit(1.7, "in"), 
    pad_y = unit(0.65, "in"),
    height = unit(0.5, "in"),
    width = unit(0.3, "in"),
    style = north_arrow_orienteering(text_size = 6)
  ) +
  
  # Scale Bar
  annotation_scale(
    location = "bl",
    pad_x = unit(1, "in"),
    pad_y = unit(0.3, "in"),
    unit_category = "imperial",
    width_hint = 0.18  ) +
  # Map Title
  annotate(
    "text", 
    x = I(0.46), y = I(0.82), 
    label = "MPA Watch\nIntercept\nSurvey", 
    size = 6.5, 
    fontface = "bold",
    color = "grey30", 
    hjust = 0
  ) +
  theme_void() +
  theme(
    legend.position = c(0.15, 0.25),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # legend.position = c(0.12, 0.22),
    legend.box = "vertical" # Stacks MPA box 
  )


ggsave("./doc/mwis_map_program2.png", width = 9, height = 9, dpi = 300)





