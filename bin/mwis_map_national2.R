# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : to map homes by state based on program codes where respondents were surveyed

# -----------------------------------------------------
library(tidyverse)
library(sf)
library(tigris)
library(colorspace)
library(scales)

options(tigris_use_cache = TRUE)
# -----------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/r_projects/mwis")

d1 <- read_csv("./results/mwis_p2_clean4_maindata_responses.csv") %>%
  mutate(
    m3_program = recode(
      m3_program,
      "Orange County Coastkeeper"   = "OC Coastkeeper",
      "Santa Barbara Channelkeepers" = "SB Channelkeeper",
      "TDN"                         = "Tolowa Dee-ni"
    ),
    state_clean = str_trim(state_resolved)
  )

# Fetch shifted US state boundaries (AK, HI, PR shifted cleanly below CONUS)
us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() %>%
  filter(!STUSPS %in% c("GU", "VI", "MP", "AS"))

# Helper function to join survey data by 2-letter STUSPS or full state NAME
join_state_data <- function(spatial_df, data_df) {
  if (any(data_df$state_clean %in% spatial_df$STUSPS)) {
    left_join(spatial_df, data_df, by = c("STUSPS" = "state_clean"))
  } else {
    left_join(spatial_df, data_df, by = c("NAME" = "state_clean"))
  }
}

# -------------------------------------------------------------------------
# 2. Diagnostics: State Overlap Across Programs
# -------------------------------------------------------------------------

state_overlap_summary <- d1 %>%
  filter(!is.na(state_clean)) %>%
  group_by(state_clean) %>%
  summarize(
    total_responses = n(),
    num_programs = n_distinct(m3_program),
    program_list = paste(sort(unique(m3_program)), collapse = ", "),
    .groups = "drop"
  )

cat("\n=== STATE OVERLAP SUMMARY ===\n")
state_overlap_summary %>%
  summarize(
    total_states = n(),
    single_program_states = sum(num_programs == 1),
    multi_program_states = sum(num_programs > 1),
    pct_overlapping = round(mean(num_programs > 1) * 100, 1)
  ) %>%
  print()
# overlapping states = 69.4% so high (25 out of 36)

# Pairwise program state sharing matrix
pairwise_state_matrix <- d1 %>%
  filter(!is.na(state_clean), !is.na(m3_program)) %>%
  distinct(state_clean, m3_program) %>%
  inner_join(., ., by = "state_clean", suffix = c("_A", "_B")) %>%
  count(m3_program_A, m3_program_B, name = "shared_states") %>%
  pivot_wider(names_from = m3_program_B, values_from = shared_states, values_fill = 0)

print(pairwise_state_matrix)


# =========================================================================
# MAP 2: GENERIC "MULTIPLE PROGRAMS" CATEGORY
# =========================================================================

state_program_cat2 <- d1 %>%
  filter(!is.na(state_clean), !is.na(m3_program)) %>%
  group_by(state_clean) %>%
  summarize(
    total_responses = n(),
    num_programs = n_distinct(m3_program),
    program_category = if_else(num_programs > 1, "Multiple Programs", first(m3_program)),
    .groups = "drop"
  )

map_data_p2 <- join_state_data(us_states, state_program_cat2) %>%
  mutate(
    program_category = replace_na(program_category, "No Survey"),
    program_category = factor(program_category) %>%
      fct_relevel("Multiple Programs", "No Survey", after = Inf)
  )

active_levels2 <- levels(map_data_p2$program_category)
program_levels2 <- setdiff(active_levels2, c("Multiple Programs", "No Survey"))

program_colors2 <- sequential_hcl(length(program_levels2), palette = "Batlow")
names(program_colors2) <- program_levels2

all_colors2 <- c(
  program_colors2,
  "Multiple Programs" = "grey60",
  "No Survey"          = "grey92"
)

p_map2 <- ggplot(map_data_p2) +
  geom_sf(aes(fill = program_category), color = "white", linewidth = 0.25) +
  scale_fill_manual(
    values = all_colors2,
    name = "Survey Program",
    na.value = "grey92"
  ) +
  annotate(
    "text",
    x = I(0.04), y = I(0.92),
    label = "MPA Watch Intercept Survey\nUS Respondents by Program",
    size = 5.5,
    fontface = "bold",
    color = "grey30",
    hjust = 0
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.88, 0.28),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )
p_map2
ggsave("./doc/mwis_map_us_program2.png", p_map2, width = 11, height = 7, dpi = 300)


# =========================================================================
# MAP 3: FACETED SMALL MULTIPLES BY PROGRAM (NO OVERLAP CONFLICT)
# =========================================================================
# Useful if many states fall into "Multiple Programs" and you want
# to show respondent volume for each program individually.

state_by_prog_counts <- d1 %>%
  filter(!is.na(state_clean), !is.na(m3_program)) %>%
  count(state_clean, m3_program, name = "count")

# Create a complete grid so every program gets all 50 states
program_names <- unique(state_by_prog_counts$m3_program)
grid_base <- expand_grid(
  STUSPS = us_states$STUSPS,
  m3_program = program_names
)

map_data_faceted <- us_states %>%
  left_join(grid_base, by = "STUSPS") %>%
  left_join(state_by_prog_counts, by = c("STUSPS" = "state_clean", "m3_program" = "m3_program")) %>%
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1                ~ "1",
      count >= 2 & count <= 5   ~ "2–5",
      count >= 6 & count <= 20  ~ "6–20",
      count > 20                ~ ">20"
    ),
    count_cat = factor(count_cat, levels = c("0", "1", "2–5", "6–20", ">20"))
  )

sunset_colors_facet <- sequential_hcl(4, "SunsetDark", rev = TRUE)

p_map3 <- ggplot(map_data_faceted) +
  geom_sf(aes(fill = count_cat), color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = c(
      ">20"  = sunset_colors_facet[4],
      "6–20" = sunset_colors_facet[3],
      "2–5"  = sunset_colors_facet[2],
      "1"    = sunset_colors_facet[1],
      "0"    = "grey92"
    ),
    name = "No. Responses",
    breaks = c(">20", "6–20", "2–5", "1", "0")
  ) +
  facet_wrap(~m3_program, ncol = 2) +
  theme_void() +
  theme(
    strip.text = element_text(size = 11, face = "bold", margin = margin(b = 6)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(12, 12, 12, 12)
  )
p_map3
ggsave("./doc/mwis_map_us_program_faceted.png", p_map3, width = 12, height = 8, dpi = 300)