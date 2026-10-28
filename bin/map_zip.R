# Jennifer Selgrath
# California Marine Sanctuary Foundation
# MPA Watch: Intercept Survey

# Goal : to map zip codes where respondents lived

# ------------------
library(tidyverse)
library(ggplot2)
library(janitor)
library(colorspace)
library(sf)
library(scales)
library(ggspatial) #  north arrow and scale bar
library(ggnewscale)



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
d1<-read_csv("./results/mwis_p2_clean4_maindata_responses.csv")%>%
  glimpse()



# summarize zip codes > counts ------------
zip_counts <- d1 %>% 
  filter(!is.na(q2_zip_code)) %>% 
  mutate(zip_clean = as.numeric(str_pad(as.character(q2_zip_code), width = 5, side = "left", pad = "0"))) %>% 
  count(zip_clean, name = "count")%>%
  glimpse()
# view(zip_counts)


# Join spatial data with counts ------------
map_data <- d0 %>% 
  left_join(zip_counts, by = c("zip_code" = "zip_clean"))

# bin data - a few very large outliers so need to manually bin to see all values
map_data_binned <- map_data %>% 
  mutate(
    count_cat = case_when(
      is.na(count) | count == 0 ~ "0",
      count == 1 ~ "1",
      count == 2 ~ "2",
      count == 3 ~ "3",
      count >= 4  & count <= 10 ~ "4–10",
      count >= 11 & count <= 50 ~ "11–50",
      count > 50                ~ ">50"
    ),
    # Levels ordered ascending so '0' is at the bottom when reversed in guide_legend
    count_cat = factor(
      count_cat, 
      levels = c("0", "1", "2", "3", "4–10", "11–50", ">50")
    )
  )

# colors
sunset_colors <- sequential_hcl(6, "SunsetDark", rev = TRUE)


# map --------------------
ggplot() +
  # Base Layer: ZIP Code Polygons
  geom_sf(
    data = map_data_binned, 
    aes(fill = count_cat), 
    color = "white", 
    linewidth = 0.05
  ) +
  scale_fill_manual(
    values = c(
      ">50"   = sunset_colors[6],
      "11–50" = sunset_colors[5],
      "4–10"  = sunset_colors[4],
      "3"     = sunset_colors[3],
      "2"     = sunset_colors[2],
      "1"     = sunset_colors[1],
      "0"     = "grey92"
    ),
    name = "No.\nResponses",
    breaks = c(">50", "11–50", "4–10", "3", "2", "1", "0")
  ) +
  
  # 2. Reset the fill scale so MPAs get their own distinct legend box
  new_scale_fill() +
  
  # 3. Layer MPAs on top with transparency (alpha = 0.5)
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
    pad_x = unit(1.3, "in"), 
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
    # text_col = "transparent"
  ) +
  
  # Scale Bar Text Annotations
  # annotate(
  #   "text", 
  #   x = I(0.26), y = I(0.08), 
  #   label = "  0                    100", 
  #   size = 2.8, 
  #   color = "black"
  # ) +
  # annotate(
  #   "text", 
  #   x = I(0.26), y = I(0.04), 
  #   label = "Miles", 
  #   size = 2.8, 
  #   color = "black"
  # ) +
  # 
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
  
  # Theme Settings & Legend Stacking
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.12, 0.22),
    legend.box = "vertical", # Stacks MPA box cleanly under No. Responses
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.05, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("./doc/mwis_map_zip_codes.png", width = 9, height = 9, dpi = 300)



