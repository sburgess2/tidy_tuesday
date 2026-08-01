###################
## Load libraries##
###################

library(tidyverse)
library(ggtext)
library(glue)
library(camcorder)
library(sbBrand)
library(sf)
library(rnaturalearth)
library(paletteer)
library(systemfonts)
library(ragg)
library(shadowtext)

###############################################################################

##############
## Load data##
##############

tuesdata <- tidytuesdayR::tt_load("2026-03-17")
monthly_losses_data <- tuesdata$monthly_losses_data
monthly_mortality_data <- tuesdata$monthly_mortality_data

#write_csv(monthly_losses_data, "2026/20260317-salmon/data/monthly_losses.csv")
#write_csv(monthly_mortality_data,"2026/20260317-salmon/data/monthly_mortality.csv")

monthly_mortality_data <- read_csv(
  "2026/20260317-salmon/data/monthly_mortality.csv"
)

###############################################################################

################
# Look at data##
################

str(monthly_mortality_data)
head(monthly_mortality_data)
glimpse(monthly_mortality_data)
monthly_mortality_data[!complete.cases(monthly_mortality_data), ]


###############################################################################

#############################
## Load fonts ##
#############################

require_font("Lato")
require_font("Baloo 2")

font <- "Lato"
brand_font <- "Baloo 2"

###############################################################################

###################
## Define colours##
###################

title_col <- "#131313"
#bg_col <- "#F5F5F5"
#text_col <- "#3b3b3b"
#label_col <- "#636363"
#brand_color <- "#55692f"

brand <- "#55692f"
accent <- "#a0bb3a"
light <- "#ddd69e"
ink <- "#000000"
grey <- "#B0B0B0"

###############################################################################

#################
## Social media##
#################

github_icon <- "&#xf09b"
github_username <- "sburgess2"
linkedin_icon <- "&#xf08c"
linkedin_username <- "sa-burgess2"

###############################################################################

###############################
## Titles, subtitles, captions##
###############################

plot_title <- "The 2025 median salmon mortality rate was highest in Møre og Romsdal and Trøndelag"

#subtitle <- glue(
# "(<span style='color:{col1}'> Label 1 </span> ",
#"<span style='color:{col2}'> Label 2 </span>)")

social_caption <- sb_social_caption(
  data_source = "Salmonid mortality dataset, Norwegian Veterinary Institute"
)

###############################################################################

###################
## Choropleth map##
###################

gg_record(
  device = "png",
  width = 6,
  height = 7.5,
  unit = "in",
  dpi = 300
)


#county_mortality <- mortality_typical_2025

salmon_county_mortality <- monthly_mortality_data |>
  filter(species == "salmon", geo_group == "county") |>
  mutate(year = year(date), month = month(date))

county_mortality <- salmon_county_mortality |>
  filter(year == 2025) |>
  summarise(mortality = median(median, na.rm = TRUE), .by = region) |>
  arrange(desc(mortality))

norway <- ne_states(country = "Norway", returnclass = "sf") |>
  mutate(
    county = case_when(
      name %in% c("Aust-Agder", "Vest-Agder", "Rogaland") ~ "Agder & Rogaland",
      name %in% c("Hordaland", "Sogn og Fjordane") ~ "Vestland",
      name %in% c("Sør-Trøndelag", "Nord-Trøndelag") ~ "Trøndelag",
      .default = name
    )
  ) |>
  group_by(county) |>
  summarise(.groups = "drop")

map_data <- norway |>
  left_join(county_mortality, by = c("county" = "region"))

ggplot(map_data) +
  geom_sf(aes(fill = mortality), colour = "white", linewidth = 0.2) +
  geom_shadowtext(
    data = ~ filter(.x, !is.na(mortality)),
    aes(label = county, geometry = geometry),
    stat = "sf_coordinates",
    colour = "white",
    bg.colour = "black",
    bg.r = 0.05,
    fontface = "bold",
    family = font,
    size = 2.5
  ) +
  scale_fill_gradient(
    low = light,
    high = brand,
    na.value = "grey85"
    #labels = scales::label_percent(accuracy = 0.1)
  ) +
  labs(
    fill = "Median mortality rate (%)",
    caption = social_caption,
    title = plot_title
  ) +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(7, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  coord_sf(xlim = c(4, 32), ylim = c(57, 72), expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(colour = ink),
    legend.text = element_text(colour = ink),
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = brand,
      hjust = 0,
      family = brand_font,
      margin = margin(t = 12)
    ),
    plot.title = element_textbox_simple(
      family = font,
      #colour = col1,
      face = "bold",
      size = rel(1.2),
      margin = margin(b = 8)
    )
  )

record_polaroid()

###############################################################################

#############
## Save png##
#############

ggsave(
  filename = "2026/20260317-salmon/20260317-salmon.png",
  width = 8,
  height = 6,
  units = "in",
  #bg = bg_col,
  dpi = 300
)
