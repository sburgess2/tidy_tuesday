# Adapted from Nicola Rennie https://www.r-bloggers.com/2023/08/creating-template-files-with-r/

###################
## Load libraries##
###################

library(tidyverse)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(sbBrand)

###############################################################################

##############
## Load data##
##############

tuesdata <- tidytuesdayR::tt_load("2026-07-28")

occurrences <- tuesdata$occurrences
tourism <- tuesdata$tourism
weather <- tuesdata$weather

###############################################################################

################
# Look at data##
################

str(occurrences)
str(weather)
head()
glimpse(occurrences)
unique(occurrences$organism_name)
df[!complete.cases(df), ]

mantaray <- occurrences |>
  filter(organism_name == "Manta ray")


ggplot(species, aes(hour)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(organism_name))

ggplot(species, aes(organism_name, hour)) +
  geom_boxplot()

species |>
  count(organism_name, month) |>
  ggplot(aes(month, n)) +
  geom_col() +
  facet_wrap(vars(organism_name))

ggplot(species, aes(organism_name, month)) +
  geom_boxplot()

species |>
  count(organism_name, year) |>
  ggplot(aes(year, n, colour = organism_name)) +
  geom_line()

species |>
  count(organism_name, obs_state) |>
  ggplot(aes(n, obs_state)) +
  geom_col() +
  facet_wrap(vars(organism_name), scales = "free")

ggplot(species, aes(obs_lon, obs_lat, colour = organism_name)) +
  geom_point(alpha = 0.5)

species |>
  left_join(weather, by = c("ws_id", "date")) |>
  ggplot(aes(organism_name, temp)) +
  geom_boxplot()

year_2020s <- c(2020:2025)
species_2020s <- mantaray |>
  filter(year %in% year_2020s & record_type == "HUMAN_OBSERVATION") |>
  select(year, month, hour, organism_name, record_type, ws_id)

counts <- species_2020s |>
  count(organism_name, month, hour)


###############################################################################

#############################
## Load fonts ##
#############################

require_font("Carter One")
require_font("Baloo 2")
require_font("Nunito")

register_variant(
  name = "Nunito Bold",
  family = "Nunito",
  weight = "bold"
)

font <- "Nunito"
title_font <- "Nunito Bold"
brand_font <- "Baloo 2"

###############################################################################

###################
## Define colours##
###################

title_col <- "#F5F5F5"
bg_col <- "#141414"
text_col <- "#e8e8e8"

label_col <- "#b0b0b0"
brand_color <- "#a3c057"
col1 <- ""
col2 <- ""

###############################################################################

#################
## Social media##
#################

github_icon <- "&#xf09b"
github_username <- "sburgess2"
linkedin_icon <- "&#xf08c"
linkedin_username <- "sara-burgess"

###############################################################################

###############################
## Titles, subtitles, captions##
###############################

plot_title <- "In Australia, manta ray sightings are most frequently reported in June."

subtitle <- "Number of human observations of manta rays from 2020 - 2024."

social_caption <- sb_social_caption(
  data_source = "Ecotourism R package",
  font_colour = "white",
  brand_colour = "white"
)

###############################################################################

#########
## Plot##
#########

library(camcorder)

gg_record(
  device = "png",
  width = 5,
  height = 6,
  unit = "in",
  dpi = 300
)

ggplot(counts, aes(x = month, y = hour, fill = n)) +
  geom_tile(color = bg_col, linewidth = 0.5) +
  #facet_wrap(vars(organism_name)) +
  scale_fill_gradient(low = "#FFCD00", high = "#00843D") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + #put breaks at each number 1-12 and replace numbers with labels - the months
  scale_y_continuous(
    breaks = seq(0, 24, 3),
    labels = \(h) sprintf("%02d:00", h) #\(h) shorthand for function(h) the vector of breaks h is passed into the function sprintf() which formats numbers to clock time
  ) +
  labs(
    caption = social_caption,
    title = plot_title,
    subtitle = subtitle,
    tag = "Time of day",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.location = "plot",
    legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0), #by default legend margins contain padding
    legend.box.spacing = unit(1, "cm"),
    legend.title = element_text(color = text_col, family = font),
    legend.text = element_text(color = label_col, family = font),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col,
      hjust = 0,
      family = brand_font,
      margin = margin(t = 10)
    ),
    plot.title = element_textbox_simple(
      color = title_col,
      family = font,
      size = rel(1.2)
    ),
    plot.subtitle = element_textbox_simple(
      color = text_col,
      family = font,
      #face = "italic",
      margin = margin(t = 5)
      #size = rel(0.9)
    ),
    plot.tag = element_text(
      color = text_col,
      family = font,
      size = rel(1),
      hjust = 0,
      vjust = 1
    ),
    plot.tag.position = c(0.0, 0.78),
    axis.text = element_text(color = label_col, family = font, size = rel(0.8)),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.4, "lines"),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    axis.ticks = element_blank()
  )

record_polaroid()
gg_stop_recording()

###############################################################################

#############
## Save png##
#############

ggsave(
  filename = "2026/20260728-ecotourism/20260728-ecotourism.png",
  width = 5,
  height = 6,
  unit = "in",
  bg = bg_col,
  dpi = 300
)
