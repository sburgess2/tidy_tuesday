# Adapted from Nicola Rennie https://www.r-bloggers.com/2023/08/creating-template-files-with-r/

###################

## Load libraries##

###################

library(tidyverse)
# library(showtext)
library(ggtext)
library(glue)

# library(sysfonts)

library(systemfonts)
library(ragg)
library(sbBrand)

# library(gridExtra)

# library(paletteer)

###############################################################################

##############
## Load data##
##############

tuesdata <- tidytuesdayR::tt_load("2026-02-17")

agstats <- tuesdata$dataset

write_csv(agstats, "2026/20260217-agstats/data/agstats.csv")

agstats <- read_csv("2026/20260217-agstats/data/agstats.csv")


###############################################################################

################
# Look at data##
################

str(agstats)

head(agstats)

glimpse(agstats)

unique(agstats$value_label)

agstats[!complete.cases(agstats), ]


cattle <- agstats |>

  filter(value_label == "Number of cattle")


## Faceted histogram

ggplot(cattle, aes(x = value)) +

  geom_histogram(
    binwidth = 100,

    center = 50,

    colour = "black",

    fill = "darkseagreen2"
  ) +

  facet_wrap(~measure, scales = "free")


dairy <- cattle |>
  filter(str_detect(measure, regex("dairy", ignore_case = TRUE)))


## Faceted line plot to look at trends

ggplot(data = dairy, aes(x = year_ended_june, y = value)) +

  geom_line() +

  facet_wrap(~measure, scales = "free") +

  labs(
    title = "Dairy Production Over Time",
    x = "Year Ended June",
    y = "Value"
  ) +

  theme_minimal()


## filter dairy df for in Milk and Sheep

dairy_sub <- agstats |>
  filter(
    measure %in%
      c(
        "Dairy Cows and Heifers, in Milk or Calf",
        "Total Sheep",
        "Total Area of Farms"
      )
  ) |>
  select(year_ended_june, measure, value) |>
  pivot_wider(names_from = "measure", values_from = "value") |>
  drop_na() |>
  mutate(
    dairycows_per_km2 = `Dairy Cows and Heifers, in Milk or Calf` /
      `Total Area of Farms` *
      100,
    sheep_per_km2 = `Total Sheep` /
      `Total Area of Farms` *
      100
  ) |>
  select(year_ended_june, dairycows_per_km2, sheep_per_km2)


#extract row maximum number of dairycows_per_km2
dairy_max <- dairy_sub |> slice_max(dairycows_per_km2)


###############################################################################

#############################
# Load fonts using sysfonts##
#############################

require_font("IBM Plex Sans")
require_font("Baloo 2")


font <- "IBM Plex Sans"

brand_font <- "Baloo 2"


###############################################################################

###################
## Define colours##
###################

bg_col <- "white"

text_col <- "#3b3b3b"

annotate_col <- "#909090"

brand_color <- "#55692f"

col1 <- "#09559C"

col2 <- "#2BACE2"

col3 <- "#7BC144"

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

plot_title <- "The density of milk-producing dairy cattle has slowly decreased since 2016"
subtitle <- "Average number of dairy cows per km<sup>2</sup> of farmland"

# subtitle <- glue(" (<span style = 'color:{col1}'> </span> Colour 1 label here <span style = 'color:{col2}'> Colour 2 label here </span>

# ")

social_caption <- sb_social_caption(data = "StatsNZ")

###############################################################################

#########

## Plot##

#########

# I have followed the same process as Nicola Rennie for building up a plot

library(camcorder)
gg_record(
  dir = "2026/20260217-agstats/output/",
  device = png,
  width = 6,
  height = 6,
  dpi = 300,
  units = "in"
)

p_basic <- ggplot(
  data = dairy_sub,
  aes(x = year_ended_june, y = dairycows_per_km2)
) +
  geom_line(color = col2, linewidth = 1)

p_basic

p_text <- p_basic +
  labs(
    caption = social_caption,
    title = plot_title,
    subtitle = subtitle,
    x = NULL,
    y = NULL
  )

p_text

p_text_notitle <- p_basic +
  labs(
    #caption = social_caption,
    #title = plot_title,
    subtitle = subtitle,
    x = NULL,
    y = NULL
  )

p_styled <- p_text +
  # scale_color_manual(values = c("", "")) +
  theme_minimal(
    base_size = 10
    #base_family = font,
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(10, 15, 10, 10),
    plot.caption = element_markdown(
      color = annotate_col,
      hjust = 0,
      family = brand_font,
      margin = margin(t = 12)
    ),
    plot.title = element_textbox_simple(
      family = font,
      colour = col1,
      face = "bold",
      size = rel(1.2),
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      colour = text_col,
      family = font,
      margin = margin(t = 4)
    ),
    axis.text = element_markdown(colour = annotate_col, family = font),
    axis.title = element_markdown(colour = text_col, family = font),
    # plot.subtitle = element_textbox_simple(
    # color = text_col,
    # margin = margin(t = 5)),
    panel.grid.major = element_blank(),
    #panel.background = element_rect(fill = bg_col, color = bg_col),
    axis.ticks.y = element_blank()
  ) +
  #geom_area(
  #  data = dairy_sub |> filter(year_ended_june > 2015),
  # aes(x = year_ended_june, y = dairycows_per_km2),
  #fill = col2,
  #alpha = 0.3) +
  scale_y_continuous(
    breaks = seq(0, 40, by = 5),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(clip = "off")
#geom_segment(
# data = dairy_max,
#linetype = "dotted",
#lineend = "round",
#color = col2,
#aes( x = year_ended_june, xend = year_ended_june,y = 1,
#yend = dairycows_per_km2))
#geom_vline(xintercept = dairy_max$year_ended_june, linetype = "dashed")

p_styled

p_styled <- p_text_notitle +
  # scale_color_manual(values = c("", "")) +
  theme_minimal(
    base_size = 10
    #base_family = font,
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(10, 15, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col,
      hjust = 0,
      family = brand_font,
      margin = margin(t = 12)
    ),
    plot.title = element_textbox_simple(
      family = font,
      colour = col1,
      face = "bold",
      size = rel(1.2),
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      colour = text_col,
      family = font,
      margin = margin(t = 4)
    ),
    axis.text = element_markdown(colour = annotate_col, family = font),
    axis.title = element_markdown(colour = text_col, family = font),
    panel.grid.major = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, by = 5),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(clip = "off")

p_annotated <- p_styled +
  geom_line(
    data = dairy_sub |> filter(year_ended_june >= dairy_max$year_ended_june),
    aes(x = year_ended_june, y = dairycows_per_km2),
    color = col1,
    linewidth = 1
  ) +
  geom_point(
    data = dairy_max,
    aes(x = year_ended_june, y = dairycows_per_km2),
    color = col1,
    size = 2
  ) +
  geom_textbox(
    data = dairy_sub |> filter(year_ended_june == 1984),
    mapping = aes(
      y = I(0.2),
      x = 1984,
      label = glue("Removal of agricultural subsidies")
    ),
    family = font,
    color = annotate_col,
    #alpha = 0.8,
    size = 2.8224,
    hjust = 0.5,
    vjust = 1,
    halign = 0.5,
    width = unit(2, "cm"),
    box.colour = NA,
    fill = NA
  ) + # Use vjust instead of halign for geom_text
  annotate(
    geom = "curve",
    x = 1984,
    xend = 1984,
    y = I(0.2),
    yend = 11.7,
    linewidth = 0.3,
    color = annotate_col,
    curvature = 0
    # arrow = arrow(
    #  length = unit(1.5, "mm"),
    # type = "closed"
    #)
  ) +
  geom_textbox(
    data = dairy_sub |> filter(year_ended_june == 1995),
    mapping = aes(
      y = I(0.55),
      x = 1995,
      label = glue("Expansion of dairy farming in the South Island")
    ),
    family = font,
    color = annotate_col,
    size = 2.8224,
    hjust = 0.5,
    vjust = 0,
    halign = 0.5,
    width = unit(3, "cm"),
    box.colour = NA,
    fill = NA
  ) +
  annotate(
    geom = "curve",
    x = 1995,
    xend = 1995,
    y = I(0.55),
    yend = dairy_sub$dairycows_per_km2[dairy_sub$year_ended_june == 1995],
    linewidth = 0.3,
    color = annotate_col,
    curvature = 0
  ) +
  geom_textbox(
    data = dairy_sub |> filter(year_ended_june == 2015),
    mapping = aes(
      y = I(0.75),
      x = 2015,
      label = glue("2015/2016 low milk payout from Fonterra")
    ),
    family = font,
    color = annotate_col,
    size = 2.8224,
    hjust = 0.5,
    vjust = 0.5,
    halign = 0.5,
    width = unit(3, "cm"),
    box.colour = NA,
    fill = NA
  ) +
  annotate(
    geom = "curve",
    x = 2015,
    xend = 2015,
    y = I(0.8),
    yend = dairy_sub$dairycows_per_km2[dairy_sub$year_ended_june == 2015],
    linewidth = 0.3,
    color = annotate_col,
    curvature = 0
  ) +
  #geom_textbox(
  # data = dairy_sub |> filter(year_ended_june == 2013),
  #mapping = aes(
  # y = I(0.8),
  #x = 2013,
  #x = 2010,
  #label = glue(
  # "The Sustainable Dairying: Water Accord resulted in stock being excluded around waterways")),
  #family = font,color = annotate_col,size = 1.5,hjust = 0.5,
  #vjust = 0.5,halign = 0.5,width = unit(4, "cm"),box.colour = NA,fill = NA)+
  geom_textbox(
    data = dairy_sub |> filter(year_ended_june == 2015),
    mapping = aes(
      y = I(1),
      x = 2015,
      label = glue("The maximum dairy cattle density peaked in 2016")
    ),
    family = font,
    color = annotate_col,
    size = 2.8224,
    hjust = 0.5,
    vjust = 0.5,
    halign = 0.5,
    width = unit(3, "cm"),
    box.colour = NA,
    fill = NA
  ) +
  annotate(
    geom = "curve",
    x = 2015,
    xend = dairy_max$year_ended_june,
    y = I(0.92),
    yend = dairy_max$dairycows_per_km2,
    linewidth = 0.3,
    color = annotate_col,
    curvature = 0
  )

p_annotated

#record_polaroid()
gg_playback(
  name = "2026/20260217-agstats/output/agstats_recording.gif",
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = 0.25,
  image_resize = 800
)

gg_stop_recording()

###############################################################################

#############

## Save png##

#############

ggsave(
  filename = "2026/20260217-agstats/20260217-agstats.png",
  p_annotated,
  device = ragg::agg_png,
  width = 6,
  height = 6,
  bg = bg_col,
  dpi = 300
)

############
##Function##
############

select_measure <- "Dairy Cows and Heifers, in Milk or Calf"

plot_agstats <- function(select_measure) {
  agstats_animal <- agstats |>
    filter(measure %in% c(select_measure, "Total Area of Farms")) |>
    select(year_ended_june, measure, value) |>
    pivot_wider(names_from = "measure", values_from = "value") |>
    drop_na() |>
    mutate(
      animals_per_km2 = .data[[select_measure]] / `Total Area of Farms` * 100
    ) |>
    select(year_ended_june, animals_per_km2)

  p <- ggplot(
    data = agstats_animal,
    aes(x = year_ended_june, y = animals_per_km2)
  ) +
    geom_line(color = col2, linewidth = 1)

  return(p)
}

plot_agstats("Dairy Cows and Heifers, in Milk or Calf")
