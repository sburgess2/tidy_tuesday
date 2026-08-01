# Template adapted from Nicola Rennie https://www.r-bloggers.com/2023/08/creating-template-files-with-r/

#################
## Load packages##
#################

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(sysfonts)
library(paletteer)

###############################################################################

#############
## Load data##
#############

tuesdata <- tidytuesdayR::tt_load("2025-10-14")

food_security <- tuesdata$food_security

###############################################################################

#############################
# Load fonts using sysfonts##
#############################

font_add_google(name = "Lato", family = "lato")
font_add_google(name = "Baloo 2", family = "baloo")
font_add(
  family = "Font Awesome 7 Brands",
  regular = "fonts/fontawesome-free-7.1.0/otfs/Font-Awesome-7-Brands-Regular-400.otf"
)

# enables showtext package to automatically render text
showtext_auto()
showtext_opts(dpi = 300)

font <- "lato"
brand_font <- "baloo"

###############################################################################

##################
## Define colours##
##################

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
brand_color <- "#55692f"
fcol <- "#EEB05AFF"
mcol <- "#87AFD1FF"

###############################################################################

################
## Social media##
################

github_icon <- "&#xf09b"
github_username <- "sburgess2"
linkedin_icon <- "&#xf08c"
linkedin_username <- "sara-burgess"

###############################################################################

##################
## Data wrangling##
##################

areas_of_interest <- c(
  "World", "Africa", "Asia", "Northern America", "Latin America and the Caribbean", "Europe", "Australia and New Zealand",
  "Oceania excluding Australia and New Zealand"
)

food_sub <- food_security %>%
  select(Year_Start, Year_End, Area, Item, Value) %>%
  filter(Item %in% unique(food_security$Item)[10:21]) %>%
  subset(Area %in% areas_of_interest)

# Filter for prevalence items
food_prevalence <- food_sub %>%
  filter(grepl("prevalence", Item, ignore.case = TRUE))

# Tidy up field names e.g replace "Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)" with "Total food insecurity"
food_prevalence2 <- food_prevalence %>%
  filter(Item %in% unique(food_prevalence$Item)[5:6]) %>%
  mutate(Item = recode(Item,
    "Prevalence of moderate or severe food insecurity in the female adult population (percent) (3-year average)" = "Female food insecurity",
    "Prevalence of moderate or severe food insecurity in the male adult population (percent) (3-year average)" = "Male food insecurity",
    "Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)" = "Total food insecurity"
  )) %>%
  mutate(Area = recode(Area,
    "Oceania excluding Australia and New Zealand" = "Oceania"
  ))

food_prevalence2022 <- food_prevalence2 %>%
  filter(Year_End == 2024)

food_prevalence2022 <- food_prevalence2022 %>%
  mutate(
    Area = str_replace_all(Area, "Australia and New Zealand", "Australia\nNew Zealand"),
    Area = str_replace_all(Area, "Latin America and the Caribbean", "Latin America\nCaribbean"),
    Area = str_replace_all(Area, "Northern America", "North\nAmerica")
  )


Males <- food_prevalence2022 %>%
  filter(Item == "Male food insecurity")


Females <- food_prevalence2022 %>%
  filter(Item == "Female food insecurity")

###############################################################################

###############################
## Titles, subtitles, captions##
###############################

plot_title <- "Is there a difference in the prevalence of moderate or severe food insecurity between females and males?"

subtitle <- glue("One measure of food insecurity from the Food and Agriculture Organization of the United Nations (FAO) is the prevalence of moderate or severe food insecurity in a population. Data from 2022-2024 reveals regional variations worldwide, with some regions showing differences between <span style = 'color:{fcol}'>females </span> and <span style = 'color:{mcol}'>males</span>.")

social_caption <- glue(
  "Data: The Food and Agriculture Organization of the United Nations (FAO) <br>",
  "<img src='logo/GreenhoodLogoMark.png' width='20' height='14.84'/> <span style='color: {brand_color};'><strong> Sara Burgess</strong></span> |",
  "<span style='font-family:\"Font Awesome 7 Brands\";'>{github_icon};</span> ",
  "<span style='color: #000000'>{github_username}</span> | ",
  "<span style='font-family:\"Font Awesome 7 Brands\";'>{linkedin_icon};</span> ",
  "<span style='color: #000000'>{linkedin_username}</span>"
)

###############################################################################

#########
## Plot##
#########

# The following code was adapted from https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html

p_basic <- ggplot(food_prevalence2022) +
  geom_segment(
    data = Males,
    aes(
      x = Value, y = Area,
      yend = Females$Area, xend = Females$Value
    ),
    color = "#d3d3d3",
    size = 2
  ) +
  geom_point(aes(x = Value, y = Area, color = Item),
    size = 4
  )

p_basic

p_caption <- p_annotated +
  labs(
    caption = social_caption,
    title = plot_title,
    x = "Prevalence (%, 3-year average)",
    subtitle = subtitle
  )

# This colour palette was used
paletteer_d("tvthemes::AirNomads")

p_styled <- p_caption +
  scale_color_manual(values = c("#EEB05AFF", "#87AFD1FF")) +
  theme_minimal(base_family = font, base_size = 10) +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(10, 15, 10, 10),
    panel.background = element_rect(fill = bg_col, color = bg_col),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    panel.grid.minor = element_blank(),
    plot.caption = element_textbox_simple(
      color = text_col, hjust = 0,
      margin = margin(t = 10)
    ),
    plot.title = element_textbox_simple(color = text_col, face = "bold"),
    plot.subtitle = element_textbox_simple(
      color = text_col,
      margin = margin(t = 5)
    ),
    axis.text = element_textbox_simple(color = text_col),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank()
  )

annot_female <- food_prevalence2022 %>%
  filter(Year_End == 2024 & Area == "Latin America\nCaribbean" & Item == "Female food insecurity")
annot_female

annot_male <- food_prevalence2022 %>%
  filter(Year_End == 2024 & Area == "Latin America\nCaribbean" & Item == "Male food insecurity")
annot_male

p_annotated <- p_styled +
  geom_text(
    data = annot_female,
    mapping = aes(
      y = Area, x = Value + 1,
      label = glue("Female")
    ),
    family = font,
    color = "#EEB05AFF",
    size = 3,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text(
    data = annot_male,
    mapping = aes(
      y = Area, x = Value - 1,
      label = glue("Male")
    ),
    family = font,
    color = "#87AFD1FF",
    size = 3,
    hjust = 0.5,
    vjust = 0.5
  )

###############################################################################

############
## Save png##
############

ggsave(
  filename = "2025/20251014-foodday/20251014-foodday.png", p_annotated,
  width = 16, height = 16 * 1.414,
  units = "cm",
  bg = bg_col,
  dpi = 300
)
