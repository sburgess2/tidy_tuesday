# Adapted from Nicola Rennie https://www.r-bloggers.com/2023/08/creating-template-files-with-r/

###################
## Load libraries##
###################

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(sysfonts)
library(gridExtra)
library(paletteer)
library(rvest)
library(patchwork)

###############################################################################

##############
## Load data##
##############

tuesdata <- tidytuesdayR::tt_load("2026-02-03")
edible_plants <- tuesdata$edible_plants
write_csv(edible_plants, "2026/20260203-edible_plants/data/edible_plants.csv")
edible_plants <- read_csv("2026/20260203-edible_plants/data/edible_plants.csv")

calendar <- read_html("https://www.plantwhatwhen.com/nz-temperate-calendar.html")

df_calendar <- calendar %>%
  html_elements("tbody") %>%
  html_table()

calendar %>%
  html_elements("table") %>%
  html_table()

df_calendar <- df_calendar[[1]]

write_csv(df_calendar, "2026/20260203-edible_plants/data/vege_calendar.csv")
df_calendar <- read_csv("2026/20260203-edible_plants/data/vege_calendar.csv")
###############################################################################

##################
# Data wrangling##
##################

## Edible plant data wrangling
#-----------------------------

edible_tidy <- edible_plants %>%
  select(
    taxonomic_name, common_name, cultivation, temperature_class,
    temperature_growing, days_harvest
  ) %>%
  filter(
    !cultivation == "Miscellaneous",
    !days_harvest == "continual"
  ) %>%
  drop_na() %>%
  separate(temperature_growing,
    into = c("temp_min", "temp_max"),
    sep = "-", fill = "right", remove = FALSE
  ) %>%
  separate(days_harvest,
    into = c("days_min", "days_max"),
    sep = "-", fill = "right", remove = FALSE
  ) %>%
  mutate(
    temp_min = as.numeric(temp_min),
    days_min = as.numeric(days_min)
  ) %>%
  mutate(cultivation_no = case_when(
    cultivation == "Legume" ~ 1,
    cultivation == "Brassica" ~ 2,
    cultivation == "Cucurbit" ~ 3,
    cultivation == "Salad" ~ 4,
    cultivation == "Solanaceae" ~ 5,
    cultivation == "Umbelliferae" ~ 6,
    cultivation == "Allium" ~ 7,
    cultivation == "Solanum" ~ 8,
    cultivation == "Other" ~ 9
  )) %>%
  mutate(common_name = recode(common_name,
    "Pea" = "Peas",
    "Beans (Runner)" = "Beans climbing/snake",
    "Beans (French)" = "Beans dwarf/bush",
    "Beans (Broad)" = "Broad bean",
    "Cabbage (Spring)" = "Cabbage loose-headed",
    "Zucchini or Courgette" = "Zucchini",
    "Cucumber" = "Cucumber",
    "Melon" = "Rockmelon",
    "Water melon" = "Watermelon",
    "Lettuce (Headed)" = "Lettuce",
    "Artichoke (Globe)" = "Artichokes globe",
    "Aubergine" = "Aubergine",
    "Beans (Borlotti)" = "Beans dwarf/bush",
    "Beetroot" = "Beetroot",
    "Bok Choy" = "Bok Choy",
    "Cabbage (Autumn red)" = "Cabbage tight-headed",
    "Cabbage (Autumn)" = "Cabbage tight-headed",
    "Cabbage (Chinese)" = "Chinese cabbage",
    "Cabbage (Spring red)" = "Cabbage loose-headed",
    "Cabbage (Summer red)" = "Cabbage tight-headed",
    "Cabbage (Summer)" = "Cabbage tight-headed",
    "Chilli Pepper" = "Chilli peppers",
    "Fennel" = "Fennel",
    "Mangetout" = "Mangetout",
    "Onion" = "Onion",
    "Parsnip" = "Parsnip",
    "Bell Pepper" = "Bell peppers",
    "Potatoes (Main crop)" = "Potato",
    "Squash (Summer)" = "Summer squash",
    "Squash (Winter)" = "Squash",
    "Tomato (Regular)" = "Tomato"
  )) %>%
  distinct() %>%
  filter(!common_name == "Cabbage loose-headed" & !cultivation == "Legume")


## calendar dataframe wrangling
#--------------------------------

# Add column names

colnames(df_calendar) <- c(
  "common_name", "January", "February", "March",
  "April", "May", "June", "July", "August", "September",
  "October", "November", "December"
)

# Remove first row
df_calendar <- df_calendar[2:155, ]

# Change the calendar letters to numbers
calendar_tidy <- df_calendar %>%
  mutate(across(2:13, ~ if_else(. != ".", 1, 0)))

### Merge dataframes

df_all <- inner_join(edible_tidy, calendar_tidy, by = "common_name")

df_long <- df_all %>%
  select(common_name, cultivation, temp_min, days_min, cultivation_no, January:December) %>%
  pivot_longer(cols = c("January":"December"), names_to = "month", values_to = "value") %>%
  mutate(common_name = factor(common_name)) %>%
  mutate(month = factor(month, levels = month.name))

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

font_add(
  family = "Font Awesome 7 Solid",
  regular = "fonts/fontawesome-free-7.1.0/otfs/Font-Awesome-7-Free-Solid-900.otf"
)

font_add(
  family = "FA Regular",
  regular = "fonts/fontawesome-free-7.1.0/otfs/Font-Awesome-7-Free-Regular-400.otf"
)

# enables showtext package to automatically render text
showtext_auto()
showtext_opts(dpi = 300)

font <- "lato"
brand_font <- "baloo"

###############################################################################

###################
## Define colours##
###################

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
brand_color <- "#55692f"
yel_col <- "#FED789FF"
blu_col <- "#A4BED5FF"
gr_col <- "#72874EFF"
bla_col <- "#023743FF"

dark_gr <- "#33691EFF"
light_gr <- "#7CB342FF"
###############################################################################

#################
## Social media##
#################

github_icon <- "&#xf09b;"
github_username <- "sburgess2"
linkedin_icon <- "&#xf08c;"
linkedin_username <- "sara-burgess"

###############################################################################

###############################
## Titles, subtitles, captions##
###############################

plot_title <- "New Zealand vegetable calendar"

# subtitle <- glue(" (<span style = 'color:{col1}'> </span> Colour 1 label here <span style = 'color:{col2}'> Colour 2 label here </span>
# ")

# Code adapted from Nicole Rennie's blog https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/
social_caption <- glue(
  "Data: The Edible Plant Database and ThePlantWhatWhen New Zealand temperate sowing calendar <br>",
  "<img src='logo/GreenhoodLogoMark.png' width='18' height='13.5'/> <span style='color: {brand_color};'><strong> Sara Burgess</strong></span> |",
  "<span style='font-family:\"Font Awesome 7 Brands\";'>{github_icon}</span> ",
  "<span style='color: #000000'>{github_username}</span> | ",
  "<span style='font-family:\"Font Awesome 7 Brands\";'>{linkedin_icon};</span> ",
  "<span style='color: #000000'>{linkedin_username}</span>"
)

# Wrap the subtile so it doesn't go off the plot
# subtitle_wrapped <- str_wrap(subtitle, width = 180)

###############################################################################

#########
## Plot##
#########


# Bar chart
veg_levels <- sort(unique(df_long$common_name))
veg_levels_rev <- rev(veg_levels)

plot_title <- "New Zealand vegetable calendar"

p_bar <- ggplot(
  df_all,
  aes(
    x = factor(common_name, levels = veg_levels_rev),
    y = days_min
  )
) +
  geom_col(fill = light_gr) +
  geom_text(aes(label = days_min), hjust = 1.5, size = 2, color = "#ffffff") +
  theme_minimal(base_family = font, base_size = 7) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    # plot.margin = margin(5, 10, 0, 10)
    # plot.margin = margin(5, 20, 5, 0.1)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.8))) +
  coord_flip()


p_bar

celery <- df_all %>%
  filter(common_name == "Celery")
p_bar_annotated <- p_bar +
  geom_textbox(
    data = celery,
    aes(
      x = "Celery", y = 104,
      label = "Potatoes and artichokes require 100 days of growing to harvest"
    ),
    color = "#3b3b3b",
    size = 1.5,
    width = unit(2, "cm"),
    fill = "#B3B7B8FF",
    box.color = "#B3B7B8FF",
    hjust = 0.1, # Aligns box to the left or right against it's coordinates
    vjust = 0.5, # Aligns box to the top or bottom against it's coordinates, where vjust = 1 aligns top of box with y coordinate
    valign = 0.5, # Moves text from bottom (0) to top (1) where 0.5 is centred
    halign = 0.5, # Moves text left (0) to right (1) within box
    inherit.aes = FALSE,
    family = font
  ) +
  geom_curve(
    aes(
      x = "Celeriac", xend = "Artichokes globe",
      y = 104, yend = 100
    ),
    linewidth = 0.3,
    color = "#B3B7B8FF",
    curvature = -0.3, # Negative for left curve, positive for right
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  geom_curve(
    aes(
      x = "Chicory", xend = "Potato",
      y = 104, yend = 100
    ),
    linewidth = 0.3,
    color = "#B3B7B8FF",
    curvature = 0.3, # Negative for left curve, positive for right
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    inherit.aes = FALSE
  )

p_bar_annotated

# Heatmap
# The following chart was adapted from https://stackoverflow.com/questions/13887365/circular-heatmap-that-looks-like-a-donut

p_heat <- ggplot(
  df_long,
  aes(
    x = factor(common_name, levels = veg_levels_rev),
    y = month,
    fill = factor(value)
  )
) +
  geom_tile(color = "white", show.legend = FALSE) +
  coord_equal() +
  theme_minimal(base_family = font, base_size = 7) +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5")
  ) +
  scale_y_discrete(
    labels = function(x) substr(x, 1, 3),
    position = "right"
  ) +
  scale_fill_discrete(palette = c("#F5F5F5", dark_gr)) +
  coord_flip()
p_heat

fennel <- df_all %>%
  filter(common_name == "Fennel")

lettuce <- df_all %>% filter(common_name == "lettuce")

p_heat_annotated <- p_heat +
  geom_text(
    data = fennel,
    aes(
      x = "Fennel", y = "January",
      label = "Lettuce can be harvested year round"
    ),
    color = "#3b3b3b",
    size = 1.5,
    # fill = NA,
    # box.color = NA,
    hjust = 0.1,
    vjust = 0.5,
    inherit.aes = FALSE,
    family = font
  ) +
  geom_curve(
    aes(
      x = "Fennel", xend = "Lettuce",
      y = "January", yend = "February"
    ),
    linewidth = 0.3,
    color = light_gr,
    curvature = -0.3, # Negative for left curve, positive for right
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    inherit.aes = FALSE
  )
p_heat_annotated

colours <- c("#A4BED5FF", "#FED789FF")
names(colours) <- c("4°C", "26°C")

square <- "\uf0c8"
# square <- "\u25A0"

subtitle <- glue(
  "Minimum growth temperature: ",
  '<span style = "color:{colours["4°C"]} ">',
  "**4°C**",
  " </span>",
  "<span style = 'font-family:\"Font Awesome 7 Solid\";color:#A4BED5FF;'>{square};</span> ",
  "to ",
  '<span style = "color:{colours["26°C"]}">**26°C**</span>',
  ' <span style="font-family:\'Font Awesome 7 Solid\'; color:#FED789FF;">{square}</span> ',
  "&nbsp;&nbsp;|&nbsp;&nbsp; ",
  "Month to sow",
  " <span style = 'font-family:\"Font Awesome 7 Solid\";color:#33691EFF;'>{square};</span> ",
  "&nbsp;&nbsp;|&nbsp;&nbsp; ",
  "Days to harvest",
  " <span style = 'font-family:\"Font Awesome 7 Solid\";color:#7CB342FF;'>{square};</span> ",
)


p_heat_temp <- ggplot(
  df_long,
  aes(
    x = factor(1),
    y = factor(common_name, levels = veg_levels_rev),
    fill = temp_min
  )
) +
  geom_tile(color = "white", show.legend = FALSE) +
  # labs(fill = "Minimum growth temperature") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = blu_col, high = yel_col) +
  coord_fixed(ratio = 1, clip = "off") +
  theme_minimal(base_family = font, base_size = 7) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5),
    axis.title = element_blank(),
    # plot.margin = margin(5, 0.1, 5, 5),
    legend.position = "top",
    legend.direction = "vertical"
  )

p_heat_temp

watermelon <- df_all %>%
  slice_max(temp_min)

lettuce <- df_all %>%
  slice_min(temp_min)

p_annotated_temp <- p_heat_temp +
  geom_textbox(
    data = watermelon,
    aes(
      x = 1.5, y = common_name,
      label = "**Watermelon**<br>Minimum 26°C to grow"
    ),
    color = "#3b3b3b",
    size = 1.5, # Text size
    width = unit(1.4, "cm"), # width of box including margin
    fill = alpha(yel_col, 0.8),
    box.color = yel_col,
    # box.padding = unit(c(2, 2, 2, 2), "pt"), #Specifies padding inside box
    hjust = 2.2,
    halign = 0.5,
    inherit.aes = FALSE,
    family = font
  ) +
  geom_textbox(
    data = lettuce,
    aes(
      x = 1.5, y = common_name,
      label = "**Lettuce**<br>Minimum 4°C to grow"
    ),
    color = "#3b3b3b",
    size = 1.5,
    width = unit(1.4, "cm"),
    fill = alpha(blu_col, 0.8),
    box.color = blu_col,
    # box.padding = unit(c(2, 2, 2, 2), "pt"),
    hjust = 2.2,
    halign = 0.5,
    inherit.aes = FALSE,
    family = font
  )

p_annotated_temp


p_join <- (p_annotated_temp | p_heat_annotated | p_bar_annotated) &
  theme(
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    axis.text = element_text(color = text_col, family = font),
    plot.title = element_text(family = font, size = 12, face = "bold"),
    plot.subtitle = element_markdown(family = font, size = 7, lineheight = 1.2),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(family = font, size = 5, hjust = 0),
    text = element_text(family = font),
    # plot.margin = margin(5,5,5,5)
  )

p_join <- p_join +
  plot_layout(widths = c(1, 6, 4)) +
  plot_annotation(
    title = "New Zealand vegetable calendar",
    subtitle = subtitle,
    caption = social_caption
  )
p_join


###############################################################################

###############
## Save image##
###############

ggsave(
  filename = "2026/20260203-edible_plants/20260203-edible_plants.png",
  plot = p_join,
  width = 7, height = 5,
  bg = bg_col,
  dpi = 300,
  device = png # Note annotations don't format correctly using device = ragg::agg_png
)

ggsave(
  "2026/20260203-edible_plants/edible_plants.svg",
  plot = p_join,
  width = 7, height = 5,
  bg = bg_col
)
