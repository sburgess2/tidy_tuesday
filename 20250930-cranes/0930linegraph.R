# loaded libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(patchwork)
library(ggimage)
library(magick)
library(ggtext)
library(glue)
library(imager)
library(cowplot)

# loaded data
tuesdata <- tidytuesdayR::tt_load("2025-09-30")
cranes <- tuesdata$cranes

# looked at the data
str(cranes)
hist(cranes$observations)

# Looked at 2024 only data
cranes2024 <- cranes %>%
  select(date, observations) %>%
  drop_na() %>%
  filter(date > "2024-01-01")


ggplot(data = cranes2024, mapping = aes(x = date, y = observations)) +
  geom_line()

# tidied up data by adding additional columns for year, day of the month, month
# filtered for those observations greater than 0
cranes_years <- cranes %>%
  mutate(year = year(date)) %>%
  mutate(year_day = yday(date)) %>%
  mutate(month_day = mday(date)) %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
  filter(observations > 0)

# Note there were 0 instead of NA for some of the Canceled/No count rows

# Looked at all the data
ggplot(cranes_years, aes(x = year_day, y = observations, group = year)) +
  geom_line()

# Checked which months had data
unique(cranes_years$month)

# Generated spring data only
cranes_spring <- cranes_years %>%
  filter(month %in% c("March", "April")) %>%
  group_by(year) %>%
  select(year, observations, year_day) %>%
  drop_na() %>%
  summarise(mean = mean(observations), maximum = max(observations), minimum = min(observations))

# Generated autumn data only
cranes_autumn <- cranes_years %>%
  filter(month %in% c("August", "September", "October")) %>%
  group_by(year) %>%
  select(year, observations, year_day) %>%
  drop_na() %>%
  summarise(mean = mean(observations), maximum = max(observations), minimum = min(observations))

# Shout out here to Yann Holtz for the R color platte finder https://r-graph-gallery.com/color-palette-finder
# colour palette that I like
paletteer_d("impressionist.colors::fleurs_dans_un_vase_de_cristal")

# Shout out here to Nicola Rennie for her book "The art of data visualization with ggplot2"
# https://nrennie.rbind.io/art-of-viz/
# A lot of the code below for "beautifying" this plot has been adapted from her book

font_add_google(
  name = "Lato", family = "lato"
)

# enables showtext package to automatically render text
showtext_auto()
showtext_opts(dpi = 300)

font <- "lato"

# Generate spring plot
p1 <- ggplot(cranes_spring, aes(x = year, y = maximum)) +
  geom_ribbon(aes(ymin = minimum, ymax = maximum), fill = "#D2D2C3FF", alpha = 0.5) +
  geom_line(color = "#4B6996FF", linewidth = 1) +
  scale_y_log10(
    limits = c(1, 100000),
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10", "100", "1000", "10000", "100000")
  ) +
  scale_x_continuous(
    limits = c(1994, 2024), breaks = c(1995, 2000, 2005, 2010, 2015, 2020)
  ) +
  theme_minimal(base_family = font, base_size = 10) +
  labs(x = "Year", y = "Number of observations", title = "Spring") +
  theme(
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    # axis.title.y = element_text(margin = margin(r = 40)), # The space between y-axis title and numbers
    axis.title.x = element_text(margin = margin(t = 10))
  )

# Generate autumn plot
p2 <- ggplot(cranes_autumn, aes(x = year, y = maximum)) +
  geom_ribbon(aes(ymin = minimum, ymax = maximum), fill = "#D2D2C3FF", alpha = 0.5) +
  geom_line(color = "#4B6996FF", linewidth = 1) +
  scale_y_log10(
    limits = c(1, 100000),
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10", "100", "1000", "10000", "100000")
  ) +
  scale_x_continuous(limits = c(2002, 2024), breaks = c(2005, 2010, 2015, 2020)) +
  theme_minimal(base_family = font, base_size = 10) +
  labs(x = "Year", title = "Autumn") +
  theme(
    panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# Look at the two plots together using patchwork, p1 needs more space for the yaxis
p1 + p2 + plot_layout(widths = c(30, 22))

# Pull out lowest count
annot_low <- cranes_years %>%
  filter(month == "March") %>%
  slice_min(observations)
annot_low

# Pull out highest count
annot_high <- cranes_years %>%
  slice_max(observations)
annot_high

# Add some annotations to the spring plot
p1_annotated <- p1 +
  geom_textbox(
    data = annot_high,
    mapping = aes(
      x = year, y = observations + 30000,
      label = glue(
        "The highest number of cranes counted to date is {format(observations, big.mark = ',')}, on {month_day} {month} {year}."
      )
    ),
    family = font,
    alpha = 0.5,
    size = 3,
    halign = 0.5,
    hjust = 0.5,
    maxheight = unit(5, "lines"),
    maxwidth = unit(7, "lines")
  ) +
  geom_textbox(
    data = annot_low,
    mapping = aes(
      x = year, y = observations,
      label = glue(
        "The counts generally begin when numbers are greater than 100, but they are sometimes less than 10."
      )
    ),
    family = font,
    alpha = 0.4,
    size = 3,
    halign = 0.5,
    hjust = 0.5,
    maxheight = unit(5, "lines"),
    maxwidth = unit(7, "lines")
  )

# Title for entire plot
title <- "Crane counts at Lake Hornborgasjön"

subtitle <- "Cranes arrive in late February or early March at Lake Hornborgasjön before flying North to breed.They return in autumn before flying South. The cranes are counted by professional birdwatchers from approximately mid March during spring and from late August during autumn. For more information see https://www.hornborga.com/naturen/transtatistik/"

# Wrap the subtile so it doesn't go off the plot
subtitle_wrapped <- str_wrap(subtitle, width = 180)

## I got some help from Claude AI for this part, as other methods wouldn't work with patchwork
## or when I could get the image to insert the transparency changed to white when converted to raster
logo <- image_read("logo/SBurgess.png") %>%
  image_background("none") %>% # Remove existing background
  image_transparent("white") # Make white transparent

# Save with transparency
image_write(logo, "logo/logo_transparent.png", format = "png")

logo <- image_read("logo/logo_transparent.png")

# Combine plots with patchwork first then add the theme
combined_plot <- p1_annotated + p2 +
  plot_layout(widths = c(30, 22)) +
  plot_annotation(
    title = title,
    subtitle = subtitle_wrapped
  ) &
  theme(
    plot.title = element_text(family = font, size = 12, face = "bold"),
    plot.subtitle = element_text(family = font, size = 10),
    plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
    plot.margin = margin(5, 5, 20, 5)
  )


# Add logo with cowplot
final_plot <- ggdraw(combined_plot) +
  draw_image(logo, x = 0.01, y = 0.005, width = 0.12, height = 0.12, hjust = 0, vjust = 0)

final_plot

ggsave("2025/20250930-cranes/final_plot.png", final_plot, width = 12, height = 0.67 * 12, dpi = 300)
