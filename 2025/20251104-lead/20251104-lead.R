# Load packages -----------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(sysfonts)
library(gridExtra)
library(ggbeeswarm)
library(paletteer)

###############################################################################

#############
## Load data##
#############

tuesdata <- tidytuesdayR::tt_load("2025-11-04")

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
mcol <- "#88C0F8FF"
vcol <- "#D06040FF"

paletteer_d("palettetown::milotic")
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

flint_mdeq <- tuesdata$flint_mdeq %>%
  mutate(data_source = "MDEQ") %>%
  select(sample, lead, data_source, notes)
flint_vt <- tuesdata$flint_vt %>%
  mutate(data_source = "Virginia Tech")

df <- bind_rows(flint_mdeq, flint_vt) %>%
  mutate(point_color = case_when(
    data_source == "MDEQ" & !is.na(notes) ~ "Outlier",
    TRUE ~ (data_source)
  ))

write_csv(flint_mdeq, "2025/20251104-lead/data/flint_mdeq.csv")
write_csv(flint_vt, "2025/20251104-lead/data/flint_vt.csv")

summary(flint_mdeq$lead2)

summary(flint_vt$lead)

df_no_outlier <- df %>%
  filter(data_source == "MDEQ") %>%
  filter(if_any(notes, is.na))

df %>%
  group_by(data_source) %>%
  summarise(quantile(lead, probs = 0.9, na.rm = TRUE))

df_no_outlier %>%
  group_by(data_source) %>%
  summarise(quantile(lead, probs = 0.9, na.rm = TRUE))

df %>%
  group_by(data_source) %>%
  summarise(quantile(lead, probs = 0.9, na.rm = TRUE, type = 8))

df_no_outlier %>%
  group_by(data_source) %>%
  summarise(quantile(lead, probs = 0.9, na.rm = TRUE, type = 8))

###############################################################################

###############################
## Titles, subtitles, captions##
###############################

plot_title <- "The Flint water crisis - lead levels exceded the EPA standard."

subtitle <- glue("In April 2014, the city of Flint, Michigan changed its water supply. The Michigan Department of Environmental Quality (<span style = 'color:{mcol}'>MDEQ </span>) tested samples from 71 household taps (two samples were excluded because they did not meet the inclusion criteria) and deemed the water safe to drink. However, many Flint residents became ill with symptoms consistent with lead poisoning.
The Environmental Protection Agency (EPA) Lead and Copper standard requires action to be taken if the 90% percentile is greater than 15 parts per billion (ppb) - i.e. when more than 10% of households tested have a lead level of greater than 15 ppb. MDEQ's testing (when two samples were excluded) found that the 90th percentile of their sample was less than 15 ppb. <span style = 'color:{vcol}'>Virginia Tech</span> researchers conducted a citizen science project, collecting samples from 250 household taps. Their results showed that the 90th percentile  the 15 ppb limit, indicating the water supply breached EPA standards.
")

social_caption <- glue(
  "Data: Loux and Gibson (2018) <br>",
  "<img src='logo/GreenhoodLogoMark.png' width='18' height='13.5'/> <span style='color: {brand_color};'><strong> Sara Burgess</strong></span> |",
  "<span style='font-family:\"Font Awesome 7 Brands\";'>{github_icon};</span> ",
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

# I have followed the same process as Nicola Rennie for building up a plot
p_basic <- ggplot(data = df, aes(x = data_source, y = lead, color = point_color)) +
  annotate(
    geom = "rect",
    xmin = 0, xmax = 3,
    ymin = 0, ymax = 15,
    fill = "gray90"
  ) +
  geom_beeswarm(priority = "random", method = "compactswarm", alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c(
    "MDEQ" = "#88C0F8FF",
    "Virginia Tech" = "#D06040FF",
    "Outlier" = "darkgray"
  ))

p_text <- p_basic +
  labs(
    caption = social_caption,
    title = plot_title,
    subtitle = subtitle,
    x = "Data Source",
    y = "Lead level (ppb)"
  )

p_styled <- p_text +
  theme_minimal(base_family = font, base_size = 7) +

  stat_summary(
    data = df %>% filter(data_source == "MDEQ"),
    color = "darkgrey",
    fun = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.ymin = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.ymax = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    geom = "crossbar",
    linewidth = 0.3,
    width = 0.2
  ) +
  stat_summary(
    data = df %>% filter(data_source == "Virginia Tech"),
    color = "#D06040FF",
    fun = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.ymin = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.ymax = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    geom = "crossbar",
    linewidth = 0.3,
    width = 0.2
  ) +
  stat_summary(
    data = df_no_outlier,
    fun = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.min = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    fun.max = ~ quantile(.x, probs = 0.9, na.rm = TRUE, type = 8),
    geom = "crossbar",
    linewidth = 0.3,
    width = 0.2,
    color = "#88C0F8FF"
  ) +
  geom_hline(yintercept = 15, linetype = "dashed") +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.margin = margin(10, 15, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col, hjust = 0,
      margin = margin(t = 10)
    ),
    plot.title = element_textbox_simple(color = text_col, face = "bold"),
    plot.subtitle = element_textbox_simple(
      color = text_col,
      margin = margin(t = 5)
    ),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.3),
    panel.background = element_blank()
  )

p_styled

annot_vt <- df %>%
  group_by(data_source) %>%
  summarise(perc90 = quantile(lead, probs = 0.9, na.rm = TRUE, type = 8)) %>%
  filter(data_source == "Virginia Tech")
annot_vt

annot_outlier <- df %>%
  group_by(data_source) %>%
  summarise(perc90 = quantile(lead, probs = 0.9, na.rm = TRUE, type = 8)) %>%
  filter(data_source == "MDEQ")
annot_outlier

annot_no_outlier <- df_no_outlier %>%
  group_by(data_source) %>%
  summarise(perc90 = quantile(lead, probs = 0.9, na.rm = TRUE, type = 8))
annot_no_outlier

p_annotated <- p_styled +
  geom_text(
    data = annot_vt,
    mapping = aes(
      y = perc90, x = 2.25,
      label = glue("90th percentile")
    ),
    family = font,
    color = "#D06040FF",
    size = 1.5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text(
    data = annot_outlier,
    mapping = aes(
      y = perc90, x = 1.8,
      label = glue("90th percentile with excluded samples")
    ),
    family = font,
    color = "darkgrey",
    size = 1.5,
    hjust = 1,
    vjust = 0.5
  ) +
  geom_text(
    data = annot_no_outlier,
    mapping = aes(
      y = perc90, x = 0.89,
      label = glue("90th percentile without excluded samples")
    ),
    family = font,
    color = "#88C0F8FF",
    size = 1.5,
    hjust = 1,
    vjust = 0.5
  ) +
  geom_text(
    data = annot_outlier,
    mapping = aes(
      y = 75, x = 0.5,
      label = glue("Excluded samples")
    ),
    family = font,
    color = "darkgrey",
    size = 1.5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  geom_text(
    data = annot_outlier,
    mapping = aes(
      y = 22, x = 2.8,
      label = glue("EPA limit")
    ),
    family = font,
    color = "#3b3b3b",
    size = 1.5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  annotate(
    geom = "curve",
    x = 2.8,
    xend = 2.8,
    y = 20,
    yend = 15,
    linewidth = 0.3,
    color = "#3b3b3b",
    curvature = 0,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    )
  ) +
  annotate(
    geom = "curve",
    x = 0.5,
    xend = 1,
    y = 72,
    yend = 103,
    linewidth = 0.3,
    color = "darkgrey",
    curvature = 0.5,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    )
  ) +
  annotate(
    geom = "curve",
    x = 0.5,
    xend = 1,
    y = 72,
    yend = 20,
    linewidth = 0.3,
    color = "darkgrey",
    curvature = 0.5,
    arrow = arrow(
      length = unit(1.5, "mm"), type = "closed"
    )
  )

p_annotated

###############################################################################

#############
## Save png##
#############
ggsave(
  filename = "2025/20251104-lead/20251104-lead.png", p_annotated,
  width = 5, height = 5,
  bg = bg_col,
  dpi = 300
)
