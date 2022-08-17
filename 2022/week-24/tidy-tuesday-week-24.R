# Load packages -------------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(ggtext)
library(systemfonts)


# Load and wrangle data -----------------------------------------------------------------------------

# FIPS codes for counties and county equivalent entities from the US Census Bureau
us_counties <- readr::read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = FALSE
)

# Arizona counties
arizona_counties <- us_counties %>%
  dplyr::select(X1:X4) %>%
  purrr::set_names(c("state_abbreviation", "state_fips", "county_fips", "county")) %>%
  dplyr::filter(state_abbreviation == "AZ") %>%
  tidyr::unite(fips, state_fips:county_fips, sep = "")

# US droughts
us_droughts <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv"
)

# Arizona droughts
arizona_droughts <- us_droughts %>%
  janitor::clean_names() %>%
  dplyr::inner_join(arizona_counties, by = "fips") %>%
  dplyr::filter(lubridate::year(date) >= 2020)

# Date labels for the X-axis
date_labs <- seq(
  lubridate::ymd("2020-01-01"),
  lubridate::ymd("2022-06-07"),
  by = "3 month"
)


# Plot ----------------------------------------------------------------------------------------------

# Base plot
base_plot <- ggplot(arizona_droughts, aes(x = date, y = county)) +
  geom_tile(aes(fill = dsci), height = 0.8, width = 6) +
  scale_y_discrete(limits = rev) +
  scale_x_date(
    breaks = "3 month",
    date_labels = dplyr::if_else(lubridate::month(date_labs) == 1, "%b %Y", "%b"),
    limits = c(lubridate::ymd("2020-01-01"), lubridate::ymd("2022-06-07")),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_fill_stepsn(
    colors = c("#FCF7E1", "#FCEFD5", "#FACC87", "#FA8D53", "#DF4B2A"),
    breaks = c(0, 100, 200, 300, 400, 500),
    guide = guide_colorsteps(
      "title" = "**Drought Severity and Coverage Index (DSCI)** <br> 0: No drought ; 500: Exceptional drought",
      title.position = "top",
      barwidth = unit(12, "lines"),
      barheight = unit(0.3, "lines"),
      show.limits = TRUE
    )
  ) +
  labs(
    title = "Droughts in Arizona",
    subtitle = "Drought conditions since January 2020",
    caption = c(
      "Last updated: June 7, 2022",
      "Source: National Integrated Drought Information System | Graphic: @shadfrigui"
    )
  )

# Theme
theme <- theme(
  text = element_text(family = "Cambay"),
  plot.title = element_text(size = 20, face = "bold"),
  plot.title.position = "plot",
  plot.subtitle = element_text(size = 15, margin = margin(t = 5, b = -40)),
  # The following line of code raises a warning message:
  # "Vectorized input to `element_text()` is not officially supported.
  # Results may be unexpected or may change in future versions of ggplot2."
  plot.caption = element_text(size = 8, hjust = c(0, 1), margin = margin(t = 10)),
  plot.caption.position = "plot",
  panel.background = element_rect(fill = NA, color = NA),
  axis.title = element_blank(),
  axis.ticks = element_line(size = 0.3),
  axis.ticks.length = unit(0.3, "lines"),
  axis.ticks.y = element_blank(),
  axis.text = element_text(size = 9, color = "black"),
  axis.text.x.top = element_text(vjust = 0.6),
  axis.text.y = element_text(hjust = 0),
  axis.line.x = element_line(size = 0.3),
  legend.position = "top",
  legend.justification = "right",
  legend.title = element_markdown(size = 8.5, lineheight = 1.5),
  legend.text = element_text(size = 8.5, margin = margin(t = -2)),
  plot.margin = margin(15, 15, 5, 15)
)

# Final plot
final_plot <- base_plot + theme


# Save plot -----------------------------------------------------------------------------------------
ggsave("tidy-tuesday-week-24.png", final_plot, width = 9, height = 5, dpi = 320)