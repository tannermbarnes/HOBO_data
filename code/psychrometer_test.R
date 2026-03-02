rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/code")
library(readxl)
library(tidyverse)
library(lubridate)

test <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/test.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, wet_bulb = `Probe Temperature , °C`, dry_bulb = `Integrated Temperature , °C`) %>%
  mutate(source = "psychrometer")

# Define psychrometric constant (hPa/°C)
gamma <- 0.00066

# Magnus equation for saturation vapor pressure (hPa)
saturation_vapor_pressure <- function(T) {
  6.112 * exp((17.62 * T) / (243.12 + T))
}

# Compute saturation vapor pressure for dry-bulb and wet-bulb
test <- test %>%
  mutate(
    e_db = saturation_vapor_pressure(dry_bulb),
    e_wb = saturation_vapor_pressure(wet_bulb),
    RH = 100 * (e_wb - gamma * (dry_bulb - wet_bulb)) / e_db
  )
# Plot RH over time
test %>%
  ggplot(aes(x = date_time, y = RH)) +
  geom_line(color = "blue", linewidth = 0.5) +
  labs(
    title = "Psychrometer was between my office and car",
    x = "Date and Time",
    y = "Relative Humidity (%)"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_datetime(
    date_labels = "%b %d, %Y",  # Example: "Apr 17 15:00"
    date_breaks = "2 days"        # You can adjust this as needed
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
