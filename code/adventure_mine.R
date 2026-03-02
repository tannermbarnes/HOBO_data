rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/code")
library(readxl)
library(tidyverse)
library(lubridate)

# Read the data and clean them
adventure_overlook <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/adventure_overlook.xlsx") %>% 
  select(date_time = `Date-Time (EST/EDT)`, temp = `Adventure Temp , °C`) %>%
  mutate(source = "overlook")


adventure_psychrometer <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/adventure_psychrometer.xlsx") %>% 
  select(date_time = `Date-Time (EST/EDT)`, wet_bulb, dry_bulb) %>%
  mutate(source = "psychrometer")

# Define psychrometric constant (hPa/°C)
gamma <- 0.00066

# Magnus equation for saturation vapor pressure (hPa)
saturation_vapor_pressure <- function(T) {
  6.112 * exp((17.62 * T) / (243.12 + T))
}

# Compute saturation vapor pressure for dry-bulb and wet-bulb
adventure_psychrometer <- adventure_psychrometer %>%
  mutate(
    e_db = saturation_vapor_pressure(dry_bulb),
    e_wb = saturation_vapor_pressure(wet_bulb),
    RH = 100 * (e_wb - gamma * (dry_bulb - wet_bulb)) / e_db
  )


combined_data <- bind_rows(adventure_overlook, adventure_psychrometer)
colnames(combined_data)

# Plot RH over time
combined_data %>% filter(RH > 0) %>% 
  ggplot(aes(x=date_time, y = RH)) +
  geom_line(color = "blue", linewidth = 0.5) +  # Line graph with blue color
  labs(
    title = "Adventure Mine Level 1: Relative Humidity",
    x = "Date",
    y = "Relative Humidity (%)"
  ) +
  scale_y_continuous(limits = c(0, 100)) + 
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(color = "black")
  )

# Plot the combined data, now using month_day on the x-axis
combined_data %>%
ggplot(aes(x = date_time, y = temp, color = source, group = source)) +
  geom_line() +
  geom_line(aes(y=dry_bulb)) +
  labs(
    title = "Adventure Mine level 1: Temperature",
    x = "Date: (2024-2025)",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  