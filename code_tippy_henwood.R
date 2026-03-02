rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO")
library(readxl)
library(tidyverse)
library(lubridate)

# Read the data and clean them
henwood_experiment <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/HENWOOD HOBO5 - Experiment.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Experiment")

henwood_control <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/HENWOOD HOBO6 - Control.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Control")

henwood_skylight <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/HENWOOD HOBO7 - Skylight.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Skylight")

tippy_dam <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/Tippy_dam_temperature 2019-2020.csv") %>% 
  select(date_time = Date, temp = Dry_Bulb._C_22) %>%
  mutate(
    source = "Tippy Dam",
    date_time = as.POSIXct(date_time, format = "%m/%d/%Y")  # Adjust format if needed
  )

# Modify all datasets to extract month and day only, ignoring the year
# Create a month-day column that doesn't include the year
combined_data <- bind_rows(henwood_experiment, henwood_control, henwood_skylight, tippy_dam) %>%
  mutate(month_day = format(date_time, "%m-%d"))  # Extract month and day as a string

# Mutate and create month_day column that only includes month and day
combined_data <- combined_data %>%
  mutate(month_day = format(as.Date(date_time), "%m-%d"))


# Mutate to set the year for August-December as 2024 and January-July as 2025
combined_data <- combined_data %>%
  mutate(
    month_day = format(as.Date(date_time), "%m-%d"),  # Extract month and day
    year_adjusted = ifelse(month_day >= "09-01", "2024", "2025"),  # Assign year based on month
    month_day = as.Date(paste(year_adjusted, month_day, sep = "-"), format = "%Y-%m-%d")  # Combine year and month-day
  )

# Define custom x-axis range from August 1 to July 31 of the next year
x_limits <- as.Date(c("2024-09-01", "2025-08-31"))

# Plot the combined data, now using month_day on the x-axis
ggplot(combined_data, aes(x = month_day, y = temp, color = source, group = source)) +
  geom_line() +
  labs(
    title = "Temperature Over Time for Henwood and Tippy Dam",
    x = "Month and Day",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month", limits = x_limits) +  # Custom x-axis from August to July
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/temperature_timeline_plot.png",  # File path and name
  plot = last_plot(),   # If you want to save the last plot you created
  width = 12,           # Set the width to make it wide
  height = 6,           # Adjust the height
  dpi = 300             # Set the resolution to 300 DPI for high quality
)


# Filter rows that are outside the limits of the x-axis
outside_scale_rows <- combined_data %>%
  filter(month_day < x_limits[1] | month_day > x_limits[2])

# Display the rows outside the scale range
outside_scale_rows

# Check for missing temperature values
missing_temp_rows <- combined_data %>%
  filter(is.na(temp))

# Display rows with missing temperature values
missing_temp_rows

