rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO")
library(readxl)
library(tidyverse)
library(lubridate)

data <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/BACK-winter_2018-2024.csv", skip = 1) 

# Check the column names and adjust them
names(data) <- c("#", "Date_Time", "Temperature_C", "Humidity_Percent")

# View the cleaned data
head(data)

# Convert Date_Time to Date format
data$Date_Time <- as.POSIXct(data$Date_Time, format = "%m/%d/%y %I:%M:%S %p")

# Define scaling factors
temp_min <- min(data$Temperature_C, na.rm = TRUE)
temp_max <- max(data$Temperature_C, na.rm = TRUE)

humidity_min <- 0
humidity_max <- 100

# Define scale transformation for matching ranges
scale_factor <- (temp_max - temp_min) / (humidity_max - humidity_min)

data1 <- data %>%
  filter(Date_Time > as.POSIXct("2019-02-27 23:00:00"))

# Create the plot
ggplot(data1, aes(x = Date_Time)) +
  geom_line(aes(y = Temperature_C, color = "Temperature")) + # Temperature line
  geom_line(aes(y = (Humidity_Percent - humidity_min) * scale_factor + temp_min, color = "Humidity")) + # Scaled humidity line
  scale_y_continuous(
    name = "Temperature (°C)", 
    sec.axis = sec_axis(~ (. - temp_min) / scale_factor + humidity_min, name = "Humidity (%)") # Secondary axis for humidity
  ) +
  scale_color_manual(
    values = c("Temperature" = "red", "Humidity" = "blue"), 
    name = "Variable"
  ) +
  theme_bw() +
  theme(
    axis.title.y.right = element_text(color = "blue"), # Color the right y-axis
    axis.title.y.left = element_text(color = "red"), # Color the left y-axis
    panel.grid = element_blank()
  ) +
  labs(x = "Date", title = "Temperature and Humidity Over Time")

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/mead.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)
