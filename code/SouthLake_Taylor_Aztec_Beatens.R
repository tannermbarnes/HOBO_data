rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO")
library(readxl)
library(tidyverse)
library(lubridate)

# Read the data and clean them
Aztec <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Aztec.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799593..SEN.S.N..10799593.`, 
         rh = `RH.....LGR.S.N..10799593..SEN.S.N..10799593.`) %>% 
  mutate(
    source = "Aztec Mine",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))


Beatens <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Beatens_2016_b.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.04.00`, temp = `Temp...C..LGR.S.N..10799587..SEN.S.N..10799587.`, 
         rh = `RH.....LGR.S.N..10799587..SEN.S.N..10799587.`) %>% 
  mutate(
    source = "Beatens Cave",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_C_treated <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/SouthLake_C-treated.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799580..SEN.S.N..10799580.`, 
         rh = `RH.....LGR.S.N..10799580..SEN.S.N..10799580.`) %>% 
  mutate(
    source = "South Lake C treated",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_G_control <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/SouthLake_G-control.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799584..SEN.S.N..10799584.`, 
         rh = `RH.....LGR.S.N..10799584..SEN.S.N..10799584.`) %>% 
  mutate(
    source = "South Lake G control",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_H_treated <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/SouthLake_H-treated.csv", skip = 1) %>% 
select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799579..SEN.S.N..10799579.`, 
       rh = `RH.....LGR.S.N..10799579..SEN.S.N..10799579.`) %>% 
  mutate(
    source = "South Lake H treated",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_SE <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/SouthLake_SE.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799588..SEN.S.N..10799588.`, 
         rh = `RH.....LGR.S.N..10799588..SEN.S.N..10799588.`) %>% 
  mutate(
    source = "South Lake SE",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

SouthLakeArea_4D <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/SouthLakeArea_4D.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.05.00`, temp = `Temp...C..LGR.S.N..10799591..SEN.S.N..10799591.`, 
         rh = `RH.....LGR.S.N..10799591..SEN.S.N..10799591.`) %>% 
  mutate(
    source = "South Lake Area 4D",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

Taylor_2016a <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/Taylor_2016_a.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.04.00`, temp = `Temp...C..LGR.S.N..10799586..SEN.S.N..10799586.`, 
         rh = `RH.....LGR.S.N..10799586..SEN.S.N..10799586.`) %>% 
  mutate(
    source = "Taylor A",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))

Taylor_2016b <- read.csv("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/Taylor_2016_b.csv", skip = 1) %>% 
  select(date_time = `Date.Time..GMT.04.00`, temp = `Temp...C..LGR.S.N..10799594..SEN.S.N..10799594.`, 
         rh = `RH.....LGR.S.N..10799594..SEN.S.N..10799594.`) %>% 
  mutate(
    source = "Taylor B",
    date_time = as.POSIXct(date_time, format = "%m/%d/%y %I:%M:%S %p"))


# Combine South Lake datasets
combined_southlake <- bind_rows(SouthLake_C_treated, SouthLake_G_control, SouthLake_H_treated, 
                           SouthLake_SE, SouthLakeArea_4D) %>%
  mutate(month_day = format(date_time, "%m-%d"))


# Mutate and create month_day column that only includes month and day
combined_southlake <- combined_southlakea %>%
  mutate(month_day = format(as.Date(date_time), "%m-%d"))

# Mutate to set the year for August-December as 2024 and January-July as 2025
combined_southlake <- combined_southlake %>%
  mutate(
    month_day = format(as.Date(date_time), "%m-%d"),  # Extract month and day
    year_adjusted = ifelse(month_day >= "09-01", "2024", "2025"),  # Assign year based on month
    month_day = as.Date(paste(year_adjusted, month_day, sep = "-"), format = "%Y-%m-%d")  # Combine year and month-day
  )

# Define custom x-axis range from August 1 to July 31 of the next year
x_limits <- as.Date(c("2024-09-01", "2025-08-31"))

# Plot the combined data, now using month_day on the x-axis
ggplot(combined_southlake, aes(x = month_day, y = temp, color = source, group = source)) +
  geom_line() +
  labs(
    title = "Temperature Over Time for Henwood and Tippy Dam",
    x = "Month and Day",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +  # Custom x-axis from August to July
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




# Combine Taylor Adit
combined_taylor <- bind_rows(Taylor_2016a, Taylor_2016b)

# Mutate and create month_day column that only includes month and day
combined_taylor <- combined_taylor %>%
  mutate(month_day = format(as.Date(date_time), "%m-%d-%y"))

# Mutate to set the year for August-December as 2024 and January-July as 2025
combined_taylor <- combined_taylor %>%
  mutate(
    month_day = format(as.Date(date_time), "%m-%d-%y"))

# Define custom x-axis range from August 1 to July 31 of the next year
x_limits <- as.Date(c("2016-11-16", "2017-08-31"))


# Create the plot
ggplot(combined_taylor, aes(x = date_time, y = temp, color = source, group = source)) +
  geom_line() +  # Add lines for each group
  labs(
    title = "Taylor Adit Temperature Over Time",
    x = "Date and Time",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  coord_cartesian(
    xlim = c(as.POSIXct("2016-11-01"), as.POSIXct("2017-10-25")),
    ylim = c(0, 12)
  ) +
  theme_bw() +  # Use a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/Taylor_adit.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)

# Create the plot
ggplot(Aztec, aes(x = date_time, y = temp)) +
  geom_line() +  # Add lines for each group
  labs(
    title = "Aztec Mine Temperature Over Time",
    x = "Date and Time",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_datetime(date_breaks = "3 month", date_labels = "%b %Y") +
  coord_cartesian(
    xlim = c(as.POSIXct("2017-03-10"), as.POSIXct("2019-07-01"))) +
   # ylim = c(0, 12)
  #) +
  theme_bw() +  # Use a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/Aztec_mine.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)

ggplot(Beatens, aes(x = date_time, y = temp)) +
  geom_line() +  # Add lines for each group
  labs(
    title = "Aztec Mine Temperature Over Time",
    x = "Date and Time",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  coord_cartesian(
    xlim = c(as.POSIXct("2016-10-01"), as.POSIXct("2018-02-15"))) +
  # ylim = c(0, 12)
  #) +
  theme_bw() +  # Use a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/Beatens_cave.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)
