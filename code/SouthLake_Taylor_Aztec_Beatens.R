rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO")
library(readxl)
library(tidyverse)
library(lubridate)

# Read the data and clean them
Aztec <- read_excel("data/Aztec_mine.xlsx", sheet = "2017-2019") %>% 
  mutate(
    source = "Aztec Mine",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

Beatens <- read_excel("data/beatens_cave.xlsx", sheet = "2016B") %>% 
  mutate(
    source = "Beatens Cave",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_E <- read_excel("data/SouthLake.xlsx", sheet = "E(25-26)") %>% 
  mutate(
    source = "South Lake E",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_K <- read_excel("data/SouthLake.xlsx", sheet = "K(25-26)") %>% 
  mutate(
    source = "South Lake K",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_A <- read_excel("data/SouthLake.xlsx", sheet = "A(25-26)") %>% 
  mutate(
    source = "South Lake A",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

SouthLake_E24_25 <- read_excel("data/SouthLake.xlsx", sheet = "SouthLake(E)24-25") %>% 
  select(datetime, temp, rh) %>%
  mutate(
    source = "South Lake E (24-25)",
    equipment = "HOBO U23 Pro v2"
  )

SouthLake_B24_25 <- read_excel("data/SouthLake.xlsx", sheet = "SouthLake(B)24-25") %>% 
  mutate(
    source = "South Lake B (24-25)",
    equipment = "HOBO U23 Pro v2"
  )

SouthLake_J24_25 <- read_excel("data/SouthLake.xlsx", sheet = "SouthLake(J)24-25") %>% 
  mutate(
    source = "South Lake J (24-25)",
    equipment = "HOBO U23 Pro v2"
  )

taylorsA <- read_excel("data/Taylors_Adit.xlsx", sheet = "2016A") %>% 
  mutate(
    source = "Taylor Adit 2016A",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

taylorsB <- read_excel("data/Taylors_Adit.xlsx", sheet = "2016B") %>% 
  mutate(
    source = "Taylor Adit 2016B",
    datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"))

# Function to calculate relative humidity from dry bulb and wet bulb temperatures
calculate_rh <- function(T_d, T_w, P = 1013.25) {
  # Saturation vapor pressure (hPa)
  e_s_d <- 6.112 * exp((17.67 * T_d) / (T_d + 243.5))
  e_s_w <- 6.112 * exp((17.67 * T_w) / (T_w + 243.5))
  # Vapor pressure at wet bulb
  e <- e_s_w - (P / 1000) * (T_d - T_w) * 0.00066 * (1 + 0.00115 * T_w)
  # Relative humidity
  rh <- (e / e_s_d) * 100
  return(pmax(pmin(rh, 100), 0))  # Clamp between 0 and 100
}

# combine South Lake data and make the date columns not specific to a year so we can compare the different years
combined_southlakea <- bind_rows(SouthLake_E, SouthLake_K, SouthLake_A) %>%
  mutate(temp = as.numeric(integrated), probe = as.numeric(probe), rh = calculate_rh(as.numeric(integrated), as.numeric(probe)))
combined_southlakeb <- bind_rows(SouthLake_E24_25, SouthLake_B24_25, SouthLake_J24_25) %>%
  select(datetime, temp, rh, source) %>%
  mutate(temp = as.numeric(temp), rh = as.numeric(rh))
combined_southlake <- bind_rows(combined_southlakea, combined_southlakeb) %>%
  mutate(source = str_replace(source, "South Lake ", "")) %>%
  mutate(source = case_when(
    source == "E" ~ "E (25-26)",
    source == "K" ~ "K (25-26)",
    source == "A" ~ "A (25-26)",
    TRUE ~ source
  )) %>%
  mutate(month_day = format(datetime, "%m-%d"),
         month = as.numeric(format(datetime, "%m")),
         year = ifelse(month >= 9, 1999, 2000),
         plot_date = as.Date(paste0(year, "-", month_day), format = "%Y-%m-%d"))

# Plot the combined data, now using plot_date on the x-axis (Sep-Aug cycle)
ggplot(combined_southlake, aes(x = plot_date, y = temp, color = source)) +
  geom_line() +
  labs(
    title = "South Lake Temperature Over Time (Sep-Aug Cycle)",
    x = "Month and Day",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_date(limits = as.Date(c("1999-09-01", "2000-08-31")), date_labels = "%b %d", date_breaks = "1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/south_lake_temps.png", width = 10, height = 6, dpi = 300)

# Filter out B (24-25) for humidity plot
combined_southlake_humidity <- combined_southlake %>% filter(source != "B (24-25)")

# Humidity plot
ggplot(combined_southlake_humidity, aes(x = plot_date, y = rh, color = source)) +
  geom_line() +
  labs(
    title = "South Lake Relative Humidity Over Time (Sep-Aug Cycle)",
    x = "Month and Day",
    y = "Relative Humidity (%)",
    color = "Source"
  ) +
  scale_x_date(limits = as.Date(c("1999-09-01", "2000-08-31")), date_labels = "%b %d", date_breaks = "1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/south_lake_humidity.png", width = 10, height = 6, dpi = 300)

