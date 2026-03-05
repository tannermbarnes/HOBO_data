rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/")
library(readxl)
library(tidyverse)
library(lubridate)

temp_sectionB <- read_excel("data/Delaware.xlsx", 
                       sheet = "light") %>% 
  select(datetime, temp, lux) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Delaware B") %>% filter(datetime > as.POSIXct("2025-01-16 00:00:00"))

psy_sectionH <- read_excel("data/Delaware.xlsx",
                      sheet = "psychrometer") %>% 
  select(datetime, probe_temp, integrated_temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Delaware H") %>% filter(datetime > as.POSIXct("2025-01-16 00:00:00"))

hobo_sectionI <- read_excel("data/Delaware.xlsx", 
                            sheet = "hobov2") %>% 
  select(datetime, temp, RH) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Delaware I")

combined_df <- bind_rows(
    temp_sectionB,
    psy_sectionH %>% rename(temp = integrated_temp),
    hobo_sectionI %>% rename(temp = temp)
)

start_date <- as.POSIXct("2025-01-15 00:00:00", tz = "America/Detroit")

# ---- Temperature Plot (colored by section) ----
temp_plot <- ggplot(combined_df, aes(x = date_time, y = temp, color = source)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = "Section",
    title = "Delaware Temperature Over Time"
    ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.line = element_line(color = "black", size = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )

# Save the plot
ggsave("figures/delaware_temperature_by_section.png", temp_plot, width = 12, height = 6, dpi = 300)

# Display
temp_plot
