rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/")
library(readxl)
library(tidyverse)
library(lubridate)

entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                       sheet = "gate_entrance") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Gate Entrance") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

psy_entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                      sheet = "psychrometer_entrance") %>% 
  select(date, probe_temp, integrated_temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Entrance Psychrometer") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59")) %>% 
  mutate(temp = integrated_temp)

wall_entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "wall_entrance") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Wall Entrance") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

middle_ICU <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                         sheet = "middle") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Middle ICU") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

wall_back <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "wall_back") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Wall Back") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

ICU_PSY <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "ICU_PSY") %>% 
  select(date, probe_temp, integrated_temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "ICU Psychrometer") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59")) %>% 
  mutate(temp = integrated_temp)

ICU_back <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "ICU_back") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "Back of ICU") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

control <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                      sheet = "control_200") %>% 
  select(date, temp) %>%
  mutate(
    date_time = as.POSIXct(date, format ="Y-%m-%d %H:%M:%S"), 
    source = "control") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

# ---- Combine all ----
combined_df <- bind_rows(
  entrance,
  wall_entrance,
  wall_back,
  ICU_PSY,
  ICU_back,
  middle_ICU,
  control,
  psy_entrance
)

start_date <- as.POSIXct("2025-06-01 00:00:00", tz = "America/Detroit")

# ---- Temperature Plot (Sept 1, 2025 – Current) ----
temp_plot <- ggplot(combined_df, aes(x = date_time, y = temp, color = source)) +
  geom_line() +
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = "Source",
    title = "Mead Temperatures Sept 2025 - Current"
  ) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2025-09-01 00:00:00"),
      Sys.time()
    ),
    date_labels = "%b %d",
    date_breaks = "1 week"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot
ggsave("figures/mead_temperatures_2025.png", temp_plot, width = 10, height = 6, dpi = 300)

# ---- Filtered Temperature Plot (Sept 1, 2025 – Current) ----
temp_plot_filtered <- combined_df %>%
  filter(!source %in% c("Gate Entrance", "control", "Wall Entrance", "ICU Psychrometer")) %>%
  mutate(source = case_when(
    source == "Entrance Psychrometer" ~ "Entrance",
    source == "Wall Back" ~ "Treatment Front",
    source == "Middle ICU" ~ "Treatment Middle",
    source == "Back of ICU" ~ "Treatment Back",
    TRUE ~ source
  )) %>%
  mutate(source = factor(source, levels = c("Entrance", "Treatment Front", "Treatment Middle", "Treatment Back"))) %>%
  ggplot(aes(x = date_time, y = temp, color = source)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Source",
    title = "Mead Temperatures Sept 2025 - January 2026"
  ) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2025-09-01 00:00:00"),
      Sys.time()
    ),
    date_labels = "%b",
    date_breaks = "1 month"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Save the filtered plot
ggsave("figures/mead_temperatures_filtered_2025.png", temp_plot_filtered, width = 10, height = 6, dpi = 300)


