rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/")
library(readxl)
library(tidyverse)
library(lubridate)

entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                       sheet = "gate_entrance") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Gate Entrance") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

psy_entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                      sheet = "psychrometer_entrance") %>% 
  select(datetime, probe, integrated) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Entrance Psychrometer") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59")) %>% 
  mutate(temp = integrated)

wall_entrance <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "wall_entrance") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Wall Entrance") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

middle_ICU <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                         sheet = "middle") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Middle ICU") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

wall_back <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "wall_back") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Wall Back") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

ICU_PSY <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "ICU_PSY") %>% 
  select(datetime, probe, integrated) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "ICU Psychrometer") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59")) %>% 
  mutate(temp = integrated)

ICU_back <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                            sheet = "ICU_back") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
    source = "Back of ICU") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

control <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", 
                      sheet = "control_200") %>% 
  select(datetime, temp) %>%
  mutate(
    date_time = as.POSIXct(datetime, format ="Y-%m-%d %H:%M:%S"), 
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

# compute y-axis breaks every 0.5°C based on the data (used in all plots)
y_breaks <- seq(
  floor(min(combined_df$temp, na.rm = TRUE)),
  ceiling(max(combined_df$temp, na.rm = TRUE)),
  by = 0.5
)

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
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal(base_size = 18) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
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
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal(base_size = 18) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Save the filtered plot
ggsave("figures/mead_temperatures_filtered_2025.png", temp_plot_filtered, width = 10, height = 6, dpi = 300)


# ---- Wall‑back zoomed plot (15 Feb 2026 – last record) ----
temp_plot_wall_back <- combined_df %>%
  filter(source == "Wall Back",
         date_time >= as.POSIXct("2026-02-15 00:00:00",
                                 tz = "America/Detroit")) %>%
  ggplot(aes(x = date_time, y = temp)) +
  geom_line(color = "steelblue") +
  labs(
    x     = "Date",
    y     = "Temperature (°C)",
  ) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2026-02-15 00:00:00", tz = "America/Detroit"),
      Sys.time()
    ),
    date_labels = "%b %d",
    date_breaks = "2 day"
  ) +
  scale_y_continuous(breaks = y_breaks) +
  theme_minimal(base_size = 18) +
  theme(
    axis.line  = element_line(color = "black", size = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
temp_plot_wall_back
# and save if you want:
ggsave("figures/mead_wallback_zoom.png",
       temp_plot_wall_back,
       width = 8, height = 4, dpi = 300)

# ---- New Plot: Experiment Year 1 (Dec 2025 - Current) ---- ##########################
experiment_year1_df <- bind_rows(
  psy_entrance,
  wall_entrance,
  wall_back,
  middle_ICU,
  ICU_back, 
  control
) %>%
  arrange(source, date_time) %>%
  mutate(source = case_when(
    source == "Entrance Psychrometer" ~ "External Temperature",
    source == "Wall Entrance" ~ "Entrance",
    source == "Wall Back" ~ "Experiment - Front",
    source == "Middle ICU" ~ "Experiment - Middle",
    source == "Back of ICU" ~ "Experiment - Back",
    source == "control" ~ "Control",
    TRUE ~ source
  )) %>%
  mutate(source = factor(
    source,
    levels = c(
      "External Temperature",
      "Entrance",
      "Experiment - Front",
      "Experiment - Middle",
      "Experiment - Back",
      "Control"
    )
  )) %>%
  group_by(source) %>%
  mutate(
    time_gap_days = as.numeric(difftime(date_time, lag(date_time), units = "days")),
    line_group = cumsum(replace_na(time_gap_days > 7, FALSE))
  ) %>%
  ungroup()

# Compute y-axis breaks for this plot
y_breaks_year1 <- seq(-10, 10, by = 1)

experiment_year1_colors <- c(
  "External Temperature" = "#1F78B4",
  "Entrance" = "#E66100",
  "Experiment - Front" = "#4DAF4A",
  "Experiment - Middle" = "#984EA3",
  "Experiment - Back" = "#A65628", 
  "Control" = "#DAA520"
)

experiment_year1_plot <- ggplot(
  experiment_year1_df,
  aes(x = date_time, y = temp, color = source, group = interaction(source, line_group))
) +
  geom_rect(data = data.frame(xmin = as.POSIXct("2025-12-11"), xmax = as.POSIXct("2025-12-16"), ymin = -Inf, ymax = Inf), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "dodgerblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_rect(data = data.frame(xmin = as.POSIXct("2026-02-17"), xmax = Sys.time(), ymin = -Inf, ymax = Inf), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "dodgerblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_line() +
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2025-12-01 00:00:00"),
      Sys.time()
    ),
    date_labels = "%b %d",
    date_breaks = "2 weeks"
  ) +
  scale_color_manual(values = experiment_year1_colors) +
  scale_y_continuous(limits = c(-10, 10), breaks = y_breaks_year1) +
  theme_minimal(base_size = 18) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.20),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  geom_hline(yintercept = 3, color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2025-12-11"), color = "darkorange", linetype = "solid") +
  geom_vline(xintercept = as.POSIXct("2026-02-27"), color = "darkblue", linetype = "solid") +
  annotate("text", x = as.POSIXct("2025-12-14"), y = 9, label = "Survey 1", color = "darkorange", size = 4, angle = 90, vjust = -0.5) +
  annotate("text", x = as.POSIXct("2026-02-27"), y = 9, label = "Survey 2", color = "darkblue", size = 4, angle = 90, vjust = -0.5)

print(experiment_year1_plot)

# Save the plot
ggsave("experiment year 1 plot.png", experiment_year1_plot, width = 10, height = 6, dpi = 300)

