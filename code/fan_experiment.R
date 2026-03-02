library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(lubridate)


# File path
file_path <- "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Mead/fan_experiment.xlsx"

# List all sheet names
sheets <- excel_sheets(file_path)

# Only process the temperature logger sheets
logger_sheets <- c("entrance_psychrometer", "wall_entrance", "middle", "experiment_psy", "back_experiment")

data_list <- lapply(logger_sheets, function(sheet) {
  df <- read_excel(file_path, sheet = sheet) %>%
    janitor::clean_names()
  
  if (sheet == "entrance_psychrometer") {
    df <- df %>%
      dplyr::rename(
        id = number,
        datetime_edt = date_time_edt,
        temperature_1_c = temperature_c_3,
        temperature_2_c = temperature_c_4
      )
  } else if (sheet == "experiment_psy") {
    df <- df %>%
      dplyr::rename(
        id = number,
        datetime_edt = date_time_edt,
        temperature = probe_temperature_c,
        integrated_temperature_c = integrated_temperature_c
      )
  } else {
    # For wall_entrance, middle, back_experiment
    df <- df %>%
      dplyr::rename(
        id = number,
        datetime_edt = date_time_edt,
        temperature = temperature_c
      )
  }
  return(df)
})
names(data_list) <- logger_sheets

# Calculate humidity and look at temperature over time

# Example psychrometric formula for relative humidity (approximate)
calc_rh <- function(dry, wet) {
  # Constants for calculation
  A <- 17.27
  B <- 237.7
  # Saturation vapor pressure for dry bulb
  svp_dry <- 6.1078 * exp((A * dry) / (B + dry))
  # Saturation vapor pressure for wet bulb
  svp_wet <- 6.1078 * exp((A * wet) / (B + wet))
  # Actual vapor pressure
  avp <- svp_wet - 0.00066 * (1 + 0.00115 * wet) * (dry - wet) * 1013.25
  # Relative humidity
  rh <- (avp / svp_dry) * 100
  return(rh)
}

plot_data <- bind_rows(
  data_list$entrance_psychrometer %>%
    mutate(
      sensor = "entrance_psychrometer",
      temperature = temperature_1_c,
      humidity = calc_rh(temperature_1_c, temperature_2_c)
    ) %>%
    select(datetime_edt, temperature, humidity, sensor),
  data_list$wall_entrance %>%
    mutate(
      sensor = "wall_entrance",
      temperature = temperature,
      humidity = NA
    ) %>%
    select(datetime_edt, temperature, humidity, sensor),
  data_list$middle %>%
    mutate(
      sensor = "middle",
      temperature = temperature,
      humidity = NA
    ) %>%
    select(datetime_edt, temperature, humidity, sensor),
  data_list$experiment_psy %>%
    mutate(
      sensor = "experiment_psy",
      # Here, temperature and integrated_temperature_c are correct
      humidity = calc_rh(temperature, integrated_temperature_c)
    ) %>%
    select(datetime_edt, temperature, humidity, sensor),
  data_list$back_experiment %>%
    mutate(
      sensor = "back_experiment",
      temperature = temperature,
      humidity = NA
    ) %>%
    select(datetime_edt, temperature, humidity, sensor)
)

# Plot temperature
ggplot(plot_data, aes(x = datetime_edt, y = temperature, color = sensor)) +
  geom_line() +
  labs(title = "Temperature Over Time", x = "Time", y = "Temperature (°C)")

# Plot humidity (only sensors with humidity data)
ggplot(plot_data %>% filter(!is.na(humidity)), aes(x = datetime_edt, y = humidity, color = sensor)) +
  geom_line() +
  labs(title = "Humidity Over Time", x = "Time", y = "Relative Humidity (%)")


# Define experiment phases
start_time <- ymd_hms("2025-09-16 08:57:15") # adjust to your actual start
fan_on_start <- ymd_hms("2025-09-16 09:17:00")
fan_off <- ymd_hms("2025-09-16 12:08:00")
recovery_end <- ymd_hms("2025-09-16 12:37:00")

# Create a data frame for background rectangles
bg_rects <- data.frame(
  xmin = c(start_time, fan_on_start, fan_off + minutes(1)),
  xmax = c(fan_on_start - minutes(1), fan_off, recovery_end),
  fill = c("Control", "Fan On", "Recovery")
)

# Plot temperature with background regions
ggplot(plot_data, aes(x = datetime_edt, y = temperature, color = sensor)) +
  geom_rect(data = bg_rects, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line(size = 1) +
  scale_fill_manual(values = c("Control" = "gray80", "Fan On" = "red3", "Recovery" = "steelblue")) +
  labs(title = "Mine Temperature Over Time",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(title = "Experiment Phase"))

exp_loggers <- c("back_experiment", "middle", "experiment_psy")
exp_data <- plot_data %>% filter(sensor %in% exp_loggers)

# Find min and max for each logger
min_temps <- exp_data %>% group_by(sensor) %>% filter(temperature == min(temperature, na.rm = TRUE)) %>%
  mutate(nudge_y = case_when(
    sensor == "back_experiment" ~ -0.5,
    sensor == "middle" ~ -1,
    sensor == "experiment_psy" ~ -1.5
  ))
max_temps <- exp_data %>% group_by(sensor) %>% filter(temperature == max(temperature, na.rm = TRUE)) %>%
  mutate(nudge_y = case_when(
    sensor == "back_experiment" ~ 0.5,
    sensor == "middle" ~ 1,
    sensor == "experiment_psy" ~ 1.5
  ))

# Plot experimental data with min/max points
ggplot(exp_data, aes(x = datetime_edt, y = temperature, color = sensor)) +
  geom_line(size = 1) +
  geom_point(data = min_temps, aes(x = datetime_edt, y = temperature), size = 3, color = "blue") +
  geom_text(
    data = min_temps,
    aes(x = datetime_edt, y = temperature, label = round(temperature, 1)),
    vjust = 0.1, color = "blue", size = 5,
    nudge_y = min_temps$nudge_y
  ) +
  geom_point(data = max_temps, aes(x = datetime_edt, y = temperature), size = 3, color = "red") +
  geom_text(
    data = max_temps,
    aes(x = datetime_edt, y = temperature, label = round(temperature, 1)),
    vjust = -0.1, color = "red", size = 5,
    nudge_y = max_temps$nudge_y
  ) +
  labs(title = "Temperature Over Time (Experimental Area)",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")



library(dplyr)
library(ggplot2)
library(lubridate)

# --- Focus on one sensor ---
df <- plot_data %>%
  filter(sensor == "experiment_psy") %>%
  arrange(datetime_edt) %>%
  mutate(
    t_sec = as.numeric(difftime(datetime_edt, first(datetime_edt), units = "secs")),
    slope = c(NA, diff(temperature) / diff(t_sec))  # °C per sec
  )

# --- Detect steepest heating & cooling ---
heat_idx <- which.max(df$slope)   # steepest positive
cool_idx <- which.min(df$slope)   # steepest negative

heat_point <- df[heat_idx, ]
cool_point <- df[cool_idx, ]

# --- Rates in °C/s (already from slope calc) ---
heat_rate <- round(heat_point$slope, 5)  # °C/s
cool_rate <- round(cool_point$slope, 5)  # °C/s

heat_label <- paste0("Max heating: ", heat_rate, " °C/s")
cool_label <- paste0("Max cooling: ", cool_rate, " °C/s")

# Rates in °C/min (clearer than °C/s)
heat_rate1 <- round(heat_point$slope * 60, 3)
cool_rate1 <- round(cool_point$slope * 60, 3)

heat_label1 <- paste0("Max heating: ", heat_rate1, " °C/min")
cool_label1 <- paste0("Max cooling: ", cool_rate1, " °C/min")

# --- Background phases (you already had this) ---
bg_rects <- data.frame(
  xmin = c(start_time, fan_on_start, fan_off + minutes(1)),
  xmax = c(fan_on_start - minutes(1), fan_off, recovery_end),
  fill = c("Control", "Fan On", "Recovery")
)

# --- Final plot ---
p <- ggplot(df, aes(x = datetime_edt, y = temperature)) +
  # Phase backgrounds
  geom_rect(data = bg_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.2) +
  # Main trace
  geom_line(color = "black", linewidth = 1) +
  # Highlight points
  geom_point(data = heat_point, aes(x = datetime_edt, y = temperature),
             color = "red", size = 3) +
  geom_point(data = cool_point, aes(x = datetime_edt, y = temperature),
             color = "blue", size = 3) +
  # Labels (nudge so they don't get cut off)
  geom_text(data = heat_point,
            aes(x = datetime_edt, y = temperature, label = heat_label1),
            vjust = 0, hjust = -0.03, color = "red", size = 5) +
  geom_text(data = cool_point,
            aes(x = datetime_edt, y = temperature, label = cool_label1),
            vjust = 1.8, hjust = 1.0, color = "blue", size = 5) +
  # Styling
  scale_fill_manual(values = c("Control" = "gray80", "Fan On" = "red3", "Recovery" = "skyblue3")) +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(title = "Experimentally Heating Mine",
       x = "Time",
       y = "Temperature (°C)",
       fill = "Experiment Phase") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Save as PNG (adjust width/height/dpi as needed for publication)
ggsave(
  filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/fan_experiment_curve.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
