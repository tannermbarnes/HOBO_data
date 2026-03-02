setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO")
library(readxl)
library(dplyr)
library(ggplot2)

temp <- read_excel("data/RockfallAdit.xlsx", sheet = "hobo")
humidity <- read_excel("data/RockfallAdit.xlsx", sheet = "psychrometer")

# Standardize column names
temp <- temp %>%
  rename(
    id = `#`,
    datetime = "date",
    temp_c = "temperature"
  ) %>%
  mutate(site = "rockfall")

humidity <- humidity %>%
  rename(
    id = `#`,
    datetime = "date",
  ) %>%
  mutate(site = "rockfall_humidity")

rockfall <- bind_rows(temp, humidity)

# Plot temperature over time starting at the earliest time
start_time <- as.POSIXct("2025-12-03 08:36:00", tz = "America/New_York")
rockfall_plot <- rockfall %>%
  mutate(datetime = as.POSIXct(datetime, tz = "America/New_York")) %>%
  filter(datetime >= start_time)

# ---- Rockfall presentation plots (paste below your existing data prep) ----

# Ensure datetime is POSIXct (uses your already-set time series column)
rockfall_plot <- rockfall_plot %>%
  mutate(datetime = as.POSIXct(datetime, tz = "America/New_York"))

# Relative humidity from psychrometer temps
gamma <- 0.00066
svp <- function(t_c) 6.112 * exp((17.62 * t_c) / (243.12 + t_c))

rockfall_plot <- rockfall_plot %>%
  mutate(
    temp_c = as.numeric(temp_c),
    probe_temp = as.numeric(probe_temp),
    integrated_temp = as.numeric(integrated_temp),
    humidity_rh = 100 * (svp(probe_temp) - gamma * (integrated_temp - probe_temp)) / svp(integrated_temp),
    humidity_rh = pmax(0, pmin(100, humidity_rh))
  )

# ---- Clean plotting data to avoid warnings ----
temp_long <- rockfall_plot %>%
  select(datetime, temp_c, integrated_temp, probe_temp) %>%
  pivot_longer(
    cols = c(temp_c, integrated_temp, probe_temp),
    names_to = "sensor_location",
    values_to = "temperature_c"
  ) %>%
  mutate(sensor_location = recode(
    sensor_location,
    temp_c = "Location 1",
    integrated_temp = "Location 2",
    probe_temp = "Location 3"
  )) %>%
  filter(!is.na(datetime), is.finite(temperature_c))

humidity_df <- rockfall_plot %>%
  filter(!is.na(datetime), is.finite(humidity_rh))

# ---- Temperature plot ----
plot_rockfall <- ggplot(temp_long, aes(x = datetime, y = temperature_c, color = sensor_location)) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  labs(
    title = "Rockfall Temperature Through Time",
    x = "Date and Time",
    y = "Temperature (°C)",
    color = "Sensor Location"
  ) +
  scale_color_manual(values = c("Location 1" = "#1B4F72", "Location 2" = "#CB4335", "Location 3" = "#F39C12"), name = "Sensor Location") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d\n%H:%M") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ---- Humidity plot ----
plot_humidity <- ggplot(humidity_df, aes(x = datetime, y = humidity_rh)) +
  geom_line(color = "#117A65", linewidth = 0.9) +
  labs(
    title = "Rockfall Relative Humidity Through Time",
    x = "Date and Time",
    y = "Relative Humidity (%)"
  ) +
  coord_cartesian(ylim = c(84, 100.5)) +   # zooms without dropping rows/warnings
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d\n%H:%M") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

plot_rockfall
plot_humidity

ggsave(
  filename = "figures/rockfall_temperature.png",
  plot = plot_rockfall,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "figures/rockfall_humidity.png",
  plot = plot_humidity,
  width = 10,
  height = 6,
  dpi = 300
)
