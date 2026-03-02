rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO")

library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

# Load and format both datasets
alley_wet <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Alley/Alley test wet_copper_coils.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, wet_bulb = `Probe Temperature , °C`, dry_bulb = `Integrated Temperature , °C`) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %H:%M:%S")) %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

alley_dry <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Alley/Alley test dry_copper_coils.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, dry_bulb = `Integrated Temperature , °C`) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %H:%M:%S")) %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))

# Round both date_time columns to nearest 30 seconds (or minute)
alley_wet <- alley_wet %>% mutate(date_time_round = round_date(date_time, unit = "1 minute"))
alley_dry <- alley_dry %>% mutate(date_time_round = round_date(date_time, unit = "1 minute"))

# Now join on rounded time
alley_merged <- inner_join(alley_wet, alley_dry, by = "date_time_round", suffix = c("_wet", "_dry"))


# Calculate dry bulb average (using both dry bulb sensors)
# Use only wet bulb from working probe
alley_combined <- alley_merged %>%
  mutate(
    dry_bulb_avg = rowMeans(select(., dry_bulb_wet, dry_bulb_dry), na.rm = TRUE),
    wet_bulb_wet = wet_bulb
  )

# Psychrometric RH formula using ONLY working wet bulb
alley_combined <- alley_combined %>%
  mutate(
    RH = 100 * ((112 - 0.1 * dry_bulb_avg + wet_bulb) / (112 + 0.9 * dry_bulb_avg)),
    RH = pmin(RH, 100)
  )

# Plot: Dry bulb temp and RH
ggplot(alley_combined, aes(x = date_time_round)) +
  geom_line(aes(y = dry_bulb_avg, color = "Temperature (°C)"), size = 1) +
  geom_line(aes(y = RH / 5, color = "Relative Humidity (%) / 5"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * 5, name = "Relative Humidity (%)")
  ) +
  scale_color_manual(values = c("Temperature (°C)" = "firebrick", "Relative Humidity (%) / 5" = "steelblue")) +
  theme_minimal() +
  labs(x = "Date-Time", color = "Legend") +
  theme(legend.position = "bottom")


# Define time window
start_time <- as.POSIXct("2025-08-16 12:17:41", tz = "UTC")
end_time   <- as.POSIXct("2025-08-16 16:20:41", tz = "UTC")

# Filter to the specified window
plot_data <- alley_merged %>%
  filter(date_time_round >= start_time & date_time_round <= end_time) %>%
  select(date_time_round, dry_bulb_wet, dry_bulb_dry) %>%
  pivot_longer(cols = starts_with("dry_bulb"), names_to = "sensor", values_to = "temperature") %>%
  mutate(sensor = recode(sensor,
                         dry_bulb_wet = "Wet Unit Sensor",
                         dry_bulb_dry = "Dry Unit Sensor"))

# Plot
plot_data %>% filter(sensor == "Dry Unit Sensor") %>% 
ggplot(aes(x = date_time_round, y = temperature)) +
  geom_line(size = 1) +
  labs(
    title = "Dry Bulb Temperature Comparison",
    x = "Time",
    y = "Temperature (°C)",
    color = "Sensor"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/alley_test1.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)


initial_curve <- plot_data %>% filter(sensor == "Dry Unit Sensor")

library(dplyr)
library(ggplot2)
library(lubridate)
library(minpack.lm)

# Assume data frame = initial_curve with columns: date_time_round, temperature
# ---- Params you can tweak ----
warm_search_start <- ymd_hms("2025-08-16 15:00:00")  # limit warming search to after this time
cool_window_min   <- 2   # mins after event to keep for fit
warm_window_min   <- 2

# ---- 1) Slopes + add an index ----
df <- initial_curve %>%
  arrange(date_time_round) %>%
  mutate(
    i      = row_number(),  # <-- index column!
    t_mins = as.numeric(difftime(date_time_round, first(date_time_round), units = "mins")),
    slope  = c(NA, diff(temperature) / diff(t_mins))  # °C per minute
  )

ggplot(df, aes(x = date_time_round, y = slope)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(x = "Time", y = "Slope (°C/min)",
       title = "Temperature Change Rate Over Time") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )


# ---- 2) Pick indices ----
cool_idx <- df %>%
  filter(!is.na(slope)) %>%
  slice_min(slope, n = 1, with_ties = FALSE) %>%
  pull(i)

warm_idx <- df %>%
  filter(date_time_round >= warm_search_start, !is.na(slope)) %>%
  slice_max(slope, n = 1, with_ties = FALSE) %>%
  pull(i)

cool_start <- df$date_time_round[cool_idx]
warm_start <- df$date_time_round[warm_idx]

cat("Cooling detected at:", cool_start, "\n")
cat("Warming detected at:", warm_start, "\n")

# --- Step 3. Subset windows (e.g. 60 min after event) ---
cool_df <- df %>%
  filter(date_time_round >= cool_start,
         date_time_round <= cool_start + minutes(2)) %>%
  mutate(t_rel = as.numeric(difftime(date_time_round, cool_start, units = "mins")))

warm_df <- df %>%
  filter(date_time_round >= warm_start,
         date_time_round <= warm_start + minutes(2)) %>%
  mutate(t_rel = as.numeric(difftime(date_time_round, warm_start, units = "mins")))

# --- Step 4. Fit Newton’s Law models ---
fit_cool <- nlsLM(
  temperature ~ Teq + (T0 - Teq) * exp(-k * t_rel),
  data = cool_df,
  start = list(Teq = min(cool_df$temperature),
               T0  = max(cool_df$temperature),
               k   = 0.01)
)

fit_warm <- nlsLM(
  temperature ~ Teq - (Teq - T0) * exp(-k * t_rel),
  data = warm_df,
  start = list(Teq = max(warm_df$temperature),
               T0  = min(warm_df$temperature),
               k   = 0.01)
)

cool_df$fit <- predict(fit_cool)
warm_df$fit <- predict(fit_warm)

# --- Step 5. Plot full series with fits overlaid ---
ggplot(df, aes(x = date_time_round, y = temperature)) +
  geom_line(color = "grey40", linewidth = 0.7) +
  geom_point(data = cool_df, color = "blue", size = 1.2) +
  geom_line(data = cool_df, aes(y = fit), color = "blue", linewidth = 1) +
  geom_point(data = warm_df, color = "red", size = 1.2) +
  geom_line(data = warm_df, aes(y = fit), color = "red", linewidth = 1) +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(x = "Time", y = "Temperature (°C)",
       title = "Cooling & Warming Dynamics",
       subtitle = "Blue = cooling rate, Red = warming rate") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

library(dplyr)
library(ggplot2)
library(lubridate)
library(minpack.lm)

# ---- Params you can tweak ----
warm_search_start <- ymd_hms("2025-08-16 15:00:00")  # limit warming search to after this time
cool_window_min   <- 2   # mins after event to keep for fit
warm_window_min   <- 2

# ---- 1) Slopes + add an index ----
df <- initial_curve %>%
  arrange(date_time_round) %>%
  mutate(
    i      = row_number(),  # <-- index column!
    t_mins = as.numeric(difftime(date_time_round, first(date_time_round), units = "mins")),
    slope  = c(NA, diff(temperature) / diff(t_mins))  # °C per minute
  )

# ---- 2) Pick indices ----
cool_idx <- df %>%
  filter(!is.na(slope)) %>%
  slice_min(slope, n = 1, with_ties = FALSE) %>%
  pull(i)

warm_idx <- df %>%
  filter(date_time_round >= warm_search_start, !is.na(slope)) %>%
  slice_max(slope, n = 1, with_ties = FALSE) %>%
  pull(i)

cool_point <- df[cool_idx, ]
warm_point <- df[warm_idx, ]

cool_rate <- cool_point$slope/60     # °C/s
warm_rate <- warm_point$slope/60

cat("Cooling rate:", cool_rate, "°C/s\n")
cat("Warming rate:", warm_rate, "°C/s\n")

# --- Make labels ---
cool_label <- paste0("Max cooling: ", round(cool_rate, 5), " °C/s")
warm_label <- paste0("Max warming: ", round(warm_rate, 5), " °C/s")
cool_rate_min <- cool_rate * 60
warm_rate_min <- warm_rate * 60

# --- Plot with labels ---
x <- ggplot(df, aes(x = date_time_round, y = temperature)) +
  geom_line(color = "grey40", linewidth = 1) +
  # mark points
  geom_point(data = cool_point, aes(x = date_time_round, y = temperature),
             color = "blue", size = 3) +
  geom_point(data = warm_point, aes(x = date_time_round, y = temperature),
             color = "red", size = 3) +
  # add text labels near points
  annotate("text", x = cool_point$date_time_round, y = cool_point$temperature,
           label = cool_label, vjust = -1, hjust = 0, color = "blue") +
  annotate("text", x = warm_point$date_time_round, y = warm_point$temperature,
           label = warm_label, vjust = -1, hjust = 1, color = "red") +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(x = "Time", y = "Temperature (°C)",
       title = "Cooling & Warming Dynamics",
       subtitle = "Max cooling/warming rates annotated") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"), 
    panel.grid.major = element_blank()
  )

# Save as PNG (adjust width/height/dpi as needed for publication)
ggsave(
  filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/radiator_experiment_curve.png",
  plot = x,
  width = 8,
  height = 6,
  dpi = 300
)

library(ggplot2)
library(dplyr)
library(grid)   # for arrow()

# ---------------------------
# Parameters
# ---------------------------
C_room <- 200000
UA      <- 200
Tamb    <- 25
Tcool   <- 20
dt      <- 1
t_cool  <- 4000
t_warm  <- 4000

# Cooling phase
time_cool <- seq(0, t_cool, by = dt)
T_cool <- Tamb + (Tcool - Tamb) * (1 - exp(-(UA/C_room) * time_cool))

# Warming phase
time_warm <- seq(0, t_warm, by = dt)
T_warm <- Tcool + (Tamb - Tcool) * (1 - exp(-(UA/C_room) * time_warm))

df <- data.frame(
  time   = c(time_cool, t_cool + time_warm),
  Temp   = c(T_cool, T_warm),
  phase  = c(rep("Cooling", length(time_cool)),
             rep("Warming", length(time_warm)))
)

# Initial slopes
slope_cool <- (Tcool - Tamb) * (UA/C_room)
slope_warm <- (Tamb - Tcool) * (UA/C_room)

# ---------------------------
# Plot
# ---------------------------
r <- ggplot(df, aes(x = time, y = Temp, color = phase)) +
  geom_line(size = 1.2) +
  # Initial slope lines
  geom_abline(intercept = Tamb, slope = slope_cool,
              linetype = "dashed", color = "blue") +
  geom_abline(intercept = Tcool - slope_warm * t_cool, slope = slope_warm,
              linetype = "dashed", color = "red") +
  # Annotations
  annotate("text", x = 500, y = Tamb - 0.5,
           label = "Initial cooling rate (dT/dt)", color = "blue", hjust = 0) +
  annotate("text", x = t_cool + 500, y = Tcool + 0.5,
           label = "Initial warming rate (dT/dt)", color = "red", hjust = 0) +
  annotate("text", x = 0, y = Tamb + 0.5,
           label = "Ambient equilibrium", color = "black", hjust = -2.75, vjust = 2.5) +
  annotate("text", x = t_cool, y = Tcool - 0.5,
           label = "Cooled equilibrium", color = "black", hjust = 1.1, vjust = -1.5) +
  # ΔT arrow
  annotate("segment", x = -200, xend = -200, y = Tcool, yend = Tamb,
           arrow = arrow(ends = "both", length = unit(0.2, "cm")),
           color = "darkgreen", size = 1) +
  annotate("text", x = -300, y = (Tamb + Tcool)/2,
           label = "Delta~T", color = "darkgreen", angle = 90, parse = TRUE) +
  # Labels and theme
  labs(
    x = "Time",
    y = "Temperature °C",
    title = "Theoretical Cooling/Warming Curve of a Room",
    subtitle = "Dashed lines show initial rates of temperature change"
  ) +
  scale_color_manual(values = c("Cooling" = "blue", "Warming" = "red")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

r

# Save as PNG (adjust width/height/dpi as needed for publication)
ggsave(
  filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/theoretical_curve.png",
  plot = r,
  width = 8,
  height = 6,
  dpi = 300
)
