rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO")

library(tidyverse)
library(readxl)
library(lubridate)
library(lubridate)

# ----- File path & timezone -----
path <- "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Mead/MEAD_DATA_HEATER_EXPERIMENT.xlsx"
tz_local <- "America/Detroit"

# ----- Helper: interpret Excel times as local wall time (no clock shift) -----
to_local_walltime <- function(x, tz = tz_local){
  if (inherits(x, "POSIXct")) {
    force_tz(x, tz)
  } else if (is.numeric(x)) {
    # Excel serial days since 1899-12-30
    force_tz(as_datetime((x - 25569) * 86400, tz = "UTC"), tz)
  } else {
    parsed <- parse_date_time(
      x,
      orders = c("Ymd HMS","Ymd HM","Ymd",
                 "mdY HMS","mdY HM","mdY",
                 "dmy HMS","dmy HM","dmy"),
      tz = "UTC", quiet = TRUE
    )
    force_tz(parsed, tz)
  }
}

# ----- Readers -----
read_simple <- function(sheet_name, label){
  read_excel(path, sheet = sheet_name) %>%
    select(date, temp) %>%
    mutate(
      date_time = to_local_walltime(date),
      value     = as.numeric(temp),
      source    = label
    ) %>%
    select(date_time, source, value) %>%
    arrange(date_time)
}

read_psy <- function(){
  read_excel(path, sheet = "ICU_PSY") %>%
    select(date, probe_temp, integrated_temp) %>%
    mutate(date_time = to_local_walltime(date)) %>%
    pivot_longer(c(probe_temp, integrated_temp),
                 names_to = "measurement", values_to = "value") %>%
    mutate(
      source = recode(
        measurement,
        "probe_temp"      = "ICU Psychrometer - Probe",
        "integrated_temp" = "ICU Psychrometer - Integrated"
      ),
      value = as.numeric(value)
    ) %>%
    select(date_time, source, value) %>%
    arrange(date_time)
}

# ----- Load all sheets & combine -----
wall_entrance <- read_simple("wall_entrance", "Wall Entrance")
wall_back     <- read_simple("wall_back",     "Wall Back")
near_heater   <- read_simple("near_heater",   "Near Heater")
ICU_back      <- read_simple("ICU_back",      "Back of ICU")
ICU_PSY_long  <- read_psy()

exp_df <- bind_rows(
  wall_entrance, wall_back, near_heater, ICU_back, ICU_PSY_long
) %>% arrange(date_time)

# Infer the experiment date from the first timestamp (adjust if needed)
exp_date <- as_date(min(exp_df$date_time, na.rm = TRUE), tz = tz_local)

# ----- Shaded intervals (adjust times here if needed) -----
heater_on_start <- force_tz(as_datetime(paste(exp_date, "12:40:00")), tz_local)
heater_on_end   <- force_tz(as_datetime(paste(exp_date, "14:30:00")), tz_local)

cool_start <- force_tz(as_datetime(paste(exp_date, "14:31:00")), tz_local)
cool_end   <- force_tz(as_datetime(paste(exp_date, "15:00:00")), tz_local)

# Define all shaded phases
shade_df <- tibble::tribble(
  ~xmin, ~xmax, ~phase,
  force_tz(as_datetime(paste(exp_date, "12:00:00")), tz_local),
  force_tz(as_datetime(paste(exp_date, "12:39:00")), tz_local),
  "Control",
  
  force_tz(as_datetime(paste(exp_date, "12:40:00")), tz_local),
  force_tz(as_datetime(paste(exp_date, "14:30:00")), tz_local),
  "Heater ON",
  
  force_tz(as_datetime(paste(exp_date, "14:31:00")), tz_local),
  force_tz(as_datetime(paste(exp_date, "15:00:00")), tz_local),
  "Cool-down"
)

# ----- Plot with shaded intervals -----
# Plot with shaded regions added to legend
p <- ggplot(exp_df, aes(x = date_time, y = value, color = source)) +
  # Shaded phases behind lines; mapped to 'phase' so they appear in legend
  geom_rect(
    data = shade_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
    inherit.aes = FALSE,
    alpha = 0.12,
    show.legend = TRUE
  ) +
  # Temperature lines
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = "Heater Experiment — Temperature Over Time",
    x = "Time (America/Detroit)",
    y = "Temperature (°C)",
    color = "Sensor",
    fill  = "Phase"
  ) +
  scale_x_datetime(date_breaks = "20 min", date_labels = "%H:%M") +
  scale_y_continuous(limits = c(7, 12), breaks = seq(7, 12, by = 1)) +
  scale_fill_manual(
    values = c(
      "Control"   = "grey80",
      "Heater ON" = "#ff0000",
      "Cool-down" = "#1f77b4"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.background= element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(color = "black")
  ) +
  coord_cartesian(xlim = c(start_ts, end_ts)) +
  scale_x_datetime(date_breaks = "20 min", date_labels = "%H:%M")

print(p)


start_ts <- floor_date(min(exp_df$date_time, na.rm = TRUE), "day") + hours(12)
end_ts   <- max(exp_df$date_time, na.rm = TRUE)

p_plus <- p +
  coord_cartesian(xlim = c(start_ts, end_ts)) +
  scale_x_datetime(date_breaks = "20 min", date_labels = "%H:%M")

print(p_plus)

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/MEAD_heater_experiment.png",  # File path and name
       plot = p,   # If you want to save the last plot you created
       width = 12,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)
