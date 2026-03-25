rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO")
library(readxl)
library(tidyverse)
library(lubridate)

# Read the data and clean them
henwood_experiment <- read_excel("data/HENWOOD_DATA.xlsx", 
                                 sheet = "Experiment") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Experiment")

henwood_control <- read_excel("data/HENWOOD_DATA.xlsx",
                              sheet = "Control") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Control")

henwood_skylight <- read_excel("data/HENWOOD_DATA.xlsx", 
                               sheet = "Skylight") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>%
  mutate(source = "Skylight")

tippy_dam <- read.csv("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/data/Tippy_dam_temperature 2019-2020.csv") %>% 
  select(date_time = Date, temp = Dry_Bulb._C_22) %>%
  mutate(
    source = "Tippy Dam",
    date_time = as.POSIXct(date_time, format = "%m/%d/%Y")  # Adjust format if needed
  )

henwood_psychrometer <- read_excel("data/HENWOOD_DATA.xlsx", 
                                  sheet = "psychrometer") %>% 
  select(date_time = "datetime", probe_temp = "probe_temp", temp = "integrated_temp") %>%
  mutate(source = "Psychrometer")

# Modify all datasets to extract month and day only, ignoring the year
# Create a month-day column that doesn't include the year
combined_data <- bind_rows(henwood_experiment, henwood_control, henwood_skylight, tippy_dam, henwood_psychrometer) %>%
  mutate(month_day = format(date_time, "%m-%d"))  # Extract month and day as a string

# Mutate and create month_day column that only includes month and day
combined_data <- combined_data %>%
  mutate(month_day = format(as.Date(date_time), "%m-%d"))


# Mutate to set the year for August-December as 2024 and January-July as 2025
combined_data <- combined_data %>%
  mutate(
    month_day = format(as.Date(date_time), "%m-%d"),  # Extract month and day
    year_adjusted = ifelse(month_day >= "09-01", "2024", "2025"),  # Assign year based on month
    month_day = as.Date(paste(year_adjusted, month_day, sep = "-"), format = "%Y-%m-%d")  # Combine year and month-day
  )

# Define custom x-axis range from August 1 to July 31 of the next year
x_limits <- as.Date(c("2024-09-01", "2025-08-31"))

# a named vector is handy – the names must exactly match the values in `source`
mycols <- c(
  Control      = "#1b9e77",
  Experiment   = "#d95f02",
  Skylight     = "#7570b3",
  Psychrometer = "#e7298a",
  `Tippy Dam`  = "#66a61e"
)
# Plot the combined data, now using month_day on the x-axis
ggplot(combined_data,
       aes(x = month_day, y = temp, color = source, group = source)) +
  geom_line() +
  labs(x = "Month", y = "Temperature (°C)", color = "Source") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = x_limits) +
  scale_color_manual(
      name   = "Source",
      values = mycols,
      labels = c("Henwood\nControl",
                 "Henwood\nExperiment",
                 "Henwood\nSkylight",
                 "Henwood\nPsychrometer",
                 "Tippy Dam")
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    legend.position = "top",                     # move legend to top
    legend.direction = "horizontal",             # horizontal layout
    legend.justification = "center",             
    legend.box.just = "center",
    legend.background = element_rect(fill = "white", color = NA), # white background
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA)    # full white plot background
  )


ggsave(filename = "figures/Henwood_Tippy_temperature.png",  # File path and name
  plot = last_plot(),   # If you want to save the last plot you created
  width = 12,           # Set the width to make it wide
  height = 6,           # Adjust the height
  dpi = 300             # Set the resolution to 300 DPI for high quality
)


# Filter rows that are outside the limits of the x-axis
outside_scale_rows <- combined_data %>%
  filter(month_day < x_limits[1] | month_day > x_limits[2])

# Display the rows outside the scale range
outside_scale_rows

# Check for missing temperature values
missing_temp_rows <- combined_data %>%
  filter(is.na(temp))

# Display rows with missing temperature values
print(missing_temp_rows, n = 82)

#---------- Check if this is a variable site or stable site based on temperature differences -------- #
# between Nov - April and May - October
head(combined_data)
unique(combined_data$source)

library(dplyr)
library(ggplot2)
library(lubridate)
library(broom)

# --- 1) Define windows (inclusive) ---
period1_start <- as.Date("2024-11-01")
period1_end   <- as.Date("2025-04-30")
period2_start <- as.Date("2025-05-01")
period2_end   <- as.Date("2025-09-09")

# Ensure you have a POSIXct datetime column named date_time; adjust if needed
# combined_data$date_time <- lubridate::ymd_hms(combined_data$date_time, tz = "America/Detroit")

# ---- Tag periods AND add year-month fields ----
cd2 <- combined_data %>%
  mutate(
    date_only  = as.Date(date_time, tz = "America/Detroit"),
    period = case_when(
      date_only >= period1_start & date_only <= period1_end ~ "Winter (Nov–Apr)",
      date_only >= period2_start & date_only <= period2_end ~ "Summer (May–Sep 9)",
      TRUE ~ NA_character_
    ),
    year_month = floor_date(date_only, "month"),
    ym_label   = format(year_month, "%Y-%b")
  ) %>%
  filter(!is.na(period)) %>%
  mutate(
    period   = factor(period, levels = c("Winter (Nov–Apr)", "Summer (May–Sep 9)")),
    ym_label = fct_reorder(ym_label, year_month)  # keeps x in chronological order
  )

# --- 3) Sanity check: means by source & period ---
summary_means <- cd2 %>%
  group_by(source, period) %>%
  summarise(n = n(), mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")
print(summary_means, n = Inf)

# If these still look flipped, run this to confirm which months landed in each bucket:
# cd2 %>% mutate(mo = month(date_only, label = TRUE)) %>% count(period, mo, sort = TRUE)

# --- 4) (Optional) t-tests again, clearly labeled ---
tt <- cd2 %>%
  group_by(source) %>%
  do(tidy(t.test(temp ~ period, data = .))) %>%
  ungroup()
print(tt)

# --- 5) Boxplot: Winter vs Summer per logger ---
ggplot(cd2, aes(x = period, y = temp, fill = period)) +
  geom_boxplot(outlier.size = 0.7, alpha = 0.8, width = 0.65) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, stroke = 0.3, fill = "white") +
  facet_wrap(~ source, scales = "free_y") +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    fill = "Period",
    title = "Temperature by Period per Logger"
  ) +
  scale_fill_manual(values = c("Winter (Nov–Apr)" = "#5DADE2",  # blue
                               "Summer (May–Sep 9)" = "#F5B041")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    axis.line        = element_line(color = "black"), 
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "Henwood_boxplot_variable.png",
  path = "figures",
  width = 10, height = 6, dpi = 300
)

# Summarize by month
cd2 %>%
  group_by(source, month_lab) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp   = sd(temp, na.rm = TRUE),
            .groups = "drop")

# ---- Boxplot by Year-Month (faceted by logger), legend on top ----
ggplot(cd2, aes(x = ym_label, y = temp, fill = period)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.85, width = 0.6) +
  facet_wrap(~ source, scales = "free_y") +
  labs(
    x = "Year–Month",
    y = "Temperature (°C)",
    fill = "Period",
    title = "Monthly Temperature Distributions by Logger and Period"
  ) +
  scale_fill_manual(values = c("Winter (Nov–Apr)" = "#5DADE2",
                               "Summer (May–Sep 9)" = "#F5B041")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position  = "top",          # put legend on top
    legend.direction = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    axis.line        = element_line(color = "black")
  )

ggsave(
  filename = "Henwood_boxplot_by_month.png",
  path = "figures",
  width = 10, height = 6, dpi = 300
)
# monthly mean line plots
monthly_means <- cd2 %>%
  group_by(source, period, year_month) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

ggplot(monthly_means, aes(x = year_month, y = mean_temp, color = period)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ source, scales = "free_y") +
  labs(x = "Year–Month", y = "Mean Temperature (°C)", color = "Period") +
  scale_color_manual(values = c("Winter (Nov–Apr)" = "#2E86C1",
                                "Summer (May–Sep 9)" = "#E67E22")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%b") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )



weather <- readr::read_csv("data/Watersmeet_Sept2024-Sept2025_weather_averages.csv")
# Make sure date is parsed properly
weather <- weather %>%
  mutate(
    date = lubridate::mdy(DATE),  # if format is MM/DD/YYYY
    month = lubridate::month(date, label = TRUE),
    year  = lubridate::year(date)
  )

weather <- weather %>%
  mutate(
    TAVG_C = (TAVG - 32) * 5/9,
    TMAX_C = (TMAX - 32) * 5/9,
    TMIN_C = (TMIN - 32) * 5/9
  )


ggplot() +
  # external station temps
  geom_line(data = weather, aes(x = date, y = TAVG_C, color = "External (TAVG)"), linewidth = 0.7) +
  geom_line(data = weather, aes(x = date, y = TMAX_C, color = "External (TMAX)"), linewidth = 0.5, linetype = "dashed") +
  geom_line(data = weather, aes(x = date, y = TMIN_C, color = "External (TMIN)"), linewidth = 0.5, linetype = "dashed") +
  
  # internal logger temps
  geom_line(data = cd2, aes(x = date_only, y = temp, color = source), alpha = 0.6) +
  
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = "Source",
    title = "External Weather Station vs Internal Logger Temperatures"
  ) +
  theme_minimal(base_size = 14)

weather_monthly <- weather %>%
  group_by(year, month) %>%
  summarise(temp = mean(TAVG_C, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "External (TAVG)",
         ym_label = paste(year, month, sep = "-"))

cd2_monthly <- cd2 %>%
  mutate(ym_label = paste(year(date_only), lubridate::month(date_only, label = TRUE), sep = "-"))

combined_plot <- bind_rows(
  cd2_monthly %>% select(source, ym_label, temp, period),
  weather_monthly %>% mutate(period = NA_character_)  # station has no seasonal tag
)

ggplot(combined_plot, aes(x = ym_label, y = temp, fill = period)) +
  geom_boxplot(outlier.size = 0.6, alpha = 0.85, width = 0.6) +
  facet_wrap(~ source, scales = "free_y") +
  labs(x = "Year–Month", y = "Temperature (°C)", title = "Logger vs Weather Station Temperatures") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cd2_summary <- cd2 %>%
  group_by(source, year = year(date_only), month = month(date_only, label = TRUE)) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

weather_summary <- weather %>%
  group_by(year, month) %>%
  summarise(mean_temp = mean(TAVG_C, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "External (TAVG)")

compare_summary <- bind_rows(cd2_summary, weather_summary)

ggplot(compare_summary, aes(x = month, y = mean_temp, color = source, group = source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ year) +
  labs(y = "Mean Temperature (°C)", title = "Monthly Mean Temperatures: Internal vs External") +
  theme_minimal(base_size = 14)

###### ----- Humidity data from psychrometer ----- ######
# ---- read psychrometer and compute RH --------------------------------------
henwood_psychrometer <- read_excel("data/HENWOOD_DATA.xlsx", 
                                   sheet = "psychrometer") %>% 
  select(date_time = "datetime",
         probe_temp = "probe_temp",      # saturated (wet‑bulb) reading
         temp       = "integrated_temp") %>% # dry‑bulb / air temp
  mutate(source = "Psychrometer")

## ensure numeric and add RH column
gamma <- 0.00066                      # psychrometric constant
svp <- function(t_c) 6.112 * exp((17.62*t_c)/(243.12+t_c))

henwood_psychrometer <- henwood_psychrometer %>%
  mutate(
    probe_temp = as.numeric(probe_temp),
    temp       = as.numeric(temp),
    humidity   = 100 * (svp(probe_temp) - gamma * (temp - probe_temp)) /
                       svp(temp),
    # keep within [0,100] just in case of tiny numeric over/undershoots
    humidity   = pmin(100, pmax(0, humidity))
  )

# optional: view a few values
head(henwood_psychrometer)

# ---- graph the humidity time series ---------------------------------------
ggplot(henwood_psychrometer, aes(x = date_time, y = humidity)) +
  geom_line(color = "#117A65", linewidth = 0.8) +
  labs(x = "Date‑time", y = "Relative humidity (%)",
       title = "Psychrometer‑derived humidity at Henwood") +
  theme_minimal(base_size = 14)

# …or if you’d rather include it in `combined_data`:
combined_data <- bind_rows(henwood_experiment, henwood_control,
                           henwood_skylight, tippy_dam,
                           henwood_psychrometer)
# the `humidity` column will then propagate and you can facet/colour by source etc.
ggsave(filename = "figures/Henwood_humidity.png",  # File path and name
  plot = last_plot(),   # If you want to save the last plot you created
  width = 8,           # Set the width to make it wide
  height = 6,           # Adjust the height
  dpi = 300             # Set the resolution to 300 DPI for high quality
)

# ---- plot temperature over time from Oct 2, 2024 --------------------------
temp_start_date <- as.POSIXct("2024-10-02 00:00:00", tz = "America/Detroit")
plot_end_date <- max(combined_data$date_time, na.rm = TRUE)

winter_windows <- tibble(
  xmin = as.POSIXct(c("2024-11-01 00:00:00", "2025-11-01 00:00:00"), tz = "America/Detroit"),
  xmax = as.POSIXct(c("2025-04-30 23:59:59", format(plot_end_date, "%Y-%m-%d %H:%M:%S")),
                    tz = "America/Detroit")
) |>
  filter(xmin <= plot_end_date)

combined_data |>
  filter(!is.na(temp), date_time >= temp_start_date) |>
  ggplot(aes(x = date_time, y = temp, color = source)) +
  geom_rect(
    data = winter_windows,
    inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "lightblue",
    alpha = 0.25
  ) +
  geom_line(linewidth = 0.7) +
  labs(
    x = "Date", 
    y = "Temperature (ºC)", 
    color = "Source", 
    caption = "Shaded regions indicate hibernation season"
  ) +
  scale_color_manual(
    name = "Source",
    values = mycols,
    labels = c("Henwood\nControl",
               "Henwood\nExperiment",
               "Henwood\nSkylight",
               "Henwood\nPsychrometer",
               "Tippy Dam")
  ) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal(base_size = 14) +
theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  panel.grid.minor = element_blank(),
  legend.position = "top",
  axis.title.x = element_text(face = "bold"),
  axis.title.y = element_text(face = "bold"),
  plot.caption = element_text(hjust = 0.5)
)


ggsave(
  filename = "figures/Henwood_Temperature_from_2024-10-02.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300
)
