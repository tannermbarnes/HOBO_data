rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/")
library(readxl)
library(tidyverse)
library(lubridate)

meadwall <- read_excel("data/Mead/MEAD_DATA_MAY_2025_ON.xlsx", sheet = "wall_back") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>% 
  mutate(
    date_time = as.POSIXct(date_time, format ="Y-%m-%d %H:%M:%S"), 
    source = "Wall Back") %>% 
  filter(date_time >= as.POSIXct("2024-10-01 11:32:59"))
  
mead200 <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/MEAD HOBO 3 - 200 Oct18.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>% 
  mutate(
    date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S"),  # Ensure date_time is in POSIXct format
    source = "200 ft"
  ) %>%
  filter(date_time >= as.POSIXct("2024-10-02 20:47:13"))

mead400 <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/MEAD HOBO 1 - 400ft.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>% 
  mutate(
    date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S"),  # Ensure date_time is in POSIXct format
    source = "400 ft"
  ) %>%
  filter(date_time >= as.POSIXct("2024-10-01 16:30:00"))

mead_humidity_treatment <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/MEAD_psychrometer_treatment.xlsx") %>% 
  select(date_time = `Date-Time (EST)`, wet_bulb, dry_bulb) %>% 
  mutate(source = "Treatment")

mead_control <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/MEAD HOBO 4 - Control.xlsx") %>% 
  select(date_time = `Date-Time (EDT)`, temp = `Temperature (°C)`) %>% 
  mutate(
    date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S"),  # Ensure date_time is in POSIXct format
    source = "Control"
  ) %>%
  filter(date_time >= as.POSIXct("2024-10-01 11:39:28"))

# Define psychrometric constant (hPa/°C)
gamma <- 0.00066 

# Magnus equation for saturation vapor pressure (hPa)
saturation_vapor_pressure <- function(T) {
  6.112 * exp((17.62 * T) / (243.12 + T))
}

# Compute saturation vapor pressure for dry-bulb and wet-bulb
humidity_treatment <- mead_humidity_treatment %>%
  mutate(
    e_db = saturation_vapor_pressure(dry_bulb),
    e_wb = saturation_vapor_pressure(wet_bulb),
    RH = 100 * (e_wb - gamma * (dry_bulb - wet_bulb)) / e_db
  )


# Modify all datasets to extract month and day only, ignoring the year
# Create a month-day column that doesn't include the year
combined_data <- bind_rows(meadwall, mead200, mead400, humidity_treatment, mead_control) %>%
  mutate(month_day = format(date_time, "%m-%d"))  # Extract month and day as a string


# Mutate to set the year for August-December as 2024 and January-July as 2025
combined_data <- combined_data %>%
  mutate(
    month_day = format(as.Date(date_time), "%m-%d"),  # Extract month and day
    year_adjusted = ifelse(month_day >= "09-01", "2024", "2025"),  # Assign year based on month
    month_day = as.Date(paste(year_adjusted, month_day, sep = "-"), format = "%Y-%m-%d")  # Combine year and month-day
  )

# Define custom x-axis range from August 1 to July 31 of the next year
x_limits <- as.Date(c("2024-09-01", "2025-08-31"))

# Plot the combined data, now using month_day on the x-axis
combined_data %>% 
  filter(source != "Treatment") %>% 
  ggplot(aes(x = month_day, y = temp, color = source, group = source)) +
  
  # Smoothed line (loess by default)
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +  
  
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Source"
  ) +
  scale_x_date(
    date_labels = "%b %d", 
    date_breaks = "1 month", 
    limits = x_limits
  ) +
  scale_color_discrete(name = "Source") +
  scale_y_continuous(limits = c(6.5, 8.5)) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.99),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )



ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/MEAD_temp_treatment.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 8,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)



# Plot RH over time
ggplot(humidity_treatment, aes(x = date_time, y = RH)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +  # Line graph with blue color
  labs(
    x = "Date-Time",
    y = "Relative Humidity (%)"
  ) +
  theme_bw() +  # Clean theme
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

ggsave(filename = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/MEAD_humidity.png",  # File path and name
       plot = last_plot(),   # If you want to save the last plot you created
       width = 8,           # Set the width to make it wide
       height = 6,           # Adjust the height
       dpi = 300             # Set the resolution to 300 DPI for high quality
)


#----------------- Compare Mead Mine stability ----------------------------------------#
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(lubridate)

# ----- File path -----
path <- "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/data/Mead/Mead_test_stability.xlsx"

# ----- Read sheets -----
wall <- read_excel(path, sheet = "wall") %>%
  mutate(source = "Wall")

back <- read_excel(path, sheet = "back_2018-2024") %>%
  mutate(source = "Back (2019–2024)")

after_wall <- read_excel(path, sheet = "after_wall") %>%
  mutate(source = "After Wall")

# ----- Combine -----
combined <- bind_rows(wall, back, after_wall) %>%
  mutate(date_only = as.Date(date_time))

# ----- Define seasonal periods by month -----
combined <- combined %>%
  mutate(
    month_num = month(date_only),
    period = case_when(
      month_num %in% c(11, 12, 1, 2, 3, 4) ~ "Winter (Nov–Apr)",
      month_num %in% c(5, 6, 7, 8, 9)      ~ "Summer (May–Sep)",
      TRUE ~ NA_character_   # e.g., October excluded
    )
  ) %>%
  filter(!is.na(period)) %>%
  mutate(period = factor(period, levels = c("Winter (Nov–Apr)", "Summer (May–Sep)")))

combined <- combined %>%
  mutate(
    month_num  = lubridate::month(date_only),
    month_name = lubridate::month(date_only, label = TRUE, abbr = TRUE)  # "Jan", "Feb", ...
  )

# ----- Summary stats -----
summary_means <- combined %>%
  group_by(source, period) %>%
  summarise(n = n(), mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

print(summary_means, n = Inf)

# ----- Run t-tests -----
tt_results <- combined %>%
  group_by(source) %>%
  do(tidy(t.test(temp ~ period, data = .))) %>%
  ungroup()

print(tt_results)

# ----- Boxplot -----
p <- ggplot(combined, aes(x = period, y = temp, fill = period)) +
  geom_boxplot(outlier.size = 0.7, alpha = 0.8, width = 0.65) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, stroke = 0.3, fill = "white") +
  facet_wrap(~ source, scales = "free_y") +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    fill = "Period",
    title = "Temperature by Period at Different Spots in Mead Mine"
  ) +
  scale_fill_manual(values = c("Winter (Nov–Apr)" = "#5DADE2",  # blue
                               "Summer (May–Sep)" = "#F5B041")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    axis.line        = element_line(color = "black")
  )

print(p)

# ----- (Optional) Save -----
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/Mead_test_stability_boxplot.png",
       plot = p, width = 10, height = 6, dpi = 300)


v <- ggplot(combined, aes(x = month_name, y = temp, fill = month_name)) +
  geom_boxplot(outlier.size = 0.7, alpha = 0.8, width = 0.65) +
  facet_wrap(~ source, scales = "free_y") +
  labs(
    x = "Month",
    y = "Temperature (°C)",
    fill = "Month",
    title = "Temperature by Month at Different Spots in Mead Mine"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    axis.line        = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ----- (Optional) Save -----
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/HOBO/figures/Mead_temp_monthly.png",
       plot = v, width = 10, height = 6, dpi = 300)


library(dplyr)
library(car)   # for leveneTest
library(broom)

# --- Variance tests per source ---

# F-test for equal variances (Winter vs Summer)
f_results <- combined %>%
  group_by(source) %>%
  do(tidy(var.test(temp ~ period, data = .))) %>%
  ungroup()

print(f_results)

library(dplyr)
library(car)
library(broom)

# Robust Levene’s test per source
levene_results <- combined %>%
  group_by(source) %>%
  group_map(~{
    lv <- car::leveneTest(temp ~ period, data = .x)
    tibble(
      source = unique(.x$source),
      statistic = as.numeric(lv[1, "F value"]),
      df1 = as.numeric(lv[1, "Df"]),
      df2 = as.numeric(lv[2, "Df"]),
      p.value = as.numeric(lv[1, "Pr(>F)"])
    )
  }) %>% bind_rows()

print(levene_results)

# Helpful summary of spread by season
spread_summary <- combined %>%
  group_by(source, period) %>%
  summarise(
    n  = sum(!is.na(temp)),
    mean = mean(temp, na.rm = TRUE),
    sd   = sd(temp, na.rm = TRUE),
    var  = var(temp, na.rm = TRUE),
    iqr  = IQR(temp, na.rm = TRUE),
    .groups = "drop"
  )
print(spread_summary, n = Inf)

library(lubridate)

combined_daily <- combined %>%
  mutate(day = as.Date(date_time)) %>%
  group_by(source, period, day) %>%
  summarise(temp = mean(temp, na.rm = TRUE), .groups = "drop")

# F-test on daily means
f_results_daily <- combined_daily %>%
  group_by(source) %>%
  do(tidy(var.test(temp ~ period, data = .))) %>%
  ungroup()


# --- F-test on daily means (you already ran this) ---
print(f_results_daily)

library(car)
library(dplyr)

levene_daily <- combined_daily %>%
  group_by(source) %>%
  group_modify(~{
    lv <- car::leveneTest(temp ~ period, data = .x)
    tibble(
      statistic = as.numeric(lv[1, "F value"]),
      df1       = as.numeric(lv[1, "Df"]),
      df2       = as.numeric(lv[2, "Df"]),
      p.value   = as.numeric(lv[1, "Pr(>F)"])
    )
  }) %>%
  ungroup()

print(levene_daily)


combined_var_tests <- f_results_daily %>%
  select(source, F_ratio = estimate, F_p = p.value) %>%
  left_join(levene_daily %>% select(source, Levene_F = statistic, Levene_p = p.value),
            by = "source")

print(combined_var_tests)

library(dplyr)
library(effsize)  # for cohen.d

# --- Coefficient of Variation (CV) per source x period ---
cv_summary <- combined %>%
  group_by(source, period) %>%
  summarise(
    n     = n(),
    mean  = mean(temp, na.rm = TRUE),
    sd    = sd(temp, na.rm = TRUE),
    cv    = sd / mean,
    .groups = "drop"
  )

print(cv_summary, n = Inf)

# --- Cohen's d: Winter vs Summer for each source ---
cohen_results <- combined %>%
  group_by(source) %>%
  group_map(~{
    cd <- effsize::cohen.d(temp ~ period, data = .x, na.rm = TRUE)
    tibble(
      source    = .y$source,          # use .y to get the group name
      cohen_d   = cd$estimate,
      magnitude = cd$magnitude
    )
  }) %>%
  bind_rows()

print(cohen_results)


# --- Merge for compact reporting ---
summary_effects <- cv_summary %>%
  select(source, period, mean, sd, cv) %>%
  left_join(cohen_results, by = "source")

print(summary_effects, n = Inf)
