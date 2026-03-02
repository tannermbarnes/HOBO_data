setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO")
library(readxl)
library(dplyr)
library(ggplot2)

bc <- read_excel("data/belt_mine.xlsx", sheet = "BC")
jk <- read_excel("data/belt_mine.xlsx", sheet = "JK")

# Standardize column names
bc <- bc %>%
  rename(
    id = `#`,
    datetime = "date",
    temp_c = "temp"
  ) %>%
  mutate(site = "bc")

jk <- jk %>%
  rename(
    id = `#`,
    datetime = "date",
    temp_c = "temp"
  ) %>%
  mutate(site = "jk")

belt <- bind_rows(bc, jk)

# Plot temperature over time starting at the earliest time
start_time <- as.POSIXct("2025-04-16 12:54:47", tz = "America/New_York")
belt_plot <- belt %>%
  mutate(datetime = as.POSIXct(datetime, tz = "America/New_York")) %>%
  filter(datetime >= start_time)

plot_belt <- ggplot(belt_plot, aes(x = datetime, y = temp_c, color = site)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  labs(
    title = "Belt Temperature Through Time",
    subtitle = "From 2025-04-16 12:54:47 onward",
    x = "Time",
    y = "Temperature (°C)",
    color = "Site"
  ) +
  scale_color_manual(values = c(bc = "#1B4F72", jk = "#7D3C98")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

plot_belt

ggsave(
  filename = "figures/belt_temperature_time.png",
  plot = plot_belt,
  width = 10,
  height = 6,
  dpi = 300
)

# --- Pastable chunk: monthly x-axis labels and 1°C y-axis ticks ---
plot_belt_dense_axes <- belt_plot %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "America/New_York"),
    temp_c = as.numeric(temp_c)
  ) %>%
  ggplot(aes(x = datetime, y = temp_c, color = site)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  labs(
    title = "Belt Temperature Through Time",
    subtitle = "From 2025-04-16 12:54:47 onward",
    x = "Time",
    y = "Temperature (°C)",
    color = "Section"
  ) +
  scale_color_manual(values = c(bc = "#1B4F72", jk = "#7D3C98")) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    breaks = seq(
      floor(min(belt_plot$temp_c, na.rm = TRUE)),
      ceiling(max(belt_plot$temp_c, na.rm = TRUE)),
      by = 1
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_belt_dense_axes
