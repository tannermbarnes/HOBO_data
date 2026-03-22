rm(list = ls())
setwd("/Users/tannermbarnes/Library/CloudStorage/OneDrive-MichiganTechnologicalUniversity/PhD/HOBO/")
# ---- Libraries ----
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# ---- Helper: map any datetime onto a common Sept→Aug axis ----
to_season_datetime <- function(dt, tz = "America/Detroit"){
  d <- with_tz(dt, tz)
  yr <- ifelse(month(d) >= 9, 2000, 2001)  # Sep–Dec -> 2000; Jan–Aug -> 2001
  make_datetime(
    year  = yr,
    month = month(d),
    day   = day(d),
    hour  = hour(d),
    min   = minute(d),
    sec   = second(d),
    tz    = tz
  )
}

# ======================
# 1) READ EACH DATASET
# ======================

# --- Mead (2019-2020 subset) ---
mead <- read_excel(
  "data/Mead/MEAD_DATA_MAY_2025_ON.xlsx",
  sheet = "control_200"
) %>%
  transmute(
    date_time = as.POSIXct(datetime, tz = "UTC"),  # adjust if needed
    temp      = as.numeric(temp)
  ) %>%
  filter(between(
    date_time,
    as.POSIXct("2024-10-18 17:06:00", tz = "UTC"),
    as.POSIXct("2025-10-18 21:06:00", tz = "UTC")
  )) %>%
  mutate(source = "Mead")

# --- Henwood (EDT timestamps in sheet) ---
henwood <- read_excel(
  "data/HENWOOD_DATA.xlsx",
  sheet = "Experiment"
) %>%
  transmute(
    date_time = suppressWarnings(lubridate::ymd_hms(`Date-Time (EDT)`, tz = "America/Detroit")),
    temp      = as.numeric(`Temperature (°C)`),
    source    = "Henwood"
  ) %>%
  filter(is.finite(temp), !is.na(date_time))

# --- Tippy Dam (date only) ---
tippy_dam <- read.csv("data/Tippy_dam_temperature 2019-2020.csv") %>% 
  transmute(
    date_time = as.POSIXct(Date, format = "%m/%d/%Y", tz = "America/Detroit"),
    temp      = as.numeric(Dry_Bulb._C_22),
    source    = "Tippy Dam"
  ) %>%
  filter(is.finite(temp), !is.na(date_time))

# --- Aztec Mine (HOBO CSV; columns show GMT offset in header) ---
aztec <- read.csv("data/Aztec.csv", skip = 1) %>% 
  # NOTE: backticks required because of special chars in column names
  transmute(
    date_time_raw = `Date.Time..GMT.05.00`,
    temp          = as.numeric(`Temp...C..LGR.S.N..10799593..SEN.S.N..10799593.`),
    rh            = as.numeric(`RH.....LGR.S.N..10799593..SEN.S.N..10799593.`)
  ) %>%
  mutate(
    # parse as local wall-clock times for consistency
    date_time = suppressWarnings(lubridate::mdy_hms(date_time_raw, tz = "America/Detroit")),
    source    = "Aztec Mine"
  ) %>%
  filter(
    !is.na(date_time),
    between(
      date_time,
      as.POSIXct("2017-09-01 00:26:38", tz = "America/Detroit"),
      as.POSIXct("2018-08-31 23:26:38", tz = "America/Detroit")
    )
  ) %>%
  select(date_time, temp, source)

# --- Beatens Cave (HOBO CSV) ---
beatens <- read.csv("data/Beatens_2016_b.csv", skip = 1) %>% 
  transmute(
    date_time_raw = `Date.Time..GMT.04.00`,
    temp          = as.numeric(`Temp...C..LGR.S.N..10799587..SEN.S.N..10799587.`),
    rh            = as.numeric(`RH.....LGR.S.N..10799587..SEN.S.N..10799587.`)
  ) %>%
  mutate(
    date_time = suppressWarnings(lubridate::mdy_hms(date_time_raw, tz = "America/Detroit")),
    source    = "Beatens Cave"
  ) %>%
  filter(
    !is.na(date_time),
    between(
      date_time,
      as.POSIXct("2016-09-17 00:59:54", tz = "America/Detroit"),
      as.POSIXct("2017-09-16 12:59:54", tz = "America/Detroit")
    )
  ) %>%
  select(date_time, temp, source)

# --- South Lake Upper (HOBO CSV) ---
library(dplyr)
library(lubridate)

start_time <- ymd_hm("2024-11-14 14:00", tz = "America/Detroit")
end_time   <- ymd_hm("2025-11-13 10:00", tz = "America/Detroit")


# --- South Lake Upper ---
south_lake_upper <- read.csv(
  "data/SouthLake12(2025).csv",
  skip = 1, check.names = FALSE
) %>%
  transmute(
    date_time_raw = `Date Time, GMT-05:00`,
    temp          = as.numeric(`Temp, °C (LGR S/N: 10799605, SEN S/N: 10799605)`),
    rh            = as.numeric(`RH, % (LGR S/N: 10799605, SEN S/N: 10799605)`)
  ) %>%
  mutate(
    date_time_clean = stringr::str_replace_all(date_time_raw, "[^0-9/: ]", ""),
    date_time = mdy_hm(date_time_clean, tz = "America/Detroit", quiet = TRUE),
    source    = "South Lake Upper"
  ) %>%
  filter(!is.na(date_time),
         date_time >= start_time,
         date_time <= end_time) %>%
  select(date_time, temp, rh, source)

# --- South Lake Lower ---
south_lake_lower <- read.csv(
  "data/SouthLake14(2025).csv",
  skip = 1, check.names = FALSE
) %>%
  transmute(
    date_time_raw = `Date Time, GMT-05:00`,
    temp          = as.numeric(`Temp, °C (LGR S/N: 10799606, SEN S/N: 10799606)`),
    rh            = as.numeric(`RH, % (LGR S/N: 10799606, SEN S/N: 10799606)`)
  ) %>%
  mutate(
    date_time_clean = stringr::str_replace_all(date_time_raw, "[^0-9/: ]", ""),
    date_time = mdy_hm(date_time_clean, tz = "America/Detroit", quiet = TRUE),
    source    = "South Lake Lower"
  ) %>%
  filter(!is.na(date_time),
         date_time >= start_time,
         date_time <= end_time) %>%
  select(date_time, temp, rh, source)

delaware <- read_excel("data/Delaware.xlsx", 
                            sheet = "light") %>% 
  transmute(
    date_time = as.POSIXct(datetime, tz = "America/Detroit"),
    temp = as.numeric(temp),
    source = "Delaware"
  ) %>% 
  filter(date_time > as.POSIXct("2025-02-01 00:00:00", tz = "America/Detroit"))

belt <- read_excel("data/belt_mine.xlsx", 
                            sheet = "JK") %>%
  transmute(
    date_time = as.POSIXct(datetime, tz = "America/Detroit"),
    temp = as.numeric(temp),
    source = "Belt Mine"
  ) %>% 
  filter(date_time > as.POSIXct("2025-04-16 17:57:12", tz = "America/Detroit"))
  
indiana <- read_excel("data/IndianaMine.xlsx", sheet = "BD21-29") %>%
  transmute(
    date_time = as.POSIXct(datetime, tz = "America/Detroit"),
    temp = as.numeric(temp),
    source = "Indiana Mine"
  ) %>%
  filter(date_time > as.POSIXct("2025-12-05 14:57:12", tz = "America/Detroit"))
# ======================
# 2) COMBINE & BUILD SEASON TIMELINE
# ======================
site_dfs <- list(
  mead,
  henwood,
  tippy_dam,
  aztec,
  beatens,
  south_lake_lower,
  south_lake_upper,
  delaware,
  belt
) %>%
  lapply(function(df) {
    df %>%
      mutate(temp = suppressWarnings(readr::parse_number(as.character(temp))))
  })

all_sites <- bind_rows(site_dfs) %>%
  filter(is.finite(temp), !is.na(date_time)) %>%
  mutate(
    # For Mead in UTC, align to local before mapping
    date_time_local = with_tz(date_time, "America/Detroit"),
    season_time     = to_season_datetime(date_time_local, tz = "America/Detroit")
  )

# ======================
# 3) PLOT (polished Option A)
# ======================

# Season window & ticks (must match season_time years)
season_start <- as.POSIXct("2000-09-01 00:00:00", tz = "America/Detroit")
season_end   <- as.POSIXct("2001-08-31 23:59:59", tz = "America/Detroit")
month_breaks <- seq(season_start, season_end, by = "1 month")

# Winter band (Nov–Apr)
winter_xmin <- as.POSIXct("2000-11-01 00:00:00", tz = "America/Detroit")
winter_xmax <- as.POSIXct("2001-04-30 23:59:59", tz = "America/Detroit")

# Colors (Okabe–Ito)
pal <- c(
  "Mead"             = "#F0E442",  # yellow
  "Henwood"          = "#D55E00",  # vermillion (distinct from orange)
  "Tippy Dam"        = "#009E73",  # green
  "Aztec Mine"       = "#CC79A7",  # reddish-purple
  "Beatens Cave"     = "#E69F00",  # orange
  "South Lake Upper" = "#56B4E9",  # light blue
  "South Lake Lower" = "#1B6DAE",   # dark blue
  "Delaware"         = "#000000",   # black
  "Belt Mine"        = "#999999"    # grey
)

# Linetypes: solid for non–South Lake; vary within the South Lake pair
lt <- c(
  "Mead"             = "solid",
  "Henwood"          = "solid",
  "Tippy Dam"        = "solid",
  "Aztec Mine"       = "solid",
  "Beatens Cave"     = "solid",
  "South Lake Upper" = "dashed",
  "South Lake Lower" = "dashed",
  "Delaware"         = "solid",
  "Belt Mine"        = "solid"
)

# (Optional) sanity check: every source has a color
setdiff(unique(all_sites$source), names(pal))
# should return character(0)

library(grid)  # for unit()

# unify scales + legend inside p_all
p_all <- ggplot() +
  annotate("rect", xmin = winter_xmin, xmax = winter_xmax,
           ymin = -Inf, ymax = Inf, fill = "grey96", alpha = 1) +
  geom_line(data = all_sites,
            aes(x = season_time, y = temp, color = source, linetype = source),
            linewidth = 1.1, alpha = 0.9) +
  scale_color_manual(
    values = pal,
    breaks = names(pal),
    name   = "Site",
    guide  = guide_legend(override.aes = list(linewidth = 1.6))
  ) +
  scale_linetype_manual(
    values = lt,
    breaks = names(pal),  # match color scale ordering
    name   = "Site"
  ) +
  scale_x_datetime(breaks = month_breaks, labels = scales::label_date("%b"),
                   limits = c(season_start, season_end), expand = c(0.005, 0)) +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    title = "Temperatures Aligned on a Common Sept→Aug Timeline",
    subtitle = "Shaded band indicates Nov–Apr (winter period)",
    caption = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.key.width = unit(2.2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey80"),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 4)),
    plot.margin = margin(10, 16, 10, 16)
  )

# Save THIS object (no extra scales added afterward)
ggsave("figures/Mines_seasonal_Timeline.png",
       p_all, width = 11, height = 6.5, dpi = 300, bg = "white")

# ggsave("All_Sites_Season_Timeline.png", p_all, width = 11, height = 6.5, dpi = 300, bg = "white")
# ggsave("All_Sites_Season_Timeline.svg", p_all, width = 11, height = 6.5, bg = "white")

mobility <- read_excel(
  "data/Joe&Kate_MI_data_multiple_papers.xlsx",
  sheet = "mobility temp all sites"
) %>% 
  mutate(overwinter.diff = as.numeric(overwinter.diff), 
         overwinter.lambda = as.numeric(overwinter.lambda))

colnames(mobility)

# scatterplot of mean temp (x) vs overwinter.difff (y)
# different colors/lines per species
# add a total line
library(ggplot2)
library(dplyr)

# Scatter + regression line per species
mobility %>% filter(species != "EPFU") %>% 
ggplot(aes(x = mean_temp, y = overwinter.diff, color = species)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Mean Winter Temperature (°C)",
    y = "Overwinter Difference (Late − Early)",
    color = "Species",
    title = "Bat Movement vs. Mean Winter Temperature"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  ) + coord_cartesian(xlim = c(3, 11), ylim = c(-100, 100))

slopes <- mobility %>%
  filter(species != "EPFU") %>%
  group_by(species) %>%
  do(tidy(lm(overwinter.diff ~ mean_temp, data = .))) %>%
  filter(term == "mean_temp") %>%
  select(species, estimate, std.error, statistic, p.value)

print(slopes)


# Scatter + regression line per species + pooled line
ggplot(mobility, aes(x = mean_temp, y = overwinter.diff, color = species)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +                # species-specific lines
  geom_smooth(aes(x = mean_temp, y = overwinter.diff),   # pooled line
              method = "lm", color = "black", se = TRUE, inherit.aes = FALSE) +
  labs(
    x = "Mean Winter Temperature (°C)",
    y = "Overwinter Difference (Late − Early)",
    color = "Species",
    title = "Bat Movement vs. Mean Winter Temperature"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape wide → long so early & late counts can be plotted together
mobility_long <- mobility %>%
  mutate(
    early.winter.count = as.numeric(early.winter.count),
    count = as.numeric(count)
  ) %>%
  select(site, mean_temp, early.winter.count, count, species, wyear) %>%
  pivot_longer(
    cols = c(early.winter.count, count),
    names_to = "season",
    values_to = "bat_count"
  ) %>%
  mutate(
    season = recode(season,
                    "early.winter.count" = "Early Winter",
                    "count" = "Late Winter")
  )

# Plot: lines show change early→late per site
mobility_long %>% filter(species == "MYLU") %>% filter(wyear == "2014") %>%  
ggplot(aes(x = mean_temp, y = bat_count, color = season, group = interaction(site, species))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ species) +
  labs(
    x = "Mean Site Temperature (°C)",
    y = "Bat Count",
    color = "Survey",
    title = "Early vs Late Winter Bat Counts per Site"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

colnames(mobility)
