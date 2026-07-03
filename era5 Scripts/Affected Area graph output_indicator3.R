library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)

windowsFonts(
  Arial = windowsFont("Arial")
)

source("C:/Users/aashi/Desktop/thesis_theme.R")
theme_set(theme_thesis())

# Read Excel file
df <- read_excel(
  "C:/Users/aashi/Desktop/THESIS RUN/1994-2024 Historical Analysis/CRP11_Results/crp11_affected_area_timeseries.xlsx"
)

# Keep only needed columns
df <- df %>%
  select(
    Year,
    `S1 RF`,
    `S1 IR`,
    `S2 RF`,
    `S2 IR`,
    `Total Area`
  )

df_long <- df %>%
  pivot_longer(
    cols = c(`S1 RF`, `S1 IR`, `S2 RF`, `S2 IR`, `Total Area`),
    names_to = "Category",
    values_to = "Area"
  )

# Thesis color palette
thesis_cols <- c(
  "S1 RF" = "#D55E00",
  "S1 IR" = "#CC79A7",
  "S2 RF" = "#0072B2",
  "S2 IR" = "#009E73",
  "Total Area" = "#000000"
)

p <- ggplot(
  df_long,
  aes(
    x = Year,
    y = Area,
    color = Category,
    group = Category,
    linetype = Category
  )
) +
  
  geom_line(linewidth = 0.8) +
  
  geom_point(size = 0) +
  
  scale_color_manual(values = thesis_cols) +
  
  scale_linetype_manual(values = c(
    "S1 RF" = "solid",
    "S1 IR" = "solid",
    "S2 RF" = "solid",
    "S2 IR" = "solid",
    "Total Area" = "dashed"
  )) +
  
  scale_x_continuous(
    breaks = seq(1994, 2024, by = 5),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(
      bold(
        "Area (km"^2*") of Wheat Cropland affected by Indicator 3"
      )
    ),
    x = "Year",
    y = expression(
      bold(
        "Affected Area (km"^2*")"
      )
    )
  )

print(p)


# -----------------------------
# GRAPH 2 — Total Percentage
# -----------------------------

df <- read_excel(
  "C:/Users/aashi/Desktop/THESIS RUN/1994-2024 Historical Analysis/CRP11_Results/crp11_affected_area_timeseries.xlsx"
)

# Keep required columns
df_percent <- df_percent %>%
  select(
    Year,
    `Total %`
  )

# Create plot
p2 <- ggplot(
  df_percent,
  aes(
    x = Year,
    y = `Total %`
  )
) +
  
  geom_line(
    linewidth = 0.8,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = seq(1994, 2024, by = 5),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  
  
  labs(
    title = "Percentage of Wheat Cropland Affected by Indicator 3",
    x = "Year",
    y = "Affected Area (%)"
  )

print(p2)

# -----------------------------------
# GRAPH 3 — Seasonal Percentages
# -----------------------------------

df <- read_excel(
  "C:/Users/aashi/Desktop/THESIS RUN/1994-2024 Historical Analysis/CRP11_Results/crp11_affected_area_timeseries.xlsx"
)

# Keep required columns
df_pct <- df_pct %>%
  select(
    Year,
    `S1 RF %`,
    `S1 IR %`,
    `S2 RF %`,
    `S2 IR %`
  )

# Convert to long format
df_pct_long <- df_pct %>%
  pivot_longer(
    cols = c(`S1 RF %`, `S1 IR %`, `S2 RF %`, `S2 IR %`),
    names_to = "Category",
    values_to = "Percent"
  )

# Color palette
pct_cols <- c(
  "S1 RF %" = "#D55E00",
  "S1 IR %" = "#CC79A7",
  "S2 RF %" = "#0072B2",
  "S2 IR %" = "#009E73"
)

# Create plot
p3 <- ggplot(
  df_pct_long,
  aes(
    x = Year,
    y = Percent,
    color = Category,
    group = Category
  )
) +
  
  geom_line(linewidth = 0.8) +
  
  scale_color_manual(values = pct_cols) +
  
  scale_x_continuous(
    breaks = seq(1994, 2024, by = 5),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  
  
  labs(
    title = "Percentage of Wheat Cropland Affected by Indicator 3",
    x = "Year",
    y = "Affected Area (%)"
  )

print(p3)
