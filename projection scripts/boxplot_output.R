# R Script: Faceted Boxplot (Historical → Mid-century → End-century)

# ============================================================
# LOAD LIBRARIES
# ============================================================
library(tidyverse)
library(ggplot2)
library(grid)

# ============================================================
# LOAD CUSTOM THESIS THEME
# ============================================================
source("C:/Users/aashi/Desktop/thesis_theme.R")

# ============================================================
# LOAD CSV FILE
# ============================================================

csv_file <- "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/affected_area_boxplot_data_30mm.csv"

# Read CSV
df <- read.csv(csv_file)

# ============================================================
# CHECK COLUMN NAMES
# ============================================================

print(names(df))

# ============================================================
# MODIFY COLUMN NAMES BELOW IF NEEDED
# ============================================================
# Replace these with your actual column names if different

# Numeric values column
value_col <- "Area_km2"

# Scenario column
scenario_col <- "SSP"

# Period column
period_col <- "Period"

# ============================================================
# RENAME COLUMNS TEMPORARILY
# ============================================================

df <- df %>%
  rename(
    affected_area = all_of(value_col),
    scenario      = all_of(scenario_col),
    period        = all_of(period_col)
  )

# ============================================================
# ORDER FACTORS
# ============================================================

# Period order
df$period <- factor(
  df$period,
  levels = c("Historical", "Mid-century", "End-century")
)

# Scenario order
df$scenario <- factor(
  df$scenario,
  levels = c("Historical", "ssp126", "ssp370", "ssp585")
)

# ============================================================
# CREATE FACETED BOXPLOT
# ============================================================

p <- ggplot(df,
            aes(x = scenario,
                y = affected_area,
                fill = scenario)) +
  
  geom_boxplot(
    width = 0.7,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.stroke = 0.3,
    alpha = 0.95
  ) +
  
  # Facets
  facet_grid(
    ~ period,
    scales = "free_x",
    space = "free_x"
  ) +
  
  # Manual colors
  scale_fill_manual(values = c(
    "Historical" = "grey40",
    "ssp126"     = "#1b9e77",
    "ssp370"     = "#d95f02",
    "ssp585"     = "#7570b3"
  )) +
  
  scale_y_continuous(labels = scales::comma) +
  
  labs(
    x = NULL,
    y = expression("Affected Area (" * km^2 * ")"),
    fill = "Scenario"
  ) +
  
  # Thesis theme
  theme_thesis() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.text.x = element_text(
      size = 11
    ),
    
    axis.title.y = element_text(
      size = 14
    ),
    
    legend.position = "none",
    
    panel.spacing.x = unit(1.2, "lines")
  )

# ============================================================
# DISPLAY PLOT
# ============================================================

print(p)

# ============================================================
# SAVE FIGURE
# ============================================================

 ggsave(
  filename = "affected_area_boxplot_30mm.png",
  plot = p,
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white"
)
