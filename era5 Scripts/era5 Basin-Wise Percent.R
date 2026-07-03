library(readr)
library(ggplot2)
library(dplyr)

# Windows Arial font
windowsFonts(
  Arial = windowsFont("Arial")
)

# Load thesis theme
source("C:/Users/aashi/Desktop/thesis_theme.R")
theme_set(theme_thesis())

# Read CSV file
basin_df <- read_csv(
  "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/era5_basin_output/30mm/basin_percent_cropland_affected_30mm_1994_2024.csv"
)

# Remove Tibetan Plateau
basin_df <- basin_df %>%
  filter(Basin != "Tibetan Plateau")

# Remove NA values
basin_df <- basin_df %>%
  filter(!is.na(`% Affected`))

# Create basin color palette
basin_cols <- c(
  "Syr Darya" = "#1B9E77",
  "Amu Darya" = "#D95F02",
  "Balkhash" = "#7570B3",
  "Brahmaputra" = "#E7298A",
  "Bay of Bengal" = "#66A61E",
  "Ganges" = "#E6AB02",
  "Hamun" = "#A6761D",
  "Indus" = "#666666",
  "Irrawaddy" = "#1F78B4",
  "Meghna" = "#B2DF8A",
  "Mekong" = "#FB9A99",
  "Murgab, Tedzen and Atrek" = "#CAB2D6",
  "Salween" = "#FDBF6F",
  "Tarim" = "#FF7F00",
  "Yangtze" = "#6A3D9A",
  "Yellow" = "#B15928",
  "Chu, Talas, Asse and Issyk-Kul" = "#A6CEE3",
  "East China Sea" = "#33A02C"
)

# Create faceted basin plot
p_basin <- ggplot(
  basin_df,
  aes(
    x = Year,
    y = `% Affected`,
    group = Basin
  )
) +
  
  geom_line(
    linewidth = 0.6,
    color = "black"
  ) +
  
  facet_wrap(
    ~ Basin,
    ncol = 5,
    scales = "fixed"
  ) +
  
  scale_x_continuous(
    breaks = seq(1995, 2025, by = 10),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0, 80),
    expand = c(0, 0)
  ) +
  
  labs(
    title = expression(
      bold(
        "Percentage of Basin Affected by Indicator 1"
      )
    ),
    

    x = "Year",
    
    y = expression(
      bold(
        "% Area Affected"
      )
    )

    
  ) +
  
  theme(
    legend.position = "none",
    

    strip.text = element_text(
      face = "bold",
      size = 9,
      family = "Arial"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    
    axis.text.y = element_text(
      size = 8
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )

# Print plot
print(p_basin)

# Save figure
ggsave(
  "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/Graphs/era5/basin_percent_affected_ind1.png",
  plot = p_basin,
  width = 10,
  height = 7,
  dpi = 600
)
