# Boxplot Script for Affected Area (kmÂ²) by Time Slice
# Intense precipitation indicator: 50 mm

# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(terra)
library(dplyr)
library(ggplot2)
library(grid)

# Load thesis theme
source("/home/WUR/sethi002/Scripts/isimip3b/final/thesis_theme.R")

# -----------------------------
# DIRECTORY
# -----------------------------
input_dir <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/Affected_Maps_50mm/"

output_dir <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/50mm Box Plot/"

# Create folder if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

print(list.files(input_dir, recursive = TRUE)[1:20])

# -----------------------------
# CROP FUNCTIONAL TYPES
# -----------------------------
cfts <- c("temp_rf", "temp_ir", "wheat_rf", "wheat_ir")

# -----------------------------
# CLIMATE MODELS
# -----------------------------
models <- c("UKESM1-0-LL",
            "MRI-ESM2-0",
            "MPI-ESM1-2-HR",
            "IPSL-CM6A-LR",
            "GFDL-ESM4")

# -----------------------------
# SSPs
# -----------------------------
ssps <- c("historical", "ssp126", "ssp370", "ssp585")

# -----------------------------
# TIME PERIODS
# -----------------------------
periods <- list(
  Historical = 1985:2014,
  MidCentury = 2041:2070,
  EndCentury = 2071:2100
)

# ============================================================
# FUNCTION TO CALCULATE TOTAL AFFECTED AREA
# ============================================================

calculate_area <- function(rast_file) {
  r <- rast(rast_file)
  
  # Sum all raster values
  # Assuming raster values already represent kmÃ‚Â²
  total_area <- global(r, sum, na.rm = TRUE)[1, 1]
  
  return(total_area)
}

# ============================================================
# LOOP THROUGH FILES
# ============================================================

results <- data.frame()

for (model in models) {
  for (ssp in ssps) {
    # Historical only for historical years
    if (ssp == "historical") {
      years <- periods$Historical
      period_name <- "Historical"
    }
    
    # Future SSPs
    if (ssp != "historical") {
      # Mid-century
      for (yr in periods$MidCentury) {
        total_area_all_cfts <- 0
        
        for (cft in cfts) {
          pattern <- paste0(
            "^affected_area_50mm_event_map_",
            cft,
            "_",
            model,
            "_",
            ssp,
            "_",
            yr,
            "\\.tif$"
          )
          
          file <- list.files(
            input_dir,
            pattern = pattern,
            full.names = TRUE,
            recursive = TRUE
          )
          
          print(file)
          
          if (length(file) == 1) {
            area_val <- calculate_area(file)
            
            total_area_all_cfts <- total_area_all_cfts + area_val
          }
        }
        
        results <- rbind(
          results,
          data.frame(
            Model = model,
            SSP = ssp,
            Period = "Mid-century",
            Year = yr,
            Area_km2 = total_area_all_cfts
          )
        )
      }
      
      # End-century
      for (yr in periods$EndCentury) {
        total_area_all_cfts <- 0
        
        for (cft in cfts) {
          pattern <- paste0(
            "^affected_area_50mm_event_map_",
            cft,
            "_",
            model,
            "_",
            ssp,
            "_",
            yr,
            "\\.tif$"
          )
          
          file <- list.files(
            input_dir,
            pattern = pattern,
            full.names = TRUE,
            recursive = TRUE
          )
          
          if (length(file) == 1) {
            area_val <- calculate_area(file)
            
            total_area_all_cfts <- total_area_all_cfts + area_val
          }
        }
        
        results <- rbind(
          results,
          data.frame(
            Model = model,
            SSP = ssp,
            Period = "End-century",
            Year = yr,
            Area_km2 = total_area_all_cfts
          )
        )
      }
    }
    
    # Historical files
    if (ssp == "historical") {
      for (yr in years) {
        total_area_all_cfts <- 0
        
        for (cft in cfts) {
          pattern <- paste0(
            "^affected_area_50mm_event_map_",
            cft,
            "_",
            model,
            "_",
            ssp,
            "_",
            yr,
            "\\.tif$"
          )
          
          file <- list.files(
            input_dir,
            pattern = pattern,
            full.names = TRUE,
            recursive = TRUE
          )
          
          if (length(file) == 1) {
            area_val <- calculate_area(file)
            
            total_area_all_cfts <- total_area_all_cfts + area_val
          }
        }
        results <- rbind(
          results,
          data.frame(
            Model = model,
            SSP = ssp,
            Period = "Historical",
            Year = yr,
            Area_km2 = total_area_all_cfts
          )
        )
      }
    }
  }
}

# -----------------------------
# CHECK RESULTS
# -----------------------------
cat("Total rows in results:", nrow(results), "\n")

head(results)

# ============================================================
# ORDER FACTORS
# ============================================================

results$Period <- factor(results$Period,
                         levels = c("Historical", "Mid-century", "End-century"))

results$SSP <- factor(results$SSP,
                      levels = c("historical", "ssp126", "ssp370", "ssp585"))


# -----------------------------
# REMOVE UNUSED SSPS
# -----------------------------
results_plot <- results %>%
  filter(
    (Period == "Historical" & SSP == "historical") |
      (Period != "Historical" & SSP != "historical")
  )

# ============================================================
# CREATE FACETED BOXPLOTS
# ============================================================

p <- ggplot(
  results_plot,
  aes(
    x = SSP,
    y = Area_km2,
    fill = SSP
  )
) +
  
  geom_boxplot(
    width = 0.7,
    outlier.shape = 21,
    outlier.size = 2,
    outlier.stroke = 0.3,
    alpha = 0.95
  ) +
  
  # Facets
  facet_grid(
    ~ Period,
    scales = "free_x",
    space = "free_x"
  ) +
  
  # Colors
  scale_fill_manual(values = c(
    "historical" = "grey40",
    "ssp126"     = "#1b9e77",
    "ssp370"     = "#d95f02",
    "ssp585"     = "#7570b3"
  )) +
  
  # Full numbers instead of scientific notation
  scale_y_continuous(
    labels = scales::comma
  ) +
  
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

# Display plot
print(p)

# ============================================================
# SAVE FIGURE
# ============================================================

ggsave(
  filename = file.path(output_dir, "affected_area_boxplots_50mm.png"),
  plot = p,
  width = 14,
  height = 10,
  dpi = 300
)

# ============================================================
# SAVE DATA TABLE
# ============================================================

write.csv(results,
          file.path(output_dir, "affected_area_boxplot_data_50mm.csv"),
          row.names = FALSE)
