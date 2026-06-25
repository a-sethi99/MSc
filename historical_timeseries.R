# ============================================================
# HISTORICAL AFFECTED AREA TIMESERIES
# ------------------------------------------------------------
# Calculates yearly total affected area for each climate model
# by summing the 4 crop classes:
#   - temp_rf
#   - temp_ir
#   - wheat_rf
#   - wheat_ir
#
# Period: 1985–2014
#
# Outputs:
#   1. CSV file for each model
#   2. PNG timeseries graph for each model
#
# INPUT FOLDER:
# C:/Users/aashi/Desktop/THESIS FIRST DRAFT/Historical
#
# OUTPUT FOLDER:
# C:/Users/aashi/Desktop/THESIS FIRST DRAFT/Historical Timeseries
#
# THESIS THEME:
# C:/Users/aashi/Desktop/thesis_theme.R
# ============================================================

# -----------------------------
# LOAD LIBRARIES
# -----------------------------
library(terra)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

# -----------------------------
# LOAD THESIS THEME
# -----------------------------
source("C:/Users/aashi/Desktop/thesis_theme.R")

# -----------------------------
# INPUT / OUTPUT PATHS
# -----------------------------
input_dir  <- "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/Historical"
output_dir <- "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/Historical Timeseries"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# MODELS
# -----------------------------
models <- c(
  "UKESM1-0-LL",
  "MRI-ESM2-0",
  "MPI-ESM1-2-HR",
  "IPSL-CM6A-LR",
  "GFDL-ESM4"
)

# -----------------------------
# CROP CLASSES
# -----------------------------
crop_classes <- c(
  "temp_rf",
  "temp_ir",
  "wheat_rf",
  "wheat_ir"
)

# -----------------------------
# FUNCTION TO EXTRACT YEAR
# -----------------------------
extract_year <- function(filename) {
  
  year <- str_extract(filename, "(19|20)\\d{2}")
  
  return(as.numeric(year))
}

# ============================================================
# LOOP THROUGH EACH MODEL
# ============================================================

for(model_name in models){
  
  cat("\nProcessing:", model_name, "\n")
  
  # -----------------------------------------
  # FIND TIFF FILES FOR THIS MODEL
  # -----------------------------------------
  model_files <- list.files(
    input_dir,
    pattern = paste0(model_name, ".*\\.tif$"),
    full.names = TRUE
  )
  
  # keep only files containing crop class names
  model_files <- model_files[
    sapply(model_files, function(x)
      any(str_detect(x, crop_classes)))
  ]
  
  # -----------------------------------------
  # CREATE FILE TABLE
  # -----------------------------------------
  file_df <- data.frame(
    file = model_files,
    stringsAsFactors = FALSE
  )
  
  file_df$year <- extract_year(basename(file_df$file))
  
  # -----------------------------------------
  # CALCULATE TOTAL AREA PER YEAR
  # -----------------------------------------
  yearly_results <- data.frame()
  
  years <- sort(unique(file_df$year))
  
  for(yr in years){
    
    cat("  Year:", yr, "\n")
    
    year_files <- file_df$file[file_df$year == yr]
    
    total_area <- 0
    
    # -------------------------------------
    # SUM ALL 4 CROP CLASS TIFFS
    # -------------------------------------
    for(f in year_files){
      
      r <- rast(f)
      
      # Sum all pixel values
      # assuming raster values already represent area
      area_sum <- global(r, "sum", na.rm = TRUE)[1,1]
      
      total_area <- total_area + area_sum
    }
    
    # store result
    yearly_results <- rbind(
      yearly_results,
      data.frame(
        Model = model_name,
        Year = yr,
        Total_Affected_Area = total_area
      )
    )
  }
  
  # -----------------------------------------
  # SAVE CSV
  # -----------------------------------------
  csv_path <- file.path(
    output_dir,
    paste0(model_name, "_historical_timeseries.csv")
  )
  
  write_csv(yearly_results, csv_path)
  
  # -----------------------------------------
  # CREATE TIMESERIES PLOT
  # -----------------------------------------
  p <- ggplot(yearly_results,
              aes(x = Year,
                  y = Total_Affected_Area)) +
    
    geom_line(linewidth = 1) +
    
    geom_point(size = 2) +
    
    labs(
      title = model_name,
      x = "Year",
      y = "Total Affected Area"
    ) +
    
    theme_thesis()
  
  # -----------------------------------------
  # SAVE PLOT
  # -----------------------------------------
  ggsave(
    filename = file.path(
      output_dir,
      paste0(model_name, "_historical_timeseries.png")
    ),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  cat("Finished:", model_name, "\n")
}

# ============================================================
# OPTIONAL: COMBINED MULTI-MODEL PLOT
# ============================================================

# combine all CSVs
all_csvs <- list.files(
  output_dir,
  pattern = "_historical_timeseries\\.csv$",
  full.names = TRUE
)

combined_df <- bind_rows(
  lapply(all_csvs, read_csv, show_col_types = FALSE)
)

# create combined plot
combined_plot <- ggplot(
  combined_df,
  aes(x = Year,
      y = Total_Affected_Area,
      color = Model)
) +
  
  geom_line(linewidth = 1) +
  
  labs(
    title = "Historical Affected Area (1985–2014)",
    x = "Year",
    y = "Total Affected Area"
  ) +
  
  theme_thesis()

# save combined plot
ggsave(
  filename = file.path(
    output_dir,
    "ALL_MODELS_historical_timeseries.png"
  ),
  plot = combined_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("\nAll processing complete.\n")