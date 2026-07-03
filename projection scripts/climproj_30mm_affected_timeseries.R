# ============================================================
# AFFECTED CROPLAND AREA TIME SERIES
# CLIMATE PROJECTION OUTPUTS (30mm)
# ============================================================
# PURPOSE:
# Calculate total affected cropland area (km²)
# from yearly affected-area raster outputs.

# OUTPUT:
# One CSV per scenario/timeline folder containing:
# - yearly affected area per crop type
# - total affected area across all crop types
# ============================================================

library(terra)
library(dplyr)

# ============================================================
# PATHS
# ============================================================

in_dir <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/Affected_Maps_30mm"

out_ts <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/30mm_Timeseries"

if (!dir.exists(out_ts)) {
  dir.create(out_ts, recursive = TRUE)
}

# ============================================================
# FOLDERS + YEAR RANGES
# ============================================================

folder_years <- list(
  "Historical" = 1985:2014,
  "ssp126 mid-century" = 2041:2070,
  "ssp370 mid-century" = 2041:2070,
  "ssp585 mid-century" = 2041:2070,
  "ssp126 end-century" = 2071:2100,
  "ssp370 end-century" = 2071:2100,
  "ssp585 end-century" = 2071:2100
)

folders <- names(folder_years)

# ============================================================
# SETTINGS
# ============================================================

# Crop functional types
cfts <- c(
  "temp_rf",
  "temp_ir",
  "wheat_rf",
  "wheat_ir"
)

# Climate models
models <- c(
  "UKESM1-0-LL",
  "MRI-ESM2-0",
  "MPI-ESM1-2-HR",
  "IPSL-CM6A-LR",
  "GFDL-ESM4"
)

# ============================================================
# FUNCTION: CALCULATE TOTAL AFFECTED AREA
# ============================================================
# Sums all raster cells (km² exposed)
# ============================================================

calc_total_area <- function(file_path) {
  
  r <- rast(file_path)
  
  total <- global(r, "sum", na.rm = TRUE)[1,1]
  
  return(total)
}

# ============================================================
# MAIN LOOP
# ============================================================

for (fld in folders) {
  
  cat("\n====================================================\n")
  cat("PROCESSING FOLDER:", fld, "\n")
  cat("====================================================\n")
  
  years <- folder_years[[fld]]
  
  cat("[INFO] Years:", min(years), "to", max(years), "\n")
  
  # ----------------------------------------------------------
  # Input/output paths
  # ----------------------------------------------------------
  
  fld_in <- file.path(in_dir, fld)
  fld_out <- file.path(out_ts, fld)
  
  if (!dir.exists(fld_out)) {
    dir.create(fld_out, recursive = TRUE)
  }
  
  # ============================================================
  # CREATE RESULTS DATAFRAME
  # ============================================================
  
  results <- data.frame()
  
  # ============================================================
  # LOOP OVER YEARS
  # ============================================================
  
  for (yr in years) {
    
    cat("\n[YEAR]", yr, "\n")
    
    # ----------------------------------------------------------
    # Create yearly row
    # ----------------------------------------------------------
    row <- data.frame(year = yr)
    
    # ============================================================
    # LOOP OVER CROP TYPES
    # ============================================================
    for (cft in cfts) {
      
      cat("  [CFT]", cft, "\n")
      
      model_areas <- c()
      
      # ============================================================
      # LOOP OVER CLIMATE MODELS
      # ============================================================
      for (model in models) {
        
        # ----------------------------------------------------------
        # FIND FILES
        # ----------------------------------------------------------
        # Example filename:
        # affected_area_event_map_wheat_rf_GFDL-ESM4_ssp370_2070.tif
        # ----------------------------------------------------------
        pattern <- paste0(
          "affected_area_30mm_event_map_.*",
          cft,
          ".*",
          model,
          ".*",
          yr,
          "\\.tif$"
        )
        
        files <- list.files(
          fld_in,
          pattern = pattern,
          full.names = TRUE,
          recursive = TRUE
        )
        
        # ----------------------------------------------------------
        # DEBUGGING
        # ----------------------------------------------------------
        cat("    [MODEL]", model, "\n")
        cat("    [DEBUG] Matching files:", length(files), "\n")
        
        # ----------------------------------------------------------
        # PROCESS FILE
        # ----------------------------------------------------------
        if (length(files) > 0) {
          
          # Use first matching file
          f <- files[1]
          
          cat("    [FILE]", basename(f), "\n")
          
          # Calculate total affected area
          area <- calc_total_area(f)
          
          cat("    [AREA]", round(area, 2), "km²\n")
          
          # Add to crop-type total
          model_areas <- c(model_areas, area)
          
        } else {
          
          cat("    [WARNING] No file found\n")
        }
      }
      
      # ----------------------------------------------------------
      # STORE CROP TOTAL
      # ----------------------------------------------------------
      row[[cft]] <- mean(model_areas, na.rm = TRUE)
      
      cat("  [MEAN", cft, "]",
          round(mean(model_areas, na.rm = TRUE), 2),
          "km²\n")
    }
    
    # ============================================================
    # CALCULATE GRAND TOTAL
    # ============================================================
    row$total_affected_area <- sum(row[cfts], na.rm = TRUE)
    
    cat("[TOTAL YEAR]",
        round(row$total_affected_area, 2),
        "km²\n")
    
    # Add row to results dataframe
    results <- rbind(results, row)
    
  }
  
  # ============================================================
  # SAVE CSV
  # ============================================================
  
  out_csv <- file.path(
    fld_out,
    paste0(
      gsub(" ", "_", fld),
      "_affected_area_timeseries.csv"
    )
  )
  
  cat("\n[SAVE] Writing CSV:\n")
  cat(out_csv, "\n")
  
  write.csv(
    results,
    out_csv,
    row.names = FALSE
  )
  
  cat("[SUCCESS] Finished folder:\n")
  cat(fld, "\n")
}

cat("\n====================================================\n")
cat("[SUCCESS] ALL FOLDERS PROCESSED\n")
cat("====================================================\n")
