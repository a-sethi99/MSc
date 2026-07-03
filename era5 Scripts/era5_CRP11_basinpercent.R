# Calculate % Cropland Affected Per Basin (1994–2024)
# 50 mm / 24 hour precipitation indicator

library(terra)
library(dplyr)

# Basin polygons
basins <- vect("C:/Users/aashi/Desktop/THESIS RUN/thirdpole_indiv_shp_hs_wmo.gpkg")

# Folder with yearly affected-area rasters
affected_dir <- "C:/Users/aashi/Desktop/THESIS RUN/1994-2024 Historical Analysis/CRP11_Affected_Maps"

# Output folder
output_dir <- "C:/Users/aashi/Desktop/THESIS FIRST DRAFT/era5_basin_output/CRP11"

# Create output folder if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------
# TOTAL CROPLAND AREA RASTERS
# -----------------------------------------------------

# Temperate cereals irrigated
temp_ir_total <- rast(
  "C:/Users/aashi/Desktop/THESIS RUN/Wheat_Maps/temperate_cereals_irrigated_area_km2.tif"
)

# Temperate cereals rainfed
temp_rf_total <- rast(
  "C:/Users/aashi/Desktop/THESIS RUN/Wheat_Maps/temperate_cereals_rainfed_area_km2.tif"
)

# Wheat irrigated
wheat_ir_total <- rast(
  "C:/Users/aashi/Desktop/THESIS RUN/Wheat_Maps/wheat2_irrigated_area_km2.tif"
)

# Wheat rainfed
wheat_rf_total <- rast(
  "C:/Users/aashi/Desktop/THESIS RUN/Wheat_Maps/wheat2_rainfed_area_km2.tif"
)

# -----------------------------------------------------
# DEBUGGING CHECKS
# -----------------------------------------------------

cat("Checking CRS consistency...\n")
print(crs(basins))
print(crs(temp_ir_total))

# Check raster geometry matches
compareGeom(temp_ir_total, temp_rf_total)
compareGeom(temp_ir_total, wheat_ir_total)
compareGeom(temp_ir_total, wheat_rf_total)

# -----------------------------------------------------
# CREATE TOTAL CROPLAND RASTER
# -----------------------------------------------------

cat("Creating total cropland raster...\n")

# Sum all 4 crop area rasters
crop_total <- temp_ir_total +
  temp_rf_total +
  wheat_ir_total +
  wheat_rf_total

# -----------------------------------------------------
# CALCULATE TOTAL CROPLAND AREA PER BASIN
# -----------------------------------------------------

cat("Calculating total cropland area per basin...\n")

# Extract summed crop area by basin
basin_total_crop <- extract(
  crop_total,
  basins,
  fun = sum,
  na.rm = TRUE
)

# Add basin names
basin_total_crop$basinname <- basins$basinname

# Rename column
colnames(basin_total_crop)[2] <- "total_crop_area_km2"

# -----------------------------------------------------
# YEARS
# -----------------------------------------------------

years <- 1994:2024

# Empty list to store yearly outputs
all_results <- list()

# -----------------------------------------------------
# LOOP THROUGH YEARS
# -----------------------------------------------------

for (yr in years) {
  
  cat("-------------------------------------\n")
  cat("Processing year:", yr, "\n")
  
  # ---------------------------------------------------
  # Load yearly affected rasters
  # ---------------------------------------------------
  
  temp_ir_file <- file.path(
    affected_dir,
    paste0("affected_area_temp_ir_", yr, ".tif")
  )
  
  temp_rf_file <- file.path(
    affected_dir,
    paste0("affected_area_temp_rf_", yr, ".tif")
  )
  
  wheat_ir_file <- file.path(
    affected_dir,
    paste0("affected_area_wheat_ir_", yr, ".tif")
  )
  
  wheat_rf_file <- file.path(
    affected_dir,
    paste0("affected_area_wheat_rf_", yr, ".tif")
  )
  
  # ---------------------------------------------------
  # Check files exist
  # ---------------------------------------------------
  
  if (!file.exists(temp_ir_file)) {
    cat("Missing file:", temp_ir_file, "\n")
    next
  }
  
  if (!file.exists(temp_rf_file)) {
    cat("Missing file:", temp_rf_file, "\n")
    next
  }
  
  if (!file.exists(wheat_ir_file)) {
    cat("Missing file:", wheat_ir_file, "\n")
    next
  }
  
  if (!file.exists(wheat_rf_file)) {
    cat("Missing file:", wheat_rf_file, "\n")
    next
  }
  
  # ---------------------------------------------------
  # Load rasters
  # ---------------------------------------------------
  
  temp_ir_aff <- rast(temp_ir_file)
  temp_rf_aff <- rast(temp_rf_file)
  wheat_ir_aff <- rast(wheat_ir_file)
  wheat_rf_aff <- rast(wheat_rf_file)
  
  # ---------------------------------------------------
  # Debug geometry consistency
  # ---------------------------------------------------
  
  compareGeom(temp_ir_aff, temp_rf_aff)
  compareGeom(temp_ir_aff, wheat_ir_aff)
  compareGeom(temp_ir_aff, wheat_rf_aff)
  
  # ---------------------------------------------------
  # Sum all affected-area rasters
  # ---------------------------------------------------
  
  affected_total <- temp_ir_aff +
    temp_rf_aff +
    wheat_ir_aff +
    wheat_rf_aff
  
  # ---------------------------------------------------
  # Extract affected area per basin
  # ---------------------------------------------------
  
  basin_affected <- extract(
    affected_total,
    basins,
    fun = sum,
    na.rm = TRUE
  )
  
  # Add basin names
  basin_affected$basinname <- basins$basinname
  
  # Rename column
  colnames(basin_affected)[2] <- "affected_area_km2"
  
  # ---------------------------------------------------
  # Merge with total crop area
  # ---------------------------------------------------
  
  result <- basin_affected %>%
    left_join(basin_total_crop, by = "basinname")
  
  # ---------------------------------------------------
  # Calculate percentage affected
  # ---------------------------------------------------
  
  result <- result %>%
    mutate(
      year = yr,
      
      percent_affected = ifelse(
        total_crop_area_km2 > 0,
        (affected_area_km2 / total_crop_area_km2) * 100,
        NA
      )
    )
  
  # ---------------------------------------------------
  # Keep useful columns only
  # ---------------------------------------------------
  
  result <- result %>%
    select(
      basinname,
      year,
      affected_area_km2,
      total_crop_area_km2,
      percent_affected
    )
  
  # ---------------------------------------------------
  # Save yearly result in list
  # ---------------------------------------------------
  
  all_results[[as.character(yr)]] <- result
  
  cat("Finished year:", yr, "\n")
}

# -----------------------------------------------------
# COMBINE ALL YEARS
# -----------------------------------------------------

final_results <- bind_rows(all_results)

# -----------------------------------------------------
# SAVE CSV
# -----------------------------------------------------

output_csv <- file.path(
  output_dir,
  "basin_percent_cropland_affected_crp11_1994_2024.csv"
)

write.csv(final_results, output_csv, row.names = FALSE)

cat("=====================================\n")
cat("Finished processing all years!\n")
cat("CSV saved to:\n")
cat(output_csv, "\n")
cat("=====================================\n")