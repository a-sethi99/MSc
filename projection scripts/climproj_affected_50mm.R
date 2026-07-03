# ============================================================
# AFFECTED AREA MAPS (WHEAT CFT × PRECIP EVENTS)
# - Keeps crop rasters at native 5 arcmin resolution
# - Resamples precipitation event rasters to crop grid
# - Uses bilinear interpolation for precipitation fields
# - Creates affected area map
# ============================================================

library(terra)

# -----------------------------
# GET YEAR FROM SLURM
# -----------------------------
yr <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("[INFO] Processing year:", yr, "\n")

# -----------------------------
# BASE PATHS
# -----------------------------

base_dir  <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/Future_GS_50mm_events"
wheat_dir <- "/lustre/nobackup/WUR/ESG/sethi002/Wheat_Maps"
out_dir   <- "/lustre/nobackup/WUR/ESG/sethi002/Climate Projection Output/Affected_Maps_50mm"

# Create output directory if missing
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# -----------------------------
# FOLDERS (SSPs + timelines)
# -----------------------------

folders <- c(
  "ssp126 mid-century",
  "ssp126 end-century",
  "ssp370 mid-century",
  "ssp370 end-century",
  "ssp585 mid-century",
  "ssp585 end-century",
  "Historical"
)

# -----------------------------
# CROP MAPS (5 arcmin templates)
# -----------------------------

# These rasters define the TARGET GRID:
# - CRS
# - resolution
# - extent
# - alignment
#
# Precipitation rasters will be resampled
# TO these crop rasters.
# -----------------------------

crop_files <- list(
  temp_rf  = file.path(wheat_dir, "temperate_cereals_rainfed_area_km2.tif"),
  temp_ir  = file.path(wheat_dir, "temperate_cereals_irrigated_area_km2.tif"),
  wheat_rf = file.path(wheat_dir, "wheat2_rainfed_area_km2.tif"),
  wheat_ir = file.path(wheat_dir, "wheat2_irrigated_area_km2.tif")
)

# -----------------------------
# LOOP OVER FOLDERS
# -----------------------------

for (fld in folders) {
  
  cat("\n====================================================\n")
  cat("[INFO] Processing folder:", fld, "\n")
  cat("====================================================\n")
  
  fld_path <- file.path(base_dir, fld)
  
  # -----------------------------
  # SKIP INVALID YEAR/FOLDER COMBINATIONS
  # -----------------------------
  
  if (fld == "Historical" && yr > 2014) {
    cat("[SKIP] Historical folder does not contain year", yr, "\n")
    next
  }
  
  if (grepl("mid-century", fld) && (yr < 2041 || yr > 2070)) {
    cat("[SKIP] Mid-century folder does not contain year", yr, "\n")
    next
  }
  
  if (grepl("end-century", fld) && (yr < 2071 || yr > 2100)) {
    cat("[SKIP] End-century folder does not contain year", yr, "\n")
    next
  }
  
  # -----------------------------
  # LIST FILES FOR CURRENT YEAR
  # -----------------------------
  
  pattern <- paste0("_", yr, "\\.tif$")
  
  files_year <- list.files(
    fld_path,
    pattern = pattern,
    full.names = TRUE
  )
  
  cat("[DEBUG] Found", length(files_year), "files\n")
  
  if (length(files_year) == 0) {
    cat("[WARNING] No files found for year", yr, "in", fld, "\n")
    next
  }
  
  # -----------------------------
  # LOOP OVER FILES
  # -----------------------------
  
  for (f in files_year) {
    
    cat("\n----------------------------------------------------\n")
    cat("[STEP] Processing file:\n")
    cat(basename(f), "\n")
    cat("----------------------------------------------------\n")
    
    # -----------------------------
    # DETERMINE CROP FUNCTIONAL TYPE (CFT)
    # -----------------------------
    cft <- NULL
    
    if (grepl("temp_rf", f))  cft <- "temp_rf"
    if (grepl("temp_ir", f))  cft <- "temp_ir"
    if (grepl("wheat_rf", f)) cft <- "wheat_rf"
    if (grepl("wheat_ir", f)) cft <- "wheat_ir"
    
    if (is.null(cft)) {
      cat("[WARNING] Could not determine CFT for file:\n")
      cat(f, "\n")
      next
    }
    
    cat("[DEBUG] Identified CFT:", cft, "\n")
    
    # -----------------------------
    # LOAD DATA
    # -----------------------------
    cat("[STEP] Loading rasters...\n")
    
    # Precipitation event raster
    pr <- rast(f)
    
    # Crop raster (acts as template)
    cr <- rast(crop_files[[cft]])
    
    # -----------------------------
    # PRINT GEOMETRY INFORMATION
    # -----------------------------
    cat("[DEBUG] Precip resolution:", res(pr), "\n")
    cat("[DEBUG] Crop resolution:", res(cr), "\n")
    
    # -----------------------------
    # RESAMPLE PRECIPITATION TO CROP GRID
    # -----------------------------
    if (!compareGeom(pr, cr, stopOnError = FALSE)) {
      
      cat("[FIX] Resampling precipitation raster to crop grid\n")
      cat("[FIX] Method: bilinear interpolation\n")
      
      pr <- resample(pr, cr, method = "bilinear")
    }
    
    # -----------------------------
    # VERIFY GEOMETRY MATCH
    # -----------------------------
    geom_check <- compareGeom(pr, cr, stopOnError = FALSE)
    
    if (!geom_check) {
      cat("[ERROR] Raster geometries still do not match!\n")
      next
    }
    
    cat("[DEBUG] Geometry alignment successful\n")
    
    # -----------------------------
    # CREATE EVENT BINARY MASK
    # -----------------------------
    cat("[STEP] Creating binary event mask...\n")
    
    event_binary <- ifel(pr >= 1 & cr > 0, 1, 0)
    
    # -----------------------------
    # CREATE AFFECTED AREA MAP
    # -----------------------------
    # Output units:
    # km² crop area affected by precipitation event
    # -----------------------------
    cat("[STEP] Creating affected area map...\n")
    
    affected_area <- event_binary * cr
    
    # -----------------------------
    # OPTIONAL SUMMARY STATISTICS
    # -----------------------------
    total_area <- global(affected_area, "sum", na.rm = TRUE)
    
    cat("[DEBUG] Total affected area (km²):",
        round(total_area[1,1], 2), "\n")
    
    # -----------------------------
    # BUILD OUTPUT NAMES
    # -----------------------------
    fname <- tools::file_path_sans_ext(basename(f))
    
    out_subdir <- file.path(out_dir, fld)
    
    if (!dir.exists(out_subdir)) {
      dir.create(out_subdir, recursive = TRUE)
    }
    
    # -----------------------------
    # OUTPUT FILES
    # -----------------------------
    out_binary <- file.path(
      out_subdir,
      paste0("affected_binary_50mm_", fname, ".tif")
    )
    
    out_area <- file.path(
      out_subdir,
      paste0("affected_area_50mm_", fname, ".tif")
    )
    
    # -----------------------------
    # SAVE OUTPUTS
    # -----------------------------
    cat("[SAVE] Writing binary affected map...\n")
    cat(out_binary, "\n")
    
    writeRaster(
      event_binary,
      out_binary,
      overwrite = TRUE
    )
    
    cat("[SAVE] Writing affected area map...\n")
    cat(out_area, "\n")
    
    writeRaster(
      affected_area,
      out_area,
      overwrite = TRUE
    )
    
    cat("[SUCCESS] Finished processing:\n")
    cat(fname, "\n")
    
  }
}

cat("\n====================================================\n")
cat("[SUCCESS] Finished year", yr, "\n")
cat("====================================================\n")
