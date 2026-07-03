# ============================================================
# LOAD LIBRARIES
# ============================================================

library(terra)

# ============================================================
# INPUTS
# ============================================================

event_dir <-
  "/lustre/nobackup/WUR/ESG/sethi002/1994-2024 Historical Analysis/50mm/50mm_GS_Precip"

# ============================================================
# OUTPUT DIRECTORY
# ============================================================

output_dir <-
  "/lustre/nobackup/WUR/ESG/sethi002/Thesis_Final_June"

if (!dir.exists(output_dir)) {
  stop(
    "Output directory does not exist:\n",
    output_dir
  )
}

years <- 1994:2024
n_years <- length(years)

cat("Number of years:", n_years, "\n")

# ============================================================
# CREATE LIST TO STORE YEARLY BINARY MAPS
# ============================================================

yearly_binary <- vector(
  "list",
  n_years
)

# ============================================================
# LOOP THROUGH YEARS
# ============================================================

for (i in seq_along(years)) {
  
  yr <- years[i]
  
  cat(
    "\n=============================\n",
    "Processing year:", yr, "\n"
  )
  
  # ----------------------------------------------------------
  # FILE PATHS
  # ----------------------------------------------------------
  
  temp_ir_file <-
    file.path(
      event_dir,
      paste0(
        "event_map_temp_ir_",
        yr,
        ".tif"
      )
    )
  
  temp_rf_file <-
    file.path(
      event_dir,
      paste0(
        "event_map_temp_rf_",
        yr,
        ".tif"
      )
    )
  
  wheat_ir_file <-
    file.path(
      event_dir,
      paste0(
        "event_map_wheat_ir_",
        yr,
        ".tif"
      )
    )
  
  wheat_rf_file <-
    file.path(
      event_dir,
      paste0(
        "event_map_wheat_rf_",
        yr,
        ".tif"
      )
    )
  
  # ----------------------------------------------------------
  # CHECK FILES EXIST
  # ----------------------------------------------------------
  
  files <- c(
    temp_ir_file,
    temp_rf_file,
    wheat_ir_file,
    wheat_rf_file
  )
  
  missing_files <- files[!file.exists(files)]
  
  if (length(missing_files) > 0) {
    
    stop(
      "Missing files:\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
    
  }
  
  # ----------------------------------------------------------
  # LOAD RASTERS
  # ----------------------------------------------------------
  
  temp_ir <- rast(temp_ir_file)
  temp_rf <- rast(temp_rf_file)
  wheat_ir <- rast(wheat_ir_file)
  wheat_rf <- rast(wheat_rf_file)
  
  # ----------------------------------------------------------
  # DEBUG
  # ----------------------------------------------------------
  
  cat(
    "Max events:\n",
    "temp_ir  =", global(temp_ir, "max", na.rm = TRUE)[1,1], "\n",
    "temp_rf  =", global(temp_rf, "max", na.rm = TRUE)[1,1], "\n",
    "wheat_ir =", global(wheat_ir, "max", na.rm = TRUE)[1,1], "\n",
    "wheat_rf =", global(wheat_rf, "max", na.rm = TRUE)[1,1], "\n"
  )
  
  # ----------------------------------------------------------
  # CONVERT TO BINARY EVENT MAPS
  # ----------------------------------------------------------
  
  temp_ir_bin <- temp_ir > 0
  temp_rf_bin <- temp_rf > 0
  wheat_ir_bin <- wheat_ir > 0
  wheat_rf_bin <- wheat_rf > 0
  
  # ----------------------------------------------------------
  # COMBINE FOUR MAPS
  # EVENT IF ANY CROP HAS AN EVENT
  # ----------------------------------------------------------
  
  yearly_event <-
    (temp_ir_bin +
       temp_rf_bin +
       wheat_ir_bin +
       wheat_rf_bin) > 0
  
  names(yearly_event) <-
    paste0(
      "event_",
      yr
    )
  
  # ----------------------------------------------------------
  # DEBUG
  # ----------------------------------------------------------
  
  n_cells_event <-
    global(
      yearly_event,
      "sum",
      na.rm = TRUE
    )[1,1]
  
  cat(
    "Cells with event:",
    n_cells_event,
    "\n"
  )
  
  yearly_binary[[i]] <-
    yearly_event
}

# ============================================================
# STACK ALL YEARS
# ============================================================

cat(
  "\nStacking yearly binary rasters...\n"
)

event_stack <-
  rast(yearly_binary)

print(event_stack)

# ============================================================
# COUNT NUMBER OF YEARS WITH EVENTS
# ============================================================

cat(
  "\nCalculating number of event years...\n"
)

event_years <-
  app(
    event_stack,
    sum,
    na.rm = TRUE
  )

names(event_years) <-
  "event_years"

print(
  global(
    event_years,
    c(
      "min",
      "mean",
      "max"
    ),
    na.rm = TRUE
  )
)

# ============================================================
# CALCULATE PROBABILITY (%)
# ============================================================

event_probability <-
  (event_years / n_years) * 100

names(event_probability) <-
  "event_probability"

# ============================================================
# DEBUG
# ============================================================

cat(
  "\nProbability statistics:\n"
)

print(
  global(
    event_probability,
    c(
      "min",
      "mean",
      "max"
    ),
    na.rm = TRUE
  )
)

stopifnot(
  global(
    event_probability,
    "max",
    na.rm = TRUE
  )[1,1] <= 100.0001
)

cat(
  "\nProbability raster successfully created.\n"
)

# ============================================================
# SAVE TIFF
# ============================================================

output_file <-
  file.path(
    output_dir,
    "event_probability_50mm_GS_Precip_1994_2024.tif"
  )

writeRaster(
  event_probability,
  output_file,
  overwrite = TRUE
)

writeRaster(
  event_years,
  file.path(
    output_dir,
    "event_years_50mm_GS_Precip_1994_2024.tif"
  ),
  overwrite = TRUE
)

cat(
  "\nProbability raster written to:\n",
  output_file,
  "\n"
)