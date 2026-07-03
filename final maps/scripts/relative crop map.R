# ============================================================
# LOAD LIBRARIES
# ============================================================

library(terra)
library(sf)
library(tidyverse)
library(tmap)

source("C:/Users/aashi/Desktop/thesis_theme.R")

# ============================================================
# INPUTS
# ============================================================

cft_file <- "C:/Users/aashi/Downloads/Crop Data/cftfrac_5min_42bands.nc"

basins_file <- "C:/Users/aashi/Desktop/THESIS RUN/thirdpole_indiv_shp_hs_wmo.gpkg"

# ============================================================
# LOAD DATA
# ============================================================

cat("\nLoading NetCDF...\n")
cft <- rast(cft_file)

cat("Number of layers:", nlyr(cft), "\n")
print(cft)

cat("\nLoading basin polygons...\n")
basins <- vect(basins_file)

cat("Number of basins:", nrow(basins), "\n")

# ============================================================
# NAME LAYERS
# ============================================================

cropnames21 <- c(
  "temperate_cereals","rice","maize","tropical cereals",
  "pulses","temperate roots","tropical roots","sunflower",
  "soybeans","groundnuts","rapeseed","sugarcane",
  "barley","cotton","wheat2","rice2","rice3",
  "others","grasses","biofuels1","biofuels2"
)

names(cft) <- c(
  paste0(cropnames21, "_rainfed"),
  paste0(cropnames21, "_irrigated")
)

cat("\nLayer names assigned.\n")

# ============================================================
# SELECT TARGET CROP FRACTIONS
# ============================================================

cft_sel <- cft[[
  c(
    "temperate_cereals_rainfed",
    "temperate_cereals_irrigated",
    "wheat2_rainfed",
    "wheat2_irrigated"
  )
]]

cat("\nSelected layers:\n")
print(names(cft_sel))

# ============================================================
# CHECK CRS
# ============================================================

cat("\nRaster CRS:\n")
print(crs(cft_sel))

cat("\nBasin CRS:\n")
print(crs(basins))

if (!same.crs(basins, cft_sel)) {
  
  cat("\nProjecting basins to raster CRS...\n")
  
  basins <- project(
    basins,
    crs(cft_sel)
  )
  
}

# ============================================================
# CLIP TO THIRD POLE
# ============================================================

cat("\nCropping and masking...\n")

cft_tp <- mask(
  crop(
    cft_sel,
    basins
  ),
  basins
)

cat("Dimensions after masking:\n")
print(dim(cft_tp))

# ============================================================
# DEBUG INDIVIDUAL CROP FRACTIONS
# ============================================================

cat("\nSummary of individual crop fractions:\n")

for (i in 1:nlyr(cft_tp)) {
  
  cat("\n---------------------------\n")
  cat(names(cft_tp)[i], "\n")
  
  print(
    global(
      cft_tp[[i]],
      c(
        "min",
        "mean",
        "max"
      ),
      na.rm = TRUE
    )
  )
}

# ============================================================
# SUM FOUR CROP FRACTIONS
# ============================================================

cat("\nCalculating cumulative crop occupancy...\n")

crop_presence <- app(
  cft_tp,
  sum,
  na.rm = TRUE
)

names(crop_presence) <- "crop_presence"

# ============================================================
# DEBUG SUMMED VALUES
# ============================================================

cat("\nSummed crop occupancy statistics:\n")

print(
  global(
    crop_presence,
    c(
      "min",
      "mean",
      "max"
    ),
    na.rm = TRUE
  )
)

cat(
  "\nCells exceeding 1:",
  global(
    crop_presence > 1,
    "sum",
    na.rm = TRUE
  )[1,1],
  "\n"
)

# ============================================================
# NORMALIZE BY MAXIMUM VALUE
# ============================================================

cat("\nNormalizing by maximum crop occupancy...\n")

max_occ <- global(
  crop_presence,
  "max",
  na.rm = TRUE
)[1,1]

cat("Maximum occupancy =", max_occ, "\n")

crop_presence_norm <-
  crop_presence / max_occ

names(crop_presence_norm) <-
  "relative_crop_occupancy"

# ============================================================
# DEBUG NORMALIZED VALUES
# ============================================================

cat("\nNormalized statistics:\n")

print(
  global(
    crop_presence_norm,
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
    crop_presence_norm,
    "max",
    na.rm = TRUE
  )[1,1] <= 1.000001
)

cat(
  "\nNormalization successful.\n"
)

# ============================================================
# SAVE TIFF
# ============================================================

writeRaster(
  crop_presence_norm,
  "C:/Users/aashi/Desktop/Final Thesis Report/crop map/relative_crop_occupancy.tif",
  overwrite = TRUE
)

cat(
  "\nTIFF written successfully.\n"
)