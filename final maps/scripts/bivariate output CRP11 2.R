# ============================================================
# LOAD LIBRARIES
# ============================================================

library(terra)
library(sf)
library(tidyverse)
library(biscale)
library(cowplot)
library(rnaturalearth)

source("C:/Users/aashi/Desktop/thesis_theme.R")

# ============================================================
# INPUTS
# ============================================================

event_file <-
  "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/event_probability_CRP11_GS_Precip_1994_2024.tif"

crop_file <-
  "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/relative_crop_occupancy.tif"

# ============================================================
# LOAD RASTERS
# ============================================================

event_r <- rast(event_file)
crop_r <- rast(crop_file)

cat("\nResampling event raster to crop grid...\n")

event_r <- resample(
  event_r,
  crop_r,
  method = "bilinear"
)

cat("Event raster:\n")
print(event_r)

cat("\nCrop raster:\n")
print(crop_r)

compareGeom(
  event_r,
  crop_r
)

# ============================================================
# CHECK GEOMETRY
# ============================================================

if (!compareGeom(
  event_r,
  crop_r,
  stopOnError = FALSE
)) {
  
  stop(
    "Rasters do not have identical extent, resolution, or CRS."
  )
  
}

cat(
  "\nGeometry check passed.\n"
)

# ============================================================
# STACK RASTERS
# ============================================================

r_stack <- c(
  crop_r,
  event_r
)

names(r_stack) <- c(
  "crop",
  "event"
)

# ============================================================
# CONVERT TO DATA FRAME
# ============================================================

bi_df <- as.data.frame(
  r_stack,
  xy = TRUE,
  na.rm = TRUE
)

cat(
  "\nNumber of cells:",
  nrow(bi_df),
  "\n"
)

summary(bi_df)

# ============================================================
# CREATE BIVARIATE CLASSES
# (ONLY AGRICULTURAL CELLS)
# ============================================================

cat(
  "\nCells with crops:",
  sum(bi_df$crop > 0),
  "\n"
)

# Keep only cells where crops exist
bi_df_crop <- bi_df %>%
  filter(crop > 0)

cat(
  "Cells used for classification:",
  nrow(bi_df_crop),
  "\n"
)

# ============================================================
# CREATE FIXED EVENT CLASSES
# ============================================================

event_breaks <- c(
  0,
  33.3,
  66.7,
  100
)

bi_df_crop$event_class <- cut(
  bi_df_crop$event,
  breaks = event_breaks,
  include.lowest = TRUE,
  labels = c(
    "1",
    "2",
    "3"
  )
)

# ============================================================
# CREATE FIXED CROP CLASSES
# (same breaks as current quantiles)
# ============================================================

crop_breaks <- c(
  5.62e-06,
  0.001,
  0.0279,
  1
)

bi_df_crop$crop_class <- cut(
  bi_df_crop$crop,
  breaks = crop_breaks,
  include.lowest = TRUE,
  labels = c(
    "1",
    "2",
    "3"
  )
)

# ============================================================
# CREATE BIVARIATE CLASSES
# ============================================================

bi_df_crop$bi_class <- paste0(
  bi_df_crop$crop_class,
  "-",
  bi_df_crop$event_class
)

table(bi_df_crop$bi_class)

# ============================================================
# DIAGNOSTICS
# ============================================================

# Number of cells in each bivariate class
cat("\nBivariate classes:\n")
print(
  table(bi_df_crop$bi_class)
)

# Cross-tabulation of crop and event classes
cat("\nCrop class vs Event class:\n")
print(
  table(
    crop_class = sub("-.*", "", bi_df_crop$bi_class),
    event_class = sub(".*-", "", bi_df_crop$bi_class)
  )
)

cat("\nCrop breaks:\n")
print(crop_breaks)

cat("\nEvent breaks:\n")
print(event_breaks)

########

# Add empty class column to full dataframe
bi_df$bi_class <- NA_character_

# Put classes back only for agricultural cells
bi_df$bi_class[
  bi_df$crop > 0
] <- bi_df_crop$bi_class

table(
  bi_df$bi_class,
  useNA = "always"
)

# ============================================================
# CREATE BIVARIATE CLASS RASTER
# ============================================================

bi_df$class_id <- as.numeric(
  factor(
    bi_df$bi_class,
    levels = c(
      "1-1","2-1","3-1",
      "1-2","2-2","3-2",
      "1-3","2-3","3-3"
    )
  )
)

# ============================================================
# CONVERT DATA FRAME TO RASTER
# ============================================================

bi_raster <- rast(
  bi_df[, c("x", "y", "class_id")],
  type = "xyz",
  crs = crs(crop_r)
)

names(bi_raster) <- "bivariate_class"

# ============================================================
# SAVE GEOTIFF
# ============================================================

writeRaster(
  bi_raster,
  "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11_classes.tif",
  overwrite = TRUE
)

# ============================================================
# WORLD MAP
# ============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# ============================================================
# CREATE MAP
# ============================================================

map_bi <-
  
  ggplot() +
  
  geom_sf(
    data = world,
    fill = "grey92",
    colour = "grey70",
    linewidth = 0.15
  ) +
  
  geom_raster(
    data = bi_df,
    aes(
      x = x,
      y = y,
      fill = bi_class
    )
  ) +
  
  bi_scale_fill(
    pal = "BlueOr",
    dim = 3,
    na.value = "grey95",
    guide = "none"
  ) +
  
  coord_sf(
    xlim = c(
      ext(crop_r)[1],
      ext(crop_r)[2]
    ),
    ylim = c(
      ext(crop_r)[3],
      ext(crop_r)[4]
    ),
    expand = FALSE
  ) +
  
  labs(
    title =
      "Relative Crop Occupancy and Extreme Rainfall Probability",
    subtitle =
      "CRP11 precipitation indicator (1994–2024)"
  ) +
  
  theme_thesis()

# ============================================================
# CREATE LEGEND
# ============================================================

legend_bi <-
  
  bi_legend(
    pal = "BlueOr",
    dim = 3,
    xlab =
      "Higher relative crop occupancy →",
    ylab =
      "Higher event probability →",
    size = 10
  )

# ============================================================
# SAVE LEGEND ONLY
# ============================================================

ggsave(
  filename =
    "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11_Legend.tiff",
  plot = legend_bi,
  width = 2.5,
  height = 2.5,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename =
    "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11_Legend.png",
  plot = legend_bi,
  width = 2.5,
  height = 2.5,
  dpi = 600,
  bg = "white"
)

# ============================================================
# COMBINE MAP + LEGEND
# ============================================================

final_plot <-
  
  ggdraw() +
  
  draw_plot(
    map_bi,
    0,
    0,
    1,
    1
  ) +
  
  draw_plot(
    legend_bi,
    0.72,
    0.12,
    0.20,
    0.20
  )

# ============================================================
# SAVE PNG
# ============================================================

ggsave(
  filename =
    "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11.png",
  plot = final_plot,
  width = 8,
  height = 6,
  dpi = 600
)

cat(
  "\nBivariate map saved successfully.\n"
)

# ============================================================
# SAVE MAP ONLY
# ============================================================

ggsave(
  filename =
    "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11_MapOnly.tiff",
  plot = map_bi,
  width = 8,
  height = 6,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename =
    "C:/Users/aashi/Desktop/Final Thesis Report/final maps/Bivariate maps/CRP11_2/Bivariate_CRP11_MapOnly.png",
  plot = map_bi,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white"
)