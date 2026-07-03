# ============================================================
# TIME SERIES: TOTAL AFFECTED AREA (PER CFT + AGGREGATED)
# Input: affected_area_*_YYYY.tif
# Output: CSV + plots
# ============================================================

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# PATHS
# -----------------------------
data_dir <- "/lustre/nobackup/WUR/ESG/sethi002/30mm_Affected_Maps"
out_dir  <- "/lustre/nobackup/WUR/ESG/sethi002/30mm_Results"
dir.create(out_dir, showWarnings = FALSE)

# -----------------------------
# SETTINGS
# -----------------------------
years <- 1994:2025
cfts <- c("temp_rf","temp_ir","wheat_rf","wheat_ir")

# -----------------------------
# FUNCTION TO CALCULATE TOTAL AREA
# -----------------------------
calc_total_area <- function(file_path) {
  r <- rast(file_path)
  val <- global(r, "sum", na.rm = TRUE)[1,1]
  return(val)
}

# -----------------------------
# BUILD DATAFRAME
# -----------------------------
results <- data.frame()

for (yr in years) {
  cat("Processing year:", yr, "\n")
  
  row <- data.frame(year = yr)
  
  for (cft in cfts) {
    f <- file.path(data_dir, paste0("affected_area_", cft, "_", yr, ".tif"))
    
    if (file.exists(f)) {
      row[[cft]] <- calc_total_area(f)
    } else {
      row[[cft]] <- NA
    }
  }
  
  # total across all CFTs
  row$total <- sum(row[cfts], na.rm = TRUE)
  
  results <- rbind(results, row)
}

# Save CSV
write.csv(results, file.path(out_dir, "affected_area_timeseries.csv"), row.names = FALSE)

# -----------------------------
# PLOT 1: Individual CFTs
# -----------------------------
results_long <- results %>%
  pivot_longer(cols = all_of(cfts), names_to = "cft", values_to = "area")

p1 <- ggplot(results_long, aes(x = year, y = area, color = cft)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Affected Wheat Area by CFT (1994–2025)",
       x = "Year", y = "Affected Area (km²)") +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "affected_area_by_cft.png"),
  plot = p1,
  width = 10,
  height = 6
)


# -----------------------------
# PLOT 2: Total affected area
# -----------------------------
p2 <- ggplot(results, aes(x = year, y = total)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point() +
  labs(title = "Total Affected Wheat Area (All CFTs)",
       x = "Year", y = "Total Affected Area (km²)") +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "total_affected_area.png"),
  plot = p2,
  width = 10,
  height = 6
)

# ============================================================
# OUTPUTS
# - affected_area_timeseries.csv
# - affected_area_by_cft.png
# - total_affected_area.png
# ============================================================
