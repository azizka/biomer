library(terra)
library(dplyr)
library(viridis)

# Load raster stack and legend table
Biome_Inventory_RasterStack <- terra::rast("data/Biome_Inventory_RasterStack.tif")
Biome_Inventory_Legends <- read.delim("data/data_raw/Biome_Inventory_Legends.txt")

# Initialize empty list
preprocessed_layers <- list()

# Loop through each raster layer
for (i in 1:nlyr(Biome_Inventory_RasterStack)) {
  message(paste("Processing layer", i, "of", nlyr(Biome_Inventory_RasterStack)))

  input_ras <- Biome_Inventory_RasterStack[[i]]
  layer_name <- as.character(i)

  # Extract legend info
  full_legend_text <- Biome_Inventory_Legends[i, 1]
  source_info <- sub(".*based on", "based on", full_legend_text)
  source_info <- sub(":.*", "", source_info)
  input_legend_raw <- gsub(".*:", "", full_legend_text)
  input_legend_split <- strsplit(input_legend_raw, ";")[[1]]

  legend_df <- do.call(rbind, lapply(input_legend_split, function(x) {
    parts <- strsplit(trimws(x), ",")[[1]]
    if (length(parts) == 2) {
      data.frame(
        grid_value = as.integer(parts[1]),
        class_name = parts[2],
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))

  # Generate azonal masks
  ras_Inland_water    <- input_ras == 95
  ras_Oceanic_islands <- input_ras == 96
  ras_Mountains       <- input_ras == 97
  ras_Urban           <- input_ras == 98

  ras_Inland_water   [ras_Inland_water == 0] <- NA
  ras_Oceanic_islands[ras_Oceanic_islands == 0] <- NA
  ras_Mountains      [ras_Mountains == 0] <- NA
  ras_Urban          [ras_Urban == 0] <- NA

  azonal_masks <- list(
    inland_water = ras_Inland_water,
    oceanic_islands = ras_Oceanic_islands,
    mountains = ras_Mountains,
    urban = ras_Urban
  )

  # Remove azonal classes from input raster
  input_ras[input_ras >= 95] <- NA

  used_classes <- sort(unique(na.omit(values(input_ras))))
  legend_used <- legend_df %>% filter(grid_value %in% used_classes)

  colors_zonal <- rev(viridis::viridis(length(used_classes)))
  class_labels <- legend_used$class_name

  # Add azonal metadata (only labels & colors)
  azonal_labels <- c("Inland water", "Oceanic islands", "Mountains", "Urban")
  azonal_colors <- c("deepskyblue", "gray50", "brown", "darkorange")

  for (j in 1:4) {
    mask_ras <- azonal_masks[[j]]
    if (!is.nan(global(mask_ras, "min", na.rm = TRUE)[[1]])) {
      class_labels <- c(class_labels, azonal_labels[j])
    }
  }

  # Store all into list
  preprocessed_layers[[layer_name]] <- list(
    zonal_colors = colors_zonal,
    azonal_colors = azonal_colors,
    class_labels = class_labels,
    source_info = source_info,
    used_classes = used_classes,
    azonal_masks = azonal_masks
  )
}

# Save result
saveRDS(preprocessed_layers, file = "data/biome_raster_legend.rds")




# save polygons for mapping

library(terra)

# Lade Rasterstack
r_path <- "inst/extdata/Biome_Inventory_RasterStack.tif"
r_stack <- rast(r_path)

# Liste für alle Layer
biome_polygons_list <- list()

# Schleife: jeden Layer konvertieren
for (i in 1:nlyr(r_stack)) {
  r <- r_stack[[i]]
  cat("Converting layer", i, "\n")
  poly <- terra::as.polygons(r, dissolve = TRUE)
  names(poly) <- "biome_id"
  biome_polygons_list[[paste0("layer_", i)]] <- poly
}

# Speichern
save(biome_polygons_list, file = "inst/extdata/biome_polygons_prepared.rda")

r_stack <- terra::rast("inst/extdata/Biome_Inventory_RasterStack.tif")

for (i in 1:nlyr(r_stack)) {
  r <- r_stack[[i]]
  poly <- terra::as.polygons(r, dissolve = TRUE)
  names(poly) <- "biome_id"
  path <- file.path("inst/extdata/polygons", paste0("layer_", i, ".gpkg"))
  terra::writeVector(poly, path, overwrite = TRUE)
}

