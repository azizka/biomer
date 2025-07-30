#' Plot number of occurrence records per raster cell
#'
#' This function visualizes the absolute number of species occurrence records per raster cell
#' over a biome raster layer. It uses precomputed biome polygons (.gpkg files) for fast plotting.
#'
#' @param data A data frame with occurrence records (must include lon/lat columns).
#' @param lon_col Column name for longitude. Default: "decimalLongitude".
#' @param lat_col Column name for latitude. Default: "decimalLatitude".
#' @param biome_layer Integer or vector. Index(es) of the biome raster layer(s).
#' @param raster_path Path to the biome raster stack. Default: internal package path.
#' @param save_path Optional folder path to save plots. PNGs will be auto-named.
#' @param show Logical. Show plots in the R viewer? Default: TRUE.
#'
#' @import terra
#' @import viridis
#' @export
biomer_plot <- function(data,
                        lon_col = "decimalLongitude",
                        lat_col = "decimalLatitude",
                        biome_layer = 1,
                        raster_path = NULL,
                        save_path = NULL,
                        show = TRUE) {

  if (is.null(raster_path)) {
    raster_path <- system.file("extdata", "Biome_Inventory_RasterStack.tif", package = "biomer")
  }

  r_stack <- terra::rast(raster_path)

  if (!is.null(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  }

  for (layer in biome_layer) {
    r_biome <- r_stack[[layer]]

    polygon_path <- file.path(system.file("extdata/polygons", package = "biomer"),
                              paste0("layer_", layer, ".gpkg"))
    if (!file.exists(polygon_path)) {
      warning("Polygon file not found for layer ", layer)
      next
    }

    biome_polys <- terra::vect(polygon_path)

    # Punktdaten vorbereiten
    points_wgs <- terra::vect(data, geom = c(lon_col, lat_col), crs = "EPSG:4326")
    points_proj <- terra::project(points_wgs, terra::crs(r_biome))

    # Zellenzuweisung und Zählung
    cell_ids <- terra::cellFromXY(r_biome, terra::crds(points_proj))
    valid <- !is.na(cell_ids)
    cell_ids <- cell_ids[valid]
    cell_counts <- table(cell_ids)

    # Raster mit Zählungen
    r_count <- r_biome
    values(r_count) <- 0
    values(r_count)[as.integer(names(cell_counts))] <- as.integer(cell_counts)
    r_count[r_count == 0] <- NA

    # Raster zu Polygonen
    polys_counts <- terra::as.polygons(r_count, dissolve = FALSE)
    names(polys_counts) <- "count"

    # Farben
    biome_ids <- sort(unique(biome_polys$biome_id))
    biome_cols <- viridis(length(biome_ids))
    biome_col_map <- setNames(biome_cols, biome_ids)
    biome_poly_colors <- biome_col_map[as.character(biome_polys$biome_id)]

    reds <- colorRampPalette(c("#FFCCCC", "#CC0000", "#330000"))(100)
    col_vals <- cut(log1p(polys_counts$count), breaks = 100, labels = FALSE)
    col_map <- reds[col_vals]

    # PNG export
    plot_file <- if (!is.null(save_path)) {
      file.path(save_path, paste0("layer_", layer, ".png"))
    } else {
      NULL
    }

    if (!is.null(plot_file)) {
      png(filename = plot_file, width = 12000, height = 8000, res = 1200)
    }

    if (show || !is.null(plot_file)) {
      par(mar = c(0, 0, 4, 0))
      plot(biome_polys, col = biome_poly_colors, border = NA,
           main = paste0("Biome Map – Absolute Occurrence Counts – Layer ", layer),
           axes = FALSE)
      plot(polys_counts, col = col_map, border = NA, add = TRUE)
    }

    if (!is.null(plot_file)) {
      dev.off()
      message("Saved plot: ", plot_file)
    }
  }

  invisible(NULL)
}
