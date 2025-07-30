#' Plot Biome Layers from Raster Stack
#'
#' Visualizes one or more biome layers from a raster stack using custom color scales and a legend.
#' Azonal categories (e.g., urban, water, mountains) are shown in separate colors.
#'
#' @param layer_name Integer or vector of integers specifying which layers to plot. If NULL, all layers are plotted.
#' @param raster_path Path to the biome raster stack (default: "data/Biome_Inventory_RasterStack.tif").
#' @param legend_path Path to the biome legend RDS file (default: "data/biome_legend.rds").
#' @param show Logical; whether to display the plot(s) interactively.
#' @param save Logical; whether to save the plot(s) as PNG file(s).
#' @param save_path Directory path to save plots. Required if `save = TRUE`.
#' @return NULL (plots are shown or saved as a side effect)
#' @export
#' @importFrom dplyr select starts_with
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @importFrom viridis viridis
#' @importFrom terra global values
#' @importFrom grDevices png dev.off
#' @importFrom graphics layout par title legend plot.new




biomer_show <- function(layer_name = NULL,
                        raster_path = NULL,
                        legend_path = NULL,
                        show = TRUE,
                        save = FALSE,
                        save_path = NULL) {

   # Set default paths from package if not provided
  if (is.null(raster_path)) {
    raster_path <- system.file("extdata", "Biome_Inventory_RasterStack.tif", package = "biomer")
  }
  if (is.null(legend_path)) {
    legend_path <- system.file("extdata", "biome_legend.rds", package = "biomer")
  }

  raster_stack <- terra::rast(raster_path)
  biome_legend <- readRDS(legend_path)

  # Determine which layers to plot
  if (is.null(layer_name)) {
    layer_name <- unique(biome_legend$layer)
  }

  if (save) {
    if (is.null(save_path)) stop("Please provide 'save_path' if save = TRUE.")
    if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
  }

  for (i in layer_name) {
    i <- as.integer(i)
    input_ras <- raster_stack[[i]]
    legend_row <- biome_legend[biome_legend$layer == i, ]


    # Extract class labels
    class_labels <- legend_row %>% select(starts_with("id_")) %>% unlist(use.names = FALSE)
    class_labels <- class_labels[!is.na(class_labels)]
    colors_zonal <- rev(viridis(length(class_labels)))

    # Azonal masks
    masks <- list(
      mountains = input_ras == 97,
      inland_water = input_ras == 95,
      oceanic_islands = input_ras == 96,
      urban = input_ras == 98
    )
    for (name in names(masks)) {
      m <- masks[[name]]
      m[m == 0] <- NA
      masks[[name]] <- m
    }

    # Remove azonal from raster
    input_ras[input_ras >= 95] <- NA
    azonal_colors <- c(mountains = "brown",
                       inland_water = "deepskyblue",
                       oceanic_islands = "gray50",
                       urban = "darkorange")

    colors_total <- colors_zonal
    labels_total <- class_labels

    for (j in names(masks)) {
      if (!is.nan(global(masks[[j]], "min", na.rm = TRUE)[[1]])) {
        labels_total <- c(labels_total, gsub("_", " ", j))
        colors_total <- c(colors_total, azonal_colors[[j]])
      }
    }

    # Plot
    if (save) {
      png(filename = file.path(save_path, paste0("map_layer_", i, ".png")),
          width = 12000, height = 8000, res = 1200)
    }

    if (show || save) {
      layout(matrix(c(1, 2), nrow = 2), heights = c(4, 1))
      par(mar = c(1, 1, 4, 1))

      terra::plot(input_ras,
                  col = colors_zonal,
                  axes = FALSE,
                  legend = FALSE)
      source_info <- legend_row$source[1]
      main_title <- paste0("Biome Map (Layer ", i,") - based on ", source_info)
      title(main = main_title, line = 2)

      for (j in names(masks)) {
        if (!is.null(masks[[j]]) && !all(is.na(values(masks[[j]])))) {
          par(new = TRUE)
          terra::plot(masks[[j]], col = azonal_colors[[j]], legend = FALSE, axes = FALSE)
        }
      }

      par(mar = c(0, 0, 0, 0))
      plot.new()
      legend("bottom",
             legend = labels_total,
             fill = colors_total,
             border = "black",
             cex = 0.55,
             bty = "n",
             ncol = 3,
             y.intersp = 0.8,
             x.intersp = 0.3,
             inset = c(0, 0.1))
    }

    if (save) {
      dev.off()
      message(sprintf("Layer %d saved to %s", i, file.path(save_path, paste0("map_layer_", i, ".png"))))
    }
  }

  # Reset plotting layout once at the end
  if (show) {
    layout(1)
  }
}
