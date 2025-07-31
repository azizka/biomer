#' Load Biome Raster and Legend into Environment
#'
#' Loads the raster stack and associated biome legend into the global environment for use in further analysis.
#'
#' @param env The environment into which the objects should be loaded. Defaults to `.GlobalEnv`.
#' @return NULL (objects are assigned in the environment)
#'
#' @examples
#' biomer_get()
#' head(biome_legend)
#'
#' @export

biomer_get <- function(env = .GlobalEnv) {
  path_legend <- system.file("extdata", "biome_legend.rds", package = "biomer")
  path_raster <- system.file("extdata", "Biome_Inventory_RasterStack.tif", package = "biomer")

  assign("biome_legend", readRDS(path_legend), envir = env)
  assign("biome_raster", terra::rast(path_raster), envir = env)

  message("Loaded: biome_legend, biome_raster")
  invisible(NULL)
}
