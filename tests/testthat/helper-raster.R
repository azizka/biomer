# Skip tests that need the ~36 MB biome raster when it cannot be made
# available: always on CRAN (no large downloads during checks) and
# whenever the download fails (e.g. offline). On success the raster is
# cached, so biomes_get() / biomes_classify() read it from disk.
skip_if_no_raster <- function() {
  testthat::skip_on_cran()
  path <- tryCatch(biomes_download(quiet = TRUE), error = function(e) NULL)
  if (is.null(path) || !file.exists(path)) {
    testthat::skip("biome raster could not be downloaded (offline?)")
  }
}
