#' Download the packaged biome raster stack
#'
#' The 31-layer biome raster stack (`Biomes_Inventory_RasterStack.tif`,
#' ~36 MB) is too large to ship inside the package on CRAN. It is hosted
#' as a release asset on GitHub instead. `biomes_download()` fetches it
#' once into a persistent, per-user cache directory (see
#' [tools::R_user_dir()]); every later call - and every internal use by
#' [biomes_get()] or [biomes_classify()] - reuses the cached copy, so the
#' download happens only the first time.
#'
#' @param overwrite Logical flag; if `TRUE`, re-download the raster even
#'   when a cached copy already exists. Defaults to `FALSE`.
#' @param quiet Logical flag; if `TRUE`, suppress the informational
#'   message and the download progress bar. Defaults to `FALSE`.
#'
#' @return The local file path to the cached raster, invisibly.
#'
#' @seealso [biomes_get()] to load the raster as a `terra::SpatRaster`.
#'
#' @examples
#' \donttest{
#' # Downloads ~36 MB on first run, then reuses the cached copy.
#' raster_path <- biomes_download()
#' raster_path
#' }
#'
#' @importFrom utils download.file
#' @export
biomes_download <- function(overwrite = FALSE, quiet = FALSE) {

  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(quiet)

  dest <- biomes_cache_path()
  if (file.exists(dest) && !overwrite) {
    return(invisible(dest))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (!quiet) {
    message(
      "Downloading biome raster stack (~36 MB) to:\n  ", dest,
      "\nThis happens only once; the file is cached for future use."
    )
  }

  # 36 MB can exceed the 60s default on a slow connection; be patient.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))

  # Download to a temporary ".part" file first, then rename, so an
  # interrupted download never leaves a truncated file in the cache.
  tmp <- paste0(dest, ".part")
  status <- tryCatch(
    utils::download.file(
      biomes_raster_url(),
      destfile = tmp,
      mode     = "wb",
      quiet    = quiet
    ),
    error = function(e) e
  )

  if (inherits(status, "error") || !file.exists(tmp)) {
    unlink(tmp)
    msg <- if (inherits(status, "error")) conditionMessage(status) else
      "download.file() did not produce a file"
    stop(
      "Failed to download the biome raster from:\n  ", biomes_raster_url(),
      "\nPlease check your internet connection and try again.\n",
      "Original error: ", msg,
      call. = FALSE
    )
  }

  file.rename(tmp, dest)
  invisible(dest)
}

# Cache directory used for downloaded package data.
#' @keywords internal
#' @noRd
biomes_cache_dir <- function() {
  tools::R_user_dir("biomes", which = "cache")
}

# Full path to the cached raster (the file may not exist yet).
#' @keywords internal
#' @noRd
biomes_cache_path <- function() {
  file.path(biomes_cache_dir(), "Biomes_Inventory_RasterStack.tif")
}

# Remote location of the raster (GitHub release asset). The "data-v1"
# tag is independent of the package version so the asset only needs to
# be re-uploaded when the raster data itself changes.
#' @keywords internal
#' @noRd
biomes_raster_url <- function() {
  paste0(
    "https://github.com/azizka/biomes/releases/download/",
    "data-v1/Biomes_Inventory_RasterStack.tif"
  )
}

# Resolve the local raster path, downloading on first use. Used by all
# internal call sites that previously read from inst/extdata.
#' @keywords internal
#' @noRd
biomes_raster_path <- function() {
  dest <- biomes_cache_path()
  if (!file.exists(dest)) {
    biomes_download()
  }
  dest
}
