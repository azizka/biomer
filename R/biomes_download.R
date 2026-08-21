#' Download the packaged biome raster stack
#'
#' The 31-layer biome raster stack (`Biomes_Inventory_RasterStack.tif`,
#' ~36 MB) is too large to ship inside the package on CRAN. It is hosted
#' as a release asset on GitHub instead. `biomes_download()` fetches it
#' once and reuses the local copy on every later call, including every
#' internal use by [biomes_get()] or [biomes_classify()].
#'
#' The storage location is chosen as follows:
#' \itemize{
#'   \item If `path` is supplied, the raster is written to that
#'     directory and no other location is touched.
#'   \item If `path` is `NULL` and a copy already exists in the
#'     persistent per-user cache directory (see [tools::R_user_dir()]),
#'     that copy is reused.
#'   \item Otherwise, in interactive sessions, the function asks once
#'     for permission to store the raster in the persistent per-user
#'     cache directory, so that the download happens only once across
#'     R sessions.
#'   \item If permission is declined, or in non-interactive sessions,
#'     the raster is stored under [tempdir()] and is removed
#'     automatically when the R session ends.
#' }
#'
#' The package therefore never writes outside [tempdir()] without the
#' user's explicit consent (an interactive confirmation or an explicit
#' `path`).
#'
#' @param path Optional character string: directory in which to store
#'   the raster. Default `NULL`: use the persistent per-user cache
#'   directory if the user has agreed to it, otherwise [tempdir()]
#'   (see Details).
#' @param overwrite Logical flag; if `TRUE`, re-download the raster even
#'   when a local copy already exists. Defaults to `FALSE`.
#' @param quiet Logical flag; if `TRUE`, suppress the informational
#'   message and the download progress bar. Defaults to `FALSE`.
#'
#' @return The local file path to the raster, invisibly.
#'
#' @seealso [biomes_get()] to load the raster as a `terra::SpatRaster`.
#'
#' @examples
#' \donttest{
#' # Downloads ~36 MB into the session's temporary directory.
#' raster_path <- biomes_download(path = tempdir())
#' raster_path
#' }
#'
#' @importFrom utils download.file
#' @export
biomes_download <- function(path = NULL, overwrite = FALSE, quiet = FALSE) {

  checkmate::assert_string(path, null.ok = TRUE)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(quiet)

  dir  <- if (is.null(path)) biomes_resolve_cache_dir() else path
  dest <- file.path(dir, "Biomes_Inventory_RasterStack.tif")

  if (file.exists(dest) && !overwrite) {
    return(invisible(dest))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (!quiet) {
    message("Downloading biome raster stack (~36 MB) to:\n  ", dest)
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

# Persistent per-user cache directory (used only with user consent).
#' @keywords internal
#' @noRd
biomes_cache_dir <- function() {
  tools::R_user_dir("biomes", which = "cache")
}

# Full path to a raster copy in the persistent cache (may not exist).
#' @keywords internal
#' @noRd
biomes_cache_path <- function() {
  file.path(biomes_cache_dir(), "Biomes_Inventory_RasterStack.tif")
}

# Full path to a raster copy in the session cache (may not exist).
#' @keywords internal
#' @noRd
biomes_temp_path <- function() {
  file.path(tempdir(), "biomes", "Biomes_Inventory_RasterStack.tif")
}

# Decide where a fresh download may be stored. The persistent per-user
# cache is used only if a copy is already there (earlier consent) or if
# the user agrees interactively; otherwise fall back to tempdir(), which
# CRAN policy always allows.
#' @keywords internal
#' @noRd
biomes_resolve_cache_dir <- function() {
  if (file.exists(biomes_cache_path())) {
    return(biomes_cache_dir())
  }
  if (interactive()) {
    ans <- utils::askYesNo(
      paste0(
        "biomes would like to store the biome raster (~36 MB) in the\n",
        "per-user cache directory\n  ", biomes_cache_dir(), "\n",
        "so it is downloaded only once. Allow?"
      )
    )
    if (isTRUE(ans)) {
      return(biomes_cache_dir())
    }
  }
  dirname(biomes_temp_path())
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

# Resolve the local raster path, downloading on first use. Checks the
# persistent per-user cache first, then the session cache.
#' @keywords internal
#' @noRd
biomes_raster_path <- function() {
  if (file.exists(biomes_cache_path())) {
    return(biomes_cache_path())
  }
  if (file.exists(biomes_temp_path())) {
    return(biomes_temp_path())
  }
  biomes_download()
}
