#' One-call workflow: from taxon (or dataset) to table + map
#'
#' Convenience wrapper that runs the full `biomes` workflow in a single
#' call. There are two entry paths:
#'
#' 1. **From a taxon name.** Pass a scientific name as `taxon`
#'    (`x = NULL`). `biomes_full()` calls [biomes_occ()] to download
#'    cleaned GBIF occurrences for the taxon and then proceeds as
#'    below.
#' 2. **From an occurrence dataset.** Pass a data frame, `sf` object
#'    or `terra::SpatVector` as `x` (`taxon = NULL`).
#'
#' Once occurrences are available the function:
#'   * picks the biome layer (either `layer = <integer>` or, with the
#'     default `layer = "best"`, by running [biomes_rank()] and
#'     selecting the top-1 layer);
#'   * classifies the records with [biomes_classify()];
#'   * tabulates them with [biomes_tab()];
#'   * draws an occurrence map with [biomes_visualise()].
#'
#' @param x Optional. A data frame with longitude/latitude columns, an
#'   `sf` spatial object, or a `terra::SpatVector`. Mutually exclusive
#'   with `taxon`.
#' @param taxon Optional scientific name (species, genus, family, ...).
#'   Mutually exclusive with `x`.
#' @param layer Either an integer in `1:31` to force a specific layer,
#'   or `"best"` (default) to pick the best layer via [biomes_rank()].
#' @param lon,lat Column names of longitude / latitude in `x`
#'   (data frame only). Defaults `"decimalLongitude"`/`"decimalLatitude"`.
#' @param value Passed to [biomes_classify()]: `"name"` (default),
#'   `"ID"`, or `"both"`.
#' @param show Logical. If `TRUE`, print the map and the tabulation
#'   to the console as a side effect. The function always returns its
#'   result invisibly. Default: `FALSE`.
#' @param ... Further arguments passed to [biomes_occ()] when
#'   `taxon` is given (e.g. `limit`, `year_min`, `year_max`,
#'   `use_download`, GBIF credentials).
#'
#' @return Invisibly, a `biomes_full` list with elements:
#' \describe{
#'   \item{`occ`}{The occurrence data frame (downloaded or provided).}
#'   \item{`layer`}{The chosen layer index.}
#'   \item{`ranking`}{The ranking data frame (only when `layer = "best"`),
#'     otherwise `NULL`.}
#'   \item{`classified`}{The output of [biomes_classify()].}
#'   \item{`table`}{The biome occurrence table from [biomes_tab()].}
#'   \item{`map`}{The `ggplot`/`cowplot` map from [biomes_visualise()].}
#' }
#'
#' @examples
#' \dontrun{
#' # Path 1: from a taxon name (downloads via GBIF)
#' res <- biomes_full(taxon = "Fagus sylvatica", limit = 2000)
#' res$table
#' res$map
#'
#' # Path 2: from an existing data frame, pick the best layer
#' data("biomes_example")
#' res <- biomes_full(x = biomes_example, layer = "best")
#'
#' # Path 2 with a fixed layer
#' res <- biomes_full(x = biomes_example, layer = 1)
#' }
#'
#' @export
biomes_full <- function(
    x      = NULL,
    taxon  = NULL,
    layer  = "best",
    lon    = "decimalLongitude",
    lat    = "decimalLatitude",
    value  = "name",
    show   = FALSE,
    ...
) {

  # ---------------------------------------------------------- assertions
  has_x     <- !is.null(x)
  has_taxon <- !is.null(taxon)
  if (has_x == has_taxon) {
    stop("Provide exactly one of `x` (occurrence data) or `taxon` ",
         "(scientific name).", call. = FALSE)
  }
  checkmate::assert_flag(show)
  checkmate::assert_choice(value, c("name", "ID", "both"))

  is_best <- is.character(layer) && identical(tolower(layer), "best")
  if (!is_best) {
    checkmate::assert_int(layer, lower = 1L, upper = 31L,
                          .var.name = "layer")
    layer <- as.integer(layer)
  }

  # ---------------------------------------------------------- occurrences
  if (has_taxon) {
    checkmate::assert_character(taxon, any.missing = FALSE,
                                min.chars = 1L, min.len = 1L)
    occ <- biomes_occ(taxon = taxon, ...)
    if (!is.data.frame(occ) || nrow(occ) == 0L) {
      stop("biomes_occ() returned no records for: ",
           paste(taxon, collapse = ", "), ".", call. = FALSE)
    }
  } else {
    occ <- x
  }

  # ---------------------------------------------------------- choose layer
  ranking <- NULL
  if (is_best) {
    ranking <- biomes_rank(occ, lon = lon, lat = lat, verbose = FALSE)
    layer   <- as.integer(attr(ranking, "best_layer"))
    if (is.na(layer)) {
      stop("biomes_rank() could not identify a best layer.", call. = FALSE)
    }
    message(sprintf("biomes_full(): best layer = %d (%s)",
                    layer,
                    ranking$layer_name[ranking$is_best][1]))
  }

  stack       <- biomes_get()
  biome_layer <- stack[[layer]]

  # ---------------------------------------------------------- classify
  classified <- suppressMessages(suppressWarnings(
    biomes_classify(x = occ, biome = biome_layer,
                    lon = lon, lat = lat, value = value)
  ))

  # ---------------------------------------------------------- table
  tab_value <- if (value == "ID") "ID" else "names"
  tab <- biomes_tab(classified, value = tab_value)

  # ---------------------------------------------------------- map
  map <- biomes_visualise(occ, layer = layer, biome = biome_layer,
                          lon = lon, lat = lat)

  # ---------------------------------------------------------- return
  out <- list(
    occ        = occ,
    layer      = layer,
    ranking    = ranking,
    classified = classified,
    table      = tab,
    map        = map
  )
  class(out) <- c("biomes_full", "list")

  if (show) {
    print(map)
    print(tab)
  }

  invisible(out)
}


#' @export
print.biomes_full <- function(x, ...) {
  cat("<biomes_full result>\n")
  cat(sprintf("  occurrences : %d records\n", nrow(x$occ)))
  cat(sprintf("  layer       : %d\n", x$layer))
  if (!is.null(x$ranking)) {
    cat(sprintf("  picked by   : biomes_rank() (composite = %.3f)\n",
                x$ranking$composite_score[x$ranking$is_best][1]))
  }
  cat(sprintf("  table rows  : %d (biomes used)\n", nrow(x$table)))
  cat("Components: $occ, $layer, $ranking, $classified, $table, $map\n")
  invisible(x)
}
