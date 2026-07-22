#' One-call workflow: from taxon (or dataset) to table (and optional figure)
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
#'   * picks the biome scheme (either `scheme = <integer>` or, with the
#'     default `scheme = "best"`, by running [biomes_rank()] and
#'     selecting the top-1 scheme);
#'   * classifies the records with [biomes_classify()];
#'   * tabulates them with [biomes_tab()];
#'   * optionally builds a figure with [biomes_visualise()] (controlled
#'     by `plot`; skipped by default for speed).
#'
#' @param x Optional. A data frame with longitude/latitude columns, an
#'   `sf` spatial object, or a `terra::SpatVector`. Mutually exclusive
#'   with `taxon`.
#' @param taxon Optional scientific name (species, genus, family, ...).
#'   Mutually exclusive with `x`.
#' @param scheme One of: an integer in `1:31` (biome scheme number) to
#'   force a specific scheme; `"best"` (default) to pick the best-fitting
#'   scheme across all 31 via [biomes_rank()]; or a scheme type
#'   (`"climate"`, `"vegetation"`, `"land_cover"`, `"ecoregion"`,
#'   `"integrative"`, `"anthropogenic"`) to pick the best-fitting scheme
#'   within that methodological group.
#' @param lon,lat Column names of longitude / latitude in `x`
#'   (data frame only). Defaults `"decimalLongitude"`/`"decimalLatitude"`.
#' @param value Passed to [biomes_classify()]: `"name"` (default),
#'   `"ID"`, or `"both"`.
#' @param plot Which figure(s) [biomes_visualise()] should build. `"none"`
#'   (default): no figure (the fastest option). `"all"`: the combined,
#'   lettered figure (rank + map + barplot) in `$plot`. A subset of
#'   `c("rank", "map", "barplot")`: the requested panels are returned
#'   **individually** (no panel letters) in `$rank`, `$map` and `$barplot`
#'   -- e.g. `plot = c("rank", "map", "barplot")` fills all three, `plot =
#'   "map"` fills only `$map`. `NULL` is accepted as an alias for `"none"`.
#' @param show Logical. If `TRUE`, print the figure (if any) and the
#'   tabulation to the console as a side effect. The function always
#'   returns its result invisibly. Default: `FALSE`.
#' @param ... Further arguments passed to [biomes_occ()] when
#'   `taxon` is given (e.g. `limit`, `year_min`, `year_max`,
#'   `use_download`, GBIF credentials).
#'
#' @return Invisibly, a `biomes_full` list with elements:
#' \describe{
#'   \item{`occ`}{The occurrence data frame (downloaded or provided).}
#'   \item{`scheme`}{The chosen biome scheme number.}
#'   \item{`ranking`}{The ranking data frame (only when `scheme = "best"`),
#'     otherwise `NULL`.}
#'   \item{`classified`}{The output of [biomes_classify()].}
#'   \item{`table`}{The biome occurrence table from [biomes_tab()].}
#'   \item{`plot`}{The combined, lettered figure (only when `plot = "all"`),
#'     otherwise `NULL`.}
#'   \item{`rank`, `map`, `barplot`}{The individual panels (no letters),
#'     each present only when requested via `plot = c(...)`, otherwise
#'     `NULL`.}
#' }
#'
#' @examples
#' \dontrun{
#' # Path 1: from a taxon name (downloads via GBIF)
#' res <- biomes_full(taxon = "Fagus sylvatica", limit = 2000)
#' res$table
#'
#' # Path 2: from an existing data frame, pick the best scheme
#' data("biomes_example")
#' res <- biomes_full(x = biomes_example, scheme = "best")
#'
#' # Path 2 with a fixed scheme
#' res <- biomes_full(x = biomes_example, scheme = 1)
#'
#' # Path 2, best-fitting scheme within the vegetation group,
#' # and build the full figure
#' res <- biomes_full(x = biomes_example, scheme = "vegetation", plot = "all")
#' res$plot
#'
#' # individual panels (no a-c letters) in $rank / $map / $barplot
#' res <- biomes_full(x = biomes_example, plot = c("map", "barplot"))
#' res$map
#' res$barplot
#' }
#'
#' @export
biomes_full <- function(
    x      = NULL,
    taxon  = NULL,
    scheme = "best",
    lon    = "decimalLongitude",
    lat    = "decimalLatitude",
    value  = "name",
    plot   = "none",
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

  # `plot`: which figure biomes_visualise() should build and return in $plot.
  #   "none" (default) = no figure; "all" = full figure; or a subset of panels.
  #   NULL is accepted as an alias for "none".
  plot_choices <- c("none", "all", "rank", "map", "barplot")
  if (is.null(plot)) plot <- "none"
  checkmate::assert_character(plot, any.missing = FALSE, min.len = 1L,
                              .var.name = "plot")
  checkmate::assert_subset(plot, plot_choices, .var.name = "plot")

  scheme_types <- c("climate", "vegetation", "land_cover", "ecoregion",
                    "integrative", "anthropogenic")
  is_best   <- FALSE
  rank_type <- "all"
  if (is.character(scheme)) {
    s <- tolower(scheme)
    if (identical(s, "best")) {
      is_best <- TRUE
    } else if (s %in% scheme_types) {
      is_best   <- TRUE
      rank_type <- s
    } else {
      stop("`scheme` must be an integer in 1:31, \"best\", or one of the ",
           "scheme types (", paste(scheme_types, collapse = ", "), ").",
           call. = FALSE)
    }
  } else {
    checkmate::assert_int(scheme, lower = 1L, upper = 31L,
                          .var.name = "scheme")
    scheme <- as.integer(scheme)
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

  # ---------------------------------------------------------- choose scheme
  ranking <- NULL
  if (is_best) {
    ranking <- biomes_rank(occ, scheme_type = rank_type,
                           lon = lon, lat = lat, verbose = FALSE)
    scheme  <- as.integer(attr(ranking, "best_scheme"))
    if (is.na(scheme)) {
      stop("biomes_rank() could not identify a best scheme.", call. = FALSE)
    }
    message(sprintf("biomes_full(): best scheme = %d (%s)",
                    scheme,
                    ranking$scheme_name[ranking$is_best][1]))
  }

  stack       <- biomes_get()
  biome_layer <- stack[[scheme]]

  # ---------------------------------------------------------- classify
  classified <- suppressMessages(suppressWarnings(
    biomes_classify(x = occ, biome = biome_layer,
                    lon = lon, lat = lat, value = value)
  ))

  # ---------------------------------------------------------- table
  tab_value <- if (value == "ID") "ID" else "names"
  tab <- biomes_tab(classified, value = tab_value)

  # ---------------------------------------------------------- figure(s)
  # "all"           -> one combined, lettered figure in $plot
  # subset of panels -> individual panels (no letters) in $rank/$map/$barplot
  fig_plot <- fig_rank <- fig_map <- fig_barplot <- NULL
  if (!("none" %in% plot)) {
    if ("all" %in% plot) {
      fig_plot <- biomes_visualise(occ, scheme = scheme, scheme_type = rank_type,
                                   panels = c("rank", "map", "barplot"),
                                   lon = lon, lat = lat)
    } else {
      panels <- unique(plot)
      figs <- biomes_visualise(occ, scheme = scheme, scheme_type = rank_type,
                               panels = panels, combine = FALSE,
                               lon = lon, lat = lat)
      if (length(panels) == 1L) figs <- stats::setNames(list(figs), panels)
      fig_rank    <- figs[["rank"]]
      fig_map     <- figs[["map"]]
      fig_barplot <- figs[["barplot"]]
    }
  }

  # ---------------------------------------------------------- return
  out <- list(
    occ        = occ,
    scheme     = scheme,
    ranking    = ranking,
    classified = classified,
    table      = tab,
    plot       = fig_plot,
    rank       = fig_rank,
    map        = fig_map,
    barplot    = fig_barplot
  )
  class(out) <- c("biomes_full", "list")

  if (show) {
    for (f in Filter(Negate(is.null),
                     list(out$plot, out$rank, out$map, out$barplot))) {
      print(f)
    }
    print(tab)
  }

  invisible(out)
}


#' @export
print.biomes_full <- function(x, ...) {
  cat("<biomes_full result>\n")
  cat(sprintf("  occurrences : %d records\n", nrow(x$occ)))
  cat(sprintf("  scheme      : %d\n", x$scheme))
  if (!is.null(x$ranking)) {
    cat(sprintf("  picked by   : biomes_rank() (composite = %.3f)\n",
                x$ranking$composite_score[x$ranking$is_best][1]))
  }
  cat(sprintf("  table rows  : %d (biome classes used)\n", nrow(x$table)))
  built <- c(if (!is.null(x$plot))    "$plot",
             if (!is.null(x$rank))    "$rank",
             if (!is.null(x$map))     "$map",
             if (!is.null(x$barplot)) "$barplot")
  cat(sprintf("  figure(s)   : %s\n",
              if (length(built)) paste(built, collapse = ", ") else "none"))
  cat("Components: $occ, $scheme, $ranking, $classified, $table,",
      "$plot, $rank, $map, $barplot\n")
  invisible(x)
}
