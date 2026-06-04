#' Tabulate the number of occurrences per biome
#'
#' Summarizes the number of **occurrence records** (one row of `x` =
#' one occurrence) in each biome, for one or more biome layers. The
#' output is a long-format table with one row per (layer, biome) pair.
#'
#' This function counts occurrences, not species. To count unique species
#' per biome, deduplicate by species before tabulating
#' (e.g. `dplyr::distinct(species, biome)` after combining classifications
#' with the original data).
#'
#' @param x A data frame returned by [biomes_classify()].
#' @param value Character. `"names"` (default) tabulates the `_name`
#'   columns from [biomes_classify()]; `"ID"` tabulates the `_value`
#'   columns.
#'
#' @return A data frame with columns `layer`, `biome`, and `n` (the number
#'   of occurrence records in that biome on that layer).
#'
#' @examples
#' # Load example occurrence data
#' data("biomes_example")
#'
#' # Tabulate by biome name
#' classified_names <- biomes_classify(
#'   x     = biomes_example,
#'   value = "name"
#' )
#' biomes_tab(classified_names, value = "names")
#'
#' # Tabulate by raster value
#' classified_ids <- biomes_classify(
#'   x     = biomes_example,
#'   value = "ID"
#' )
#' biomes_tab(classified_ids, value = "ID")
#'
#' @export
biomes_tab <- function(x,
                       value = "names") {

  # Assertions
  checkmate::assert_data_frame(x, min.rows = 1)
  checkmate::assert_choice(value, c("names", "ID"))

  # Pick the relevant columns by suffix (drop = FALSE keeps single-layer
  # results as a data frame).
  suffix <- if (value == "names") "_name" else "_value"
  sel    <- grepl(paste0(suffix, "$"), names(x))

  checkmate::assert_true(
    any(sel),
    .var.name = paste0(
      "x must contain at least one column ending in '", suffix,
      "' (output of biomes_classify with value = '",
      if (value == "names") "name" else "ID",
      "' or 'both')"
    )
  )

  dat <- x[, sel, drop = FALSE]

  # Tabulate occurrences per biome, per layer.
  tab2 <- lapply(seq_along(dat), function(i) {
    counts <- table(dat[[i]], useNA = "no")
    if (length(counts) == 0) {
      return(NULL)
    }
    data.frame(
      layer = sub(paste0(suffix, "$"), "", names(dat)[i]),
      biome = names(counts),
      n     = as.integer(counts),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, tab2)
  rownames(out) <- NULL
  out
}
