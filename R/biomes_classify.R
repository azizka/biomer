#' Classify occurrences into biomes
#'
#' For each occurrence record, assigns a biome label based on its spatial
#' position and one or more biome raster layers. One row of the returned
#' data frame corresponds to one row (one occurrence record) of `x`.
#'
#' @param x A data frame (with longitude and latitude columns), an `sf`
#'   spatial object, or a `terra::SpatVector` of point geometries.
#' @param layer Integer vector in `1:31` selecting one or more layers
#'   from the packaged biome stack (e.g. `layer = 1` or
#'   `layer = c(1, 25)`). Ignored when `biome` is supplied. Defaults to
#'   `NULL`, meaning *all* 31 layers.
#' @param biome Optional `terra::SpatRaster` with one or more biome
#'   layers. Use this for custom rasters; for the packaged stack prefer
#'   `layer = <int>`.
#' @param lon Column name of longitude in `x` (only used if `x` is a
#'   non-spatial data frame). Default: "decimalLongitude".
#' @param lat Column name of latitude in `x` (only used if `x` is a
#'   non-spatial data frame). Default: "decimalLatitude".
#' @param value Character. One of `"ID"`, `"name"`, or `"both"`. Controls
#'   whether the returned data frame contains the raw raster value
#'   (`"ID"`), the biome name (`"name"`), or both (`"both"`).
#' @param append Logical. If `TRUE` (default), the classification columns
#'   are appended to the input `x`. If `FALSE`, only the classification
#'   columns are returned (one column per layer for `"ID"` / `"name"`,
#'   two columns per layer for `"both"`).
#' @param na Character or `NA`. Label used for records that fall outside
#'   every biome polygon (`NA` in the raw extraction) in the `_name`
#'   columns. Defaults to `"no_biome"`, so such records are counted by
#'   [biomes_tab()] under that label. Pass `na = NA` to keep the missing
#'   values as `NA`.
#' @param raster_file Optional path to a custom biome raster stack file
#'   or a `SpatRaster`. Only used if `biome` is `NULL`.
#'
#' @return A data frame with one row per record in `x`. By default
#'   (`append = TRUE`) the original columns of `x` are kept and the
#'   classification columns are added on the right. With `append = FALSE`
#'   only the classification columns are returned. Classification columns
#'   are named after the input layers, with the suffix `_value` for the
#'   raster value and `_name` for the biome name. Raster values without
#'   a name in the legend (typically azonal classes encoded with high
#'   values) fall back to `"azonal (raster value: X)"`.
#'
#' @examples
#' # Load example occurrence data
#' data("biomes_example")
#'
#' # Default: classify against all 31 layers and append the result to x
#' biomes_classify(biomes_example)
#'
#' # Single layer, both raster value and biome name
#' biomes_classify(biomes_example, layer = 1, value = "both")
#'
#' # Multiple layers
#' biomes_classify(biomes_example, layer = c(1, 25))
#'
#' # Return only the classification columns (old default behaviour)
#' biomes_classify(biomes_example, layer = 1, append = FALSE)
#'
#' @importFrom terra nlyr rast sources
#' @importFrom readr parse_number
#' @export
biomes_classify <- function(
    x,
    layer = NULL,
    biome = NULL,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    value = "name",
    append = TRUE,
    na = "no_biome",
    raster_file = NULL
) {

  # Assertions: x is either an sf spatial object, a SpatVector or a data frame
  checkmate::assert_true(
    any(c("data.frame", "sf", "SpatVector") %in% class(x)),
    .var.name = "x"
  )

  # Assertions: x contains the lat and long columns if a (non-sf) data frame
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    checkmate::assert_subset(c(lon, lat), choices = names(x), .var.name = "x")
    checkmate::assert_numeric(x[[lon]], any.missing = FALSE)
    checkmate::assert_numeric(x[[lat]], any.missing = FALSE)
  }

  # Assertions: biome is a terra raster or raster stack
  if (!is.null(biome)) {
    checkmate::assert_class(biome, "SpatRaster")
  }
  if (!is.null(layer)) {
    checkmate::assert_integerish(layer, lower = 1L, upper = 31L,
                                 any.missing = FALSE, min.len = 1L,
                                 .var.name = "layer")
    if (!is.null(biome)) {
      warning("`layer` is ignored because `biome` was supplied.",
              call. = FALSE)
    }
  }

  # Assertions: raster_file must be NULL, a path to a tif, or a SpatRaster
  if (!is.null(raster_file)) {
    if (is.character(raster_file)) {
      checkmate::assert_file_exists(raster_file)
    } else {
      checkmate::assert_class(raster_file, "SpatRaster")
    }
  }

  # Assertions: value
  checkmate::assert_choice(value, c("ID", "name", "both"))
  checkmate::assert_flag(append)
  if (!is.na(na)) checkmate::assert_string(na)

  # Resolve biome raster:
  #   1) explicit SpatRaster in `biome`              -> use as-is
  #   2) integer index/indices in `layer`            -> subset of the default
  #   3) custom raster_file path or SpatRaster       -> load that
  #   4) nothing -> packaged 31-layer default stack
  if (is.null(biome)) {
    if (is.null(raster_file)) {
      if (is.null(layer)) {
        message("no biome file or layer provided using default biomes")
      }
      raster_file <- system.file(
        "extdata",
        "Biomes_Inventory_RasterStack.tif",
        package = "biomes"
      )
    }
    biome <- terra::rast(raster_file)
    if (!is.null(layer)) {
      biome <- biome[[as.integer(layer)]]
    }
  }

  # Prepare points: build a SpatVector and reproject to the raster CRS
  # once, up front. Pre-projecting silences terra::extract()'s "transforming
  # vector data to the CRS of the raster" warning, and we do the WGS84
  # assumption as a low-noise message rather than a warning.
  if (is.data.frame(x) && !inherits(x, "sf")) {
    message("Coordinates provided as data.frame, assuming WGS84 as CRS.")
    pts <- terra::vect(
      as.data.frame(x),
      geom = c(lon, lat),
      crs  = "EPSG:4326"
    )
  } else if (inherits(x, "sf")) {
    pts <- terra::vect(x)
  } else {
    pts <- x   # SpatVector
  }
  raster_crs <- terra::crs(biome)
  if (nzchar(raster_crs) && !is.na(raster_crs)) {
    pts_crs <- terra::crs(pts)
    if (!nzchar(pts_crs) || !identical(pts_crs, raster_crs)) {
      pts <- terra::project(pts, raster_crs)
    }
  }

  # Use the packaged legend if the layer names follow the default pattern
  # ("Biome_Inventory_layer_NN"). This is more robust than comparing
  # terra::sources(): the latter can fail after subsetting, in-memory copies,
  # or pkgload::load_all() dev sessions, even though the raster values are
  # still the packaged ones.
  default_raster <- system.file(
    "extdata",
    "Biomes_Inventory_RasterStack.tif",
    package = "biomes"
  )
  is_default <- identical(terra::sources(biome), default_raster) ||
    all(grepl("^Biome_Inventory_layer_[0-9]+$", names(biome)))
  legend_df  <- if (is_default) biomes::biomes_legend else NULL

  # Extract raster values at each point. terra::extract() adds a leading
  # "ID" column with the point index plus one column per raster layer.
  biome_id   <- terra::extract(biome, pts)
  layer_cols <- setdiff(names(biome_id), "ID")

  # Print a short summary of which biome layers were used (#62).
  layer_idx <- suppressWarnings(readr::parse_number(layer_cols))
  if (!is.null(legend_df) && all(!is.na(layer_idx))) {
    message(sprintf(
      "Classified %d record(s) against %d biome layer(s):",
      nrow(biome_id), length(layer_cols)
    ))
    for (i in seq_along(layer_idx)) {
      idx <- layer_idx[i]
      pub <- if (idx >= 1 && idx <= nrow(biomes::biomes_information)) {
        biomes::biomes_information[[idx, "publication"]]
      } else {
        NA
      }
      message(sprintf("  - %s (%s)", layer_cols[i], pub))
    }
  } else {
    message(sprintf(
      "Classified %d record(s) against %d layer(s): %s",
      nrow(biome_id), length(layer_cols),
      paste(layer_cols, collapse = ", ")
    ))
  }

  # Build a data frame of layer values (drop terra's point-id column and
  # rename to the user-facing "<layer>_value" pattern).
  value_df <- biome_id[, layer_cols, drop = FALSE]
  names(value_df) <- paste0(layer_cols, "_value")

  # If we cannot match the layer to the packaged legend, return raw values.
  if (is.null(legend_df) || value == "ID") {
    if (value == "ID" || is.null(legend_df)) {
      if (value != "ID" && is.null(legend_df)) {
        warning(
          "biome names are only available for the default raster; ",
          "returning raster values instead of names."
        )
      }
      return(.maybe_append(x, value_df, append))
    }
  }

  # Build a parallel data frame of biome names.
  name_df <- as.data.frame(
    lapply(layer_cols, function(layer_name) {
      layer_index <- readr::parse_number(layer_name)
      lookup      <- .layer_lookup(layer_index, legend_df)
      raw_values  <- value_df[[paste0(layer_name, "_value")]]

      out <- unname(lookup[raw_values])
      # Last-resort fallback for raster values neither in the regular
      # legend nor in the universal azonal codes (95..98).
      azonal <- which(is.na(out) & !is.na(raw_values))
      if (length(azonal) > 0) {
        out[azonal] <- paste0("azonal (raster value: ",
                              raw_values[azonal], ")")
      }
      # Label points that fall outside every polygon (raw NA) so that
      # they are counted by biomes_tab() instead of being silently
      # dropped. Set na = NA in biomes_classify() to keep the missings.
      if (!is.na(na)) {
        outside <- which(is.na(out) & is.na(raw_values))
        if (length(outside) > 0) out[outside] <- na
      }
      out
    }),
    stringsAsFactors = FALSE
  )
  names(name_df) <- paste0(layer_cols, "_name")

  if (value == "name") {
    return(.maybe_append(x, name_df, append))
  }

  # value == "both": interleave so that for each layer the *_value column
  # is followed by its *_name column.
  interleaved <- as.list(rep(NA, 2 * length(layer_cols)))
  for (i in seq_along(layer_cols)) {
    interleaved[[2 * i - 1]] <- value_df[[i]]
    interleaved[[2 * i]]     <- name_df[[i]]
  }
  out <- as.data.frame(interleaved, stringsAsFactors = FALSE)
  names(out) <- as.vector(rbind(names(value_df), names(name_df)))
  .maybe_append(x, out, append)
}

#' Build a length-98 lookup vector mapping raster value -> biome label
#' for a single packaged biome layer.
#'
#' Regular raster values 1..N pull straight from the layer's `id_N`
#' columns. The high "azonal" raster codes 95..98 (Inland water,
#' Oceanic islands, Mountains, Urban) are not stored at those positions
#' in `biomes_legend`; instead each layer lists their per-layer wording
#' somewhere in its `id_*` columns. This helper looks those labels up by
#' name and slots them into positions 95..98 so a plain `lookup[value]`
#' works for every code the rasters use.
#'
#' @param layer_idx Integer index in `1:nrow(legend_df)`.
#' @param legend_df Data frame in the shape of `biomes::biomes_legend`.
#' @keywords internal
#' @noRd
.layer_lookup <- function(layer_idx, legend_df) {
  cls <- unname(unlist(legend_df[layer_idx, -c(1, 2)]))
  if (length(cls) < 98L) {
    cls <- c(cls, rep(NA_character_, 98L - length(cls)))
  }
  azonal <- c(`95` = "Inland water",
              `96` = "Oceanic islands",
              `97` = "Mountains",
              `98` = "Urban")
  for (code_str in names(azonal)) {
    code <- as.integer(code_str)
    if (!is.na(cls[code])) next   # already filled (unlikely but harmless)
    hit <- which(tolower(cls) == tolower(azonal[[code_str]]))
    if (length(hit) > 0L) cls[code] <- cls[hit[1]]
  }
  cls
}


#' Append classification columns to the original input, or return them
#' on their own.
#'
#' For data frames and `sf` objects we use `cbind()` so the original
#' columns / geometry are preserved. For `SpatVector` we add the
#' classification columns via `terra::values<-`. If `append = FALSE`,
#' we just return the classification table on its own.
#'
#' @keywords internal
#' @noRd
.maybe_append <- function(x, class_df, append) {
  if (!isTRUE(append)) return(class_df)

  if (inherits(x, "SpatVector")) {
    out <- x
    vals <- as.data.frame(x)
    terra::values(out) <- cbind(vals, class_df)
    return(out)
  }
  cbind(x, class_df)
}
