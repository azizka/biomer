#' Map occurrences over a biome layer
#'
#' Produces a publication-style map of occurrence records overlaid on a
#' single biome layer (rendered as a categorical raster). Optionally adds
#' a pie-chart inset showing the proportion of records per biome.
#'
#' The chosen biome layer is drawn directly with
#' [tidyterra::geom_spatraster()] (no polygonisation), occurrence points
#' are reprojected to the layer's CRS, and the count per biome --
#' computed via [biomes_classify()] so it matches [biomes_tab()] exactly
#' -- is appended to the legend labels.
#'
#' @param x A data frame with longitude/latitude columns, an `sf`
#'   spatial object, or a `terra::SpatVector` of point geometries.
#' @param layer Integer in `1:31` (default `1`). Index into the packaged
#'   biome raster stack. Ignored when `biome` is provided.
#' @param biome Optional `terra::SpatRaster` with a single layer. If
#'   `NULL`, the layer at index `layer` of the packaged raster stack is
#'   used.
#' @param lon Column name of longitude in `x` (data frame only).
#'   Default `"decimalLongitude"`.
#' @param lat Column name of latitude in `x` (data frame only).
#'   Default `"decimalLatitude"`.
#' @param pie Logical. If `TRUE`, draw a pie inset on the map showing the
#'   share of records per biome (only segments >= 5% are labelled).
#'   Default `FALSE`.
#' @param legend Logical. If `TRUE` (default), draw the biome colour
#'   legend on the right side of the map. Set to `FALSE` to drop it
#'   (useful for clean publication figures or when the pie inset already
#'   conveys the proportions).
#' @param point_color Colour of the occurrence points. Default
#'   `"#B20000"`.
#' @param point_size Numeric size of the occurrence points. Default
#'   `0.25`.
#' @param title Plot title. If `NULL` (default), a sensible title is
#'   generated from the layer name and the source.
#'
#' @return A `ggplot` object (when `pie = FALSE`) or a
#'   `cowplot::ggdraw` object (when `pie = TRUE`). Print to display or
#'   save with `ggplot2::ggsave()`.
#'
#' @examples
#' \dontrun{
#' data("biomes_example")
#' biomes_visualise(biomes_example, layer = 1)
#' biomes_visualise(biomes_example, layer = 17, pie = FALSE)
#' biomes_visualise(biomes_example, layer = 1, legend = FALSE)
#' }
#'
#' @export
biomes_visualise <- function(
    x,
    layer       = 1L,
    biome       = NULL,
    lon         = "decimalLongitude",
    lat         = "decimalLatitude",
    pie         = FALSE,
    legend      = TRUE,
    point_color = "#B20000",
    point_size  = 0.25,
    title       = NULL
) {

  # ----------------------------------------------------- assertions
  checkmate::assert_true(
    any(c("data.frame", "sf", "SpatVector") %in% class(x)),
    .var.name = "x"
  )
  checkmate::assert_int(layer, lower = 1L, upper = 31L)
  if (!is.null(biome)) checkmate::assert_class(biome, "SpatRaster")
  checkmate::assert_flag(pie)
  checkmate::assert_flag(legend)
  checkmate::assert_string(point_color)
  checkmate::assert_number(point_size, lower = 0)
  checkmate::assert_string(title, null.ok = TRUE)

  for (pkg in c("sf", "ggplot2", "viridis", "tidyterra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required for biomes_visualise().", pkg),
           call. = FALSE)
    }
  }
  if (pie) {
    for (pkg in c("cowplot", "ggforce")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' is required when pie = TRUE.", pkg),
             call. = FALSE)
      }
    }
  }

  # ----------------------------------------------------- raster layer
  if (is.null(biome)) {
    stack <- biomes_get()
    biome <- stack[[layer]]
  }
  if (terra::nlyr(biome) != 1L) {
    stop("biomes_visualise() expects a single raster layer.",
         "Pass e.g. biomes_get()[[1]] instead of the full stack.",
         call. = FALSE)
  }

  # ----------------------------------------------------- points -> sf
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    checkmate::assert_subset(c(lon, lat), choices = names(x), .var.name = "x")
    keep <- is.finite(x[[lon]]) & is.finite(x[[lat]])
    if (!any(keep)) {
      stop("No records with finite coordinates in `x`.", call. = FALSE)
    }
    if (!all(keep)) {
      warning(sprintf("Dropping %d record(s) with non-finite coordinates.",
                      sum(!keep)), call. = FALSE)
    }
    pts_sf <- sf::st_as_sf(x[keep, , drop = FALSE],
                           coords = c(lon, lat), crs = 4326)
  } else if (inherits(x, "sf")) {
    pts_sf <- x
  } else {
    pts_sf <- sf::st_as_sf(x)
  }
  pts_proj <- sf::st_transform(pts_sf, sf::st_crs(terra::crs(biome)))

  # ----------------------------------------------------- per-point classify
  # Same path as biomes_classify() / biomes_tab(), so legend counts always
  # agree with biomes_tab() exactly.
  pts_v  <- terra::project(terra::vect(pts_proj), terra::crs(biome))
  ex_pts <- terra::extract(biome, pts_v)
  raw_pt <- ex_pts[[setdiff(names(ex_pts), "ID")[1]]]

  # ----------------------------------------------------- biome labels
  leg     <- biomes::biomes_legend
  leg_row <- leg[leg$layer == as.integer(layer), , drop = FALSE]
  if (nrow(leg_row) >= 1L) {
    cls         <- .layer_lookup(as.integer(layer), leg)
    source_info <- as.character(leg_row$source[1])
  } else {
    cls         <- NULL
    source_info <- ""
  }

  # Union of values that appear in the raster OR in the points, so the
  # legend covers every coloured cell on the map *and* every biome the
  # records hit -- in one consistent order.
  ras_vals <- terra::unique(biome)[[1]]
  ras_vals <- ras_vals[!is.na(ras_vals)]
  all_vals <- sort(unique(c(ras_vals, raw_pt[!is.na(raw_pt)])))

  base_lab <- if (!is.null(cls)) cls[all_vals] else rep(NA_character_,
                                                        length(all_vals))
  na_lab   <- is.na(base_lab)
  if (any(na_lab)) {
    base_lab[na_lab] <- paste0("azonal (raster value: ",
                               all_vals[na_lab], ")")
  }

  n_per        <- vapply(all_vals,
                         function(v) sum(raw_pt == v, na.rm = TRUE),
                         integer(1))
  plot_labels  <- paste0(base_lab, " (", n_per, ")")
  biome_colors <- viridis::viridis(length(all_vals), option = "D")

  # ----------------------------------------------------- factor raster
  # Categorical SpatRaster so tidyterra::geom_spatraster gives a
  # discrete fill scale we can colour with scale_fill_manual().
  biome_fac           <- biome
  levels(biome_fac)   <- data.frame(ID = all_vals, biome = plot_labels)

  # ----------------------------------------------------- main map
  if (is.null(title)) {
    title <- sprintf("Biome map (layer %d)%s",
                     as.integer(layer),
                     if (nzchar(source_info)) paste0(" - ", source_info) else "")
  }

  names(biome_colors) <- plot_labels
  mapplot <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = biome_fac) +
    ggplot2::scale_fill_manual(
      "Biomes",
      values   = biome_colors,
      breaks   = plot_labels,
      na.value = "transparent",
      drop     = FALSE,
      guide    = if (legend) ggplot2::guide_legend(ncol = 1) else "none"
    ) +
    ggplot2::geom_sf(data = pts_proj, color = point_color,
                     alpha = 1, size = point_size,
                     inherit.aes = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = if (legend) "right" else "none",
      legend.title    = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.text     = ggplot2::element_text(size = 9),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      plot.title      = ggplot2::element_text(size = 14, face = "bold",
                                              hjust = 0.5)
    ) +
    ggplot2::ggtitle(title)

  if (!pie) return(mapplot)

  # ----------------------------------------------------- pie inset
  pie_df <- data.frame(
    label = plot_labels,
    count = n_per,
    color = biome_colors,
    stringsAsFactors = FALSE
  )
  pie_df <- pie_df[pie_df$count > 0, , drop = FALSE]

  if (nrow(pie_df) == 0L) {
    return(mapplot)
  }

  pie_df$fraction <- pie_df$count / sum(pie_df$count)
  pie_df$show     <- pie_df$fraction >= 0.05
  pie_df$pct_text <- paste0(round(100 * pie_df$fraction, 1), "%")
  pie_df$start    <- c(0, utils::head(cumsum(pie_df$fraction), -1))
  pie_df$end      <- cumsum(pie_df$fraction)
  pie_df$angle    <- 2 * pi * (pie_df$start + pie_df$fraction / 2)
  pie_df$line_x   <- sin(pie_df$angle) * 0.80
  pie_df$line_y   <- cos(pie_df$angle) * 0.80
  pie_df$lx       <- sin(pie_df$angle) * 1.3
  pie_df$ly       <- cos(pie_df$angle) * 1.3

  pie_plot <- ggplot2::ggplot(pie_df) +
    ggforce::geom_arc_bar(
      ggplot2::aes(
        start = .data$start * 2 * pi,
        end   = .data$end   * 2 * pi,
        r0 = 0.35, r = 0.8,
        fill = factor(.data$label, levels = .data$label),
        x0 = 0, y0 = 0
      ),
      color = NA
    ) +
    ggplot2::scale_fill_manual(values = pie_df$color, guide = "none") +
    ggplot2::coord_fixed(xlim = c(-0.9, 0.9), ylim = c(-0.9, 0.9),
                         clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(15, 60, 15, 60)) +
    ggplot2::geom_segment(
      data = subset(pie_df, pie_df$show),
      ggplot2::aes(x = .data$line_x, y = .data$line_y,
                   xend = .data$lx,  yend = .data$ly),
      color = "grey30", linewidth = 0.3
    ) +
    ggplot2::geom_label(
      data = subset(pie_df, pie_df$show),
      ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$pct_text),
      size = 3, fontface = "bold", fill = "white", color = "black",
      label.size = NA, label.padding = ggplot2::unit(0.05, "lines")
    )

  cowplot::ggdraw(mapplot) +
    cowplot::draw_plot(pie_plot, x = 0.62, y = 0.60,
                       width = 0.2, height = 0.2, scale = 1)
}
