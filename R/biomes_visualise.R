#' Visualise the biomes workflow (ranking, map and biome-class composition)
#'
#' Produces the publication figure of the *biomes* workflow for a set of
#' occurrence records. Up to three panels are drawn and combined:
#'
#' * **rank**: the data-driven ranking of the biome schemes
#'   ([biomes_rank()]): the composite score per scheme (best highlighted)
#'   next to the raw criterion values it averages (coverage, effective
#'   number of classes, granularity, ...).
#' * **map**: the occurrence records (points) mapped over the chosen
#'   biome scheme, with the number of records per biome class optionally
#'   appended to the legend labels.
#' * **barplot**: the number of occurrence records (left) and species
#'   (right) per biome class, with the biome-class names in the centre.
#'
#' Which panels are drawn is controlled by `panels`; the panel letters
#' (a, b, c) are assigned in drawing order, so selecting only `rank` and
#' `barplot` labels them (a) and (b).
#'
#' @param x A data frame with longitude/latitude columns, an `sf` spatial
#'   object, or a `terra::SpatVector` of point geometries.
#' @param scheme Integer in `1:31` (biome scheme number). If `NULL`
#'   (default), the best-fitting scheme is chosen by [biomes_rank()]
#'   (within `scheme_type`).
#' @param scheme_type Character. Methodological group to rank within when
#'   `scheme` is `NULL`; passed to [biomes_rank()]. Default `"all"`.
#' @param biome Optional single-layer `terra::SpatRaster`. If supplied it
#'   is mapped directly and only the `map` panel is available (no ranking).
#' @param lon,lat Column names of longitude / latitude in `x`
#'   (data frame only). Defaults `"decimalLongitude"`/`"decimalLatitude"`.
#' @param panels Character vector, any subset of `c("rank", "map",
#'   "barplot")` (default all three). Panels are drawn and lettered in
#'   this order.
#' @param legend_counts Logical. If `TRUE` (default), append the number of
#'   records per biome class to the map legend labels.
#' @param legend Logical. If `TRUE` (default), draw the biome-class colour
#'   legend on the map panel.
#' @param point_color Colour of the occurrence points. Default `"#B20000"`.
#' @param point_size Numeric size of the occurrence points. Default `0.25`.
#' @param combine Logical. When more than one panel is drawn: `TRUE`
#'   (default) combines them into one lettered figure (a, b, c); `FALSE`
#'   returns a **named list** of the individual panels (no letters).
#'   Ignored for a single panel (always returned as a bare `ggplot`).
#' @param verbose Logical. Passed to [biomes_rank()]. Default `FALSE`.
#'
#' @return For a single panel, a `ggplot` object. For several panels: a
#'   combined `cowplot` object when `combine = TRUE` (default), or a named
#'   list of `ggplot` objects (`rank`, `map`, `barplot`) when
#'   `combine = FALSE`. Print to display or save with [ggplot2::ggsave()].
#'
#' @examples
#' \dontrun{
#' data("biomes_example")
#' # full figure (rank + map + barplot), best scheme chosen automatically
#' biomes_visualise(biomes_example)
#'
#' # only the map, for a fixed scheme
#' biomes_visualise(biomes_example, scheme = 1, panels = "map")
#'
#' # map + barplot for the best vegetation scheme
#' biomes_visualise(biomes_example, scheme_type = "vegetation",
#'                  panels = c("map", "barplot"))
#' }
#'
#' @export
biomes_visualise <- function(
    x,
    scheme        = NULL,
    scheme_type   = "all",
    biome         = NULL,
    lon           = "decimalLongitude",
    lat           = "decimalLatitude",
    panels        = c("rank", "map", "barplot"),
    legend_counts = TRUE,
    legend        = TRUE,
    point_color   = "#B20000",
    point_size    = 0.25,
    combine       = TRUE,
    verbose       = FALSE
) {

  # ----------------------------------------------------- assertions
  checkmate::assert_true(
    any(c("data.frame", "sf", "SpatVector") %in% class(x)),
    .var.name = "x"
  )
  panels <- unique(match.arg(panels, c("rank", "map", "barplot"),
                             several.ok = TRUE))
  panels <- c("rank", "map", "barplot")[c("rank", "map", "barplot") %in% panels]
  if (!is.null(scheme)) checkmate::assert_int(scheme, lower = 1L, upper = 31L)
  if (!is.null(biome)) checkmate::assert_class(biome, "SpatRaster")
  checkmate::assert_flag(legend_counts)
  checkmate::assert_flag(legend)
  checkmate::assert_string(point_color)
  checkmate::assert_number(point_size, lower = 0)
  checkmate::assert_flag(combine)

  for (pkg in c("sf", "ggplot2", "viridis", "tidyterra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required for biomes_visualise().", pkg),
           call. = FALSE)
    }
  }

  # A custom raster cannot be ranked -> only the map panel is meaningful.
  # (Restrict BEFORE the cowplot check, so a custom-raster map never needs it.)
  if (!is.null(biome)) {
    if (!all(panels == "map")) {
      warning("`biome` was supplied; only the 'map' panel is drawn.",
              call. = FALSE)
    }
    panels <- "map"
  }
  if (length(panels) == 0L) {
    stop("`panels` must select at least one of 'rank', 'map', 'barplot'.",
         call. = FALSE)
  }

  # The rank and barplot panels are themselves multi-part; they and any
  # multi-panel figure need cowplot. A lone map does not.
  needs_cowplot <- length(panels) > 1L || any(c("rank", "barplot") %in% panels)
  if (needs_cowplot && !requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package 'cowplot' is required for the 'rank'/'barplot' panels and ",
         "to combine panels. Install it with install.packages('cowplot').",
         call. = FALSE)
  }

  # ----------------------------------------------------- ranking (if needed)
  ranking <- NULL
  need_rank <- ("rank" %in% panels) || (is.null(scheme) && is.null(biome))
  if (need_rank) {
    ranking <- biomes_rank(x, scheme_type = scheme_type,
                           lon = lon, lat = lat, verbose = verbose)
    if (is.null(scheme)) {
      scheme <- as.integer(attr(ranking, "best_scheme"))
      if (is.na(scheme)) {
        stop("biomes_rank() could not identify a best scheme.", call. = FALSE)
      }
    }
  }
  if (is.null(scheme) && is.null(biome)) {
    stop("Provide `scheme`, or allow ranking to choose one.", call. = FALSE)
  }

  # ----------------------------------------------------- build panels
  plots <- list()
  if ("rank" %in% panels) {
    plots$rank <- .biomes_panel_rank(ranking)
  }
  if ("map" %in% panels) {
    plots$map <- .biomes_panel_map(
      x, scheme = scheme, biome = biome, lon = lon, lat = lat,
      legend = legend, legend_counts = legend_counts,
      point_color = point_color, point_size = point_size
    )
  }
  if ("barplot" %in% panels) {
    plots$barplot <- .biomes_panel_barplot(
      x, scheme = scheme, biome = biome, lon = lon, lat = lat
    )
  }
  plots <- plots[c("rank", "map", "barplot")]
  plots <- plots[!vapply(plots, is.null, logical(1))]

  if (length(plots) == 1L) return(plots[[1]])
  if (!combine) return(plots)   # named list of individual panels, no letters

  cowplot::plot_grid(
    plotlist   = plots,
    ncol       = 1,
    rel_heights = ifelse(names(plots) == "map", 1.05, 0.95),
    labels     = letters[seq_along(plots)],
    label_size = 14,
    label_x = 0, label_y = 1, hjust = 0, vjust = 1.3
  )
}


# ----------------------------------------------------------------------------
# Panel builders (internal)
# ----------------------------------------------------------------------------

#' Ranking panel (a): composite score + the raw criteria it averages.
#' Composite bar (with the biome-scheme numbers on the y-axis, best
#' highlighted) plus one zoomed bar per criterion, sharing one scheme order.
#' @keywords internal
#' @noRd
.biomes_panel_rank <- function(ranking) {
  if (is.null(ranking)) {
    stop("The 'rank' panel needs a ranking; do not pass a custom `biome`.",
         call. = FALSE)
  }
  rk <- as.data.frame(ranking)
  criteria <- attr(ranking, "criteria")
  if (is.null(criteria)) criteria <- "coverage"

  # scheme order: ascending composite so the best scheme sits at the TOP
  rk$scheme_f <- factor(as.character(rk$scheme),
                        levels = as.character(rk$scheme)[order(rk$composite_score)])

  # shrink the y-axis labels when there are many schemes
  n_s   <- nrow(rk)
  ysize <- if (n_s >= 25) 5 else if (n_s >= 15) 6.5 else 8

  no_grid_y <- ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank()
  )
  drop_y <- ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank())

  # (a) composite score, best highlighted
  p_comp <- ggplot2::ggplot(rk, ggplot2::aes(x = .data$composite_score,
                                             y = .data$scheme_f,
                                             fill = .data$is_best)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "#1b9e77", `FALSE` = "grey70"), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Composite score", y = "Biome scheme no.") +
    ggplot2::theme_minimal(base_size = 11) + no_grid_y +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = ysize),
                   plot.margin = ggplot2::margin(12, 6, 2, 12))

  crit_pretty <- c(coverage = "Coverage", effective_classes = "Eff. classes",
                   granularity = "Granularity",
                   informativeness = "Informativeness", agreement = "Agreement")
  crit_col <- c(coverage = "#0072B2", effective_classes = "#E69F00",
                granularity = "#009E73", informativeness = "#CC79A7",
                agreement = "#56B4E9")

  crit_bar <- function(key) {
    raw_col <- paste0(key, "_raw")
    v <- rk[[raw_col]]
    if (is.null(v)) return(NULL)
    d <- data.frame(scheme_f = rk$scheme_f, value = v)
    fin <- d$value[is.finite(d$value)]
    if (length(fin) == 0) fin <- 0
    lo <- min(fin); hi <- max(fin); rng <- hi - lo
    if (rng <= 0) { lo <- lo - 0.5; hi <- hi + 0.5 } else {
      lo <- lo - 0.05 * rng; hi <- hi + 0.05 * rng
    }
    lab_fun <- function(z) ifelse(!is.finite(z), "",
                                  ifelse(round(z, 2) >= 1 & z < 1, "0.99",
                                         as.character(round(z, 2))))
    ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$scheme_f)) +
      ggplot2::geom_col(fill = unname(crit_col[key]), width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = lab_fun(.data$value)),
                         hjust = 1.1, size = 1.9, colour = "white") +
      ggplot2::coord_cartesian(xlim = c(lo, hi)) +
      ggplot2::labs(x = unname(crit_pretty[key]), y = NULL) +
      ggplot2::theme_minimal(base_size = 11) + no_grid_y + drop_y +
      ggplot2::theme(plot.margin = ggplot2::margin(12, 6, 2, 6))
  }

  crit_plots <- lapply(criteria, crit_bar)
  crit_plots <- crit_plots[!vapply(crit_plots, is.null, logical(1))]

  cowplot::plot_grid(
    plotlist   = c(list(p_comp), crit_plots),
    nrow       = 1, align = "h", axis = "tb",
    rel_widths = c(1.35, rep(1, length(crit_plots)))
  )
}


#' Map panel (b): occurrence records over one biome scheme.
#' @keywords internal
#' @noRd
.biomes_panel_map <- function(x, scheme, biome, lon, lat,
                              legend, legend_counts,
                              point_color, point_size) {

  if (is.null(biome)) {
    stack <- biomes_get()
    ras   <- stack[[as.integer(scheme)]]
    scheme_idx <- as.integer(scheme)
  } else {
    ras <- biome
    scheme_idx <- NA_integer_
  }
  if (terra::nlyr(ras) != 1L) {
    stop("biomes_visualise() expects a single biome scheme layer.",
         call. = FALSE)
  }

  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    checkmate::assert_subset(c(lon, lat), choices = names(x), .var.name = "x")
    keep <- is.finite(x[[lon]]) & is.finite(x[[lat]])
    if (!any(keep)) stop("No records with finite coordinates in `x`.",
                         call. = FALSE)
    pts_sf <- sf::st_as_sf(x[keep, , drop = FALSE],
                           coords = c(lon, lat), crs = 4326)
  } else if (inherits(x, "sf")) {
    pts_sf <- x
  } else {
    pts_sf <- sf::st_as_sf(x)
  }
  pts_proj <- sf::st_transform(pts_sf, sf::st_crs(terra::crs(ras)))
  pts_v  <- terra::project(terra::vect(pts_proj), terra::crs(ras))
  ex_pts <- terra::extract(ras, pts_v)
  raw_pt <- ex_pts[[setdiff(names(ex_pts), "ID")[1]]]

  leg <- biomes::biomes_legend
  cls <- NULL; source_info <- ""
  if (!is.na(scheme_idx)) {
    leg_row <- leg[leg$layer == scheme_idx, , drop = FALSE]
    if (nrow(leg_row) >= 1L) {
      cls <- .layer_lookup(scheme_idx, leg)
      source_info <- as.character(leg_row$source[1])
    }
  }

  ras_vals <- terra::unique(ras)[[1]]
  ras_vals <- ras_vals[!is.na(ras_vals)]
  all_vals <- sort(unique(c(ras_vals, raw_pt[!is.na(raw_pt)])))

  if (is.null(cls)) {
    base_lab <- paste0("raster value: ", all_vals)
  } else {
    base_lab <- cls[all_vals]
    na_lab   <- is.na(base_lab)
    if (any(na_lab)) {
      base_lab[na_lab] <- paste0("azonal (raster value: ", all_vals[na_lab], ")")
    }
  }

  n_per <- vapply(all_vals, function(v) sum(raw_pt == v, na.rm = TRUE),
                  integer(1))
  plot_labels <- if (legend_counts) paste0(base_lab, " (", n_per, ")") else base_lab
  biome_colors <- viridis::viridis(length(all_vals), option = "D")
  names(biome_colors) <- plot_labels

  ras_fac <- ras
  levels(ras_fac) <- data.frame(ID = all_vals, biome = plot_labels)

  title <- if (!is.na(scheme_idx)) {
    sprintf("Biome scheme no. %d%s", scheme_idx,
            if (nzchar(source_info)) paste0(": ", source_info) else "")
  } else "Biome map"

  # shrink the legend when there are many biome classes, so a long legend is
  # not clipped at the top/bottom of the map.
  nclass  <- length(all_vals)
  key_cm  <- if (nclass >= 25) 0.28 else if (nclass >= 15) 0.36 else 0.5
  txt_pt  <- if (nclass >= 25) 6    else if (nclass >= 15) 7.5  else 9

  ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = ras_fac) +
    ggplot2::scale_fill_manual(
      "Biome classes",
      values   = biome_colors,
      breaks   = plot_labels,
      na.value = "transparent",
      drop     = FALSE,
      guide    = if (legend) ggplot2::guide_legend(ncol = 1) else "none"
    ) +
    ggplot2::geom_sf(data = pts_proj, color = point_color,
                     alpha = 1, size = point_size, inherit.aes = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = if (legend) "right" else "none",
      legend.title    = ggplot2::element_text(size = 11, hjust = 0.5),
      legend.text     = ggplot2::element_text(size = txt_pt),
      legend.key.size = ggplot2::unit(key_cm, "cm"),
      plot.title      = ggplot2::element_text(size = 13, face = "bold",
                                              hjust = 0.5)
    ) +
    ggplot2::ggtitle(title)
}


#' Barplot panel (c): records (left) and species (right) per biome class,
#' with the biome-class names centred between the two mirrored bar panels.
#' @keywords internal
#' @noRd
.biomes_panel_barplot <- function(x, scheme, biome, lon, lat) {

  # align the species vector with the classified rows
  species <- NULL
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    if (all(c(lon, lat) %in% names(x))) {
      keep <- is.finite(x[[lon]]) & is.finite(x[[lat]])
      x <- x[keep, , drop = FALSE]
    }
    if ("species" %in% names(x)) species <- as.character(x[["species"]])
  } else if (inherits(x, "sf")) {
    if ("species" %in% names(x)) species <- as.character(x[["species"]])
  } else if (inherits(x, "SpatVector")) {
    xdf <- terra::as.data.frame(x)
    if ("species" %in% names(xdf)) species <- as.character(xdf[["species"]])
  }

  cls_df <- suppressMessages(suppressWarnings(
    biomes_classify(x = x, scheme = scheme, biome = biome,
                    lon = lon, lat = lat, value = "name", append = FALSE)
  ))
  name_col <- grep("_name$", names(cls_df), value = TRUE)[1]
  if (is.na(name_col)) {
    stop("Classification produced no biome-class names for the barplot panel.",
         call. = FALSE)
  }
  biome_cls <- cls_df[[name_col]]

  rec <- as.data.frame(table(biome = biome_cls), stringsAsFactors = FALSE)
  names(rec) <- c("biome", "n_records")
  have_species <- !is.null(species) && length(species) == length(biome_cls)
  if (have_species) {
    sp_tab <- tapply(species, biome_cls, function(s) {
      s <- s[!is.na(s) & nzchar(s)]; length(unique(s))
    })
    sp <- data.frame(biome = names(sp_tab), n_species = as.integer(sp_tab),
                     stringsAsFactors = FALSE)
    df <- merge(rec, sp, by = "biome", all = TRUE)
  } else {
    df <- rec; df$n_species <- NA_integer_
  }
  df$n_records[is.na(df$n_records)] <- 0L
  df$n_species[is.na(df$n_species)] <- 0L
  df <- df[order(-df$n_records), , drop = FALSE]
  df$biome_f   <- factor(df$biome, levels = rev(df$biome))   # most records on top
  df$biome_lab <- ifelse(df$biome == "no_biome", "No biome", df$biome)

  no_grid_y <- ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                              panel.grid.minor   = ggplot2::element_blank())
  drop_y <- ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank())

  # If there is no species information, fall back to a single records bar
  # with the biome-class names on the y-axis.
  if (!have_species || all(df$n_species == 0L)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$n_records, y = .data$biome_f)) +
      ggplot2::geom_col(fill = "#2C7A7B") +
      ggplot2::geom_text(ggplot2::aes(label = .data$n_records),
                         hjust = -0.15, size = 2.6) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
      ggplot2::scale_y_discrete(labels = stats::setNames(df$biome_lab, df$biome_f)) +
      ggplot2::labs(x = "Number of occurrence records", y = NULL) +
      ggplot2::theme_minimal(base_size = 10) + no_grid_y
    return(p)
  }

  rec_max <- max(df$n_records); sp_max <- max(df$n_species)

  # (left) records, mirrored to the left
  p_rec <- ggplot2::ggplot(df, ggplot2::aes(y = .data$biome_f, x = -.data$n_records)) +
    ggplot2::geom_col(fill = "#2C7A7B", orientation = "y") +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_records), hjust = 1.15,
                       size = 2.5) +
    ggplot2::scale_x_continuous(limits = c(-rec_max * 1.30, 0),
                                labels = function(z) abs(z), expand = c(0, 0)) +
    ggplot2::labs(x = "Occurrence records", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) + no_grid_y + drop_y +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 1, 2, 2))

  # (centre) biome-class names
  p_lab <- ggplot2::ggplot(df, ggplot2::aes(y = .data$biome_f, x = 0)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$biome_lab), size = 2.5) +
    ggplot2::scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 2, 0))

  # (right) species, growing to the right
  p_sp <- ggplot2::ggplot(df, ggplot2::aes(y = .data$biome_f, x = .data$n_species)) +
    ggplot2::geom_col(fill = "#B7791F", orientation = "y") +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_species), hjust = -0.15,
                       size = 2.5) +
    ggplot2::scale_x_continuous(limits = c(0, sp_max * 1.30), expand = c(0, 0)) +
    ggplot2::labs(x = "Species", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) + no_grid_y + drop_y +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 2, 2, 1))

  cowplot::plot_grid(p_rec, p_lab, p_sp, nrow = 1, align = "h", axis = "tb",
                     rel_widths = c(1, 0.9, 1))
}
