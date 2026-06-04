#' Rank biome layers for a given occurrence dataset
#'
#' Scores the 31 biome layers shipped with the package for a user-provided
#' set of occurrences and proposes a single "best" layer based on the
#' (equal-weight) mean of three data-driven criteria:
#' \enumerate{
#'   \item \strong{coverage}: share of records classified (non-NA).
#'   \item \strong{effective_classes}: \eqn{\exp(H')} (Hill number of
#'     order 1), min-max scaled across layers.
#'   \item \strong{granularity}: classes actually used / classes available
#'     in the layer.
#' }
#' Two additional criteria are available on request via `criteria`:
#' `"informativeness"` (Pielou's evenness \eqn{J' = H' / \log(k_{used})})
#' and `"agreement"` (mean pairwise Cohen's \eqn{\kappa} against the
#' other 30 layers, Monserud & Leemans 1992). All raw scores are min-max
#' scaled to \eqn{[0, 1]} across layers and combined into a
#' `composite_score`. Layers are then ranked by the chosen `tiebreaker`:
#' `"year"` or `"classes"` produce strict ranks 1..N via the chain
#' `composite_score -> <tiebreaker> -> <secondary> -> name`; `"none"`
#' produces dense ranks where layers with identical `composite_score`
#' share a rank.
#'
#' @param x A data frame with longitude / latitude columns, an `sf`
#'   spatial object, or a `terra::SpatVector` of point geometries.
#' @param layer Optional integer vector in `1:31` to restrict the ranking
#'   to a subset of the packaged layers (e.g. `layer = c(1, 5, 25)`).
#'   `NULL` (default) ranks all 31 layers. Ignored when `biome` is
#'   supplied.
#' @param biome Optional `terra::SpatRaster` stack of biome layers. Use
#'   this for custom rasters; for the packaged stack prefer
#'   `layer = <int>` instead.
#' @param lon Column name of longitude in `x` (only used if `x` is a
#'   non-spatial data frame). Default `"decimalLongitude"`.
#' @param lat Column name of latitude in `x` (only used if `x` is a
#'   non-spatial data frame). Default `"decimalLatitude"`.
#' @param scheme_type Character. Restrict the ranking to one methodological
#'   group of biome definitions: one of `"all"` (default; rank all 31
#'   layers), `"climate"`, `"vegetation"`, `"land_cover"`, `"ecoregion"`,
#'   `"integrative"`, or `"anthropogenic"`. The grouping is taken from the
#'   `scheme_type` column of [biomes_information]. When a specific type is
#'   chosen, only the layers of that type are classified, scored and
#'   returned, so the scaled scores and the best layer are determined
#'   within that group. Ignored when `biome` is supplied.
#' @param criteria Character vector with one or more of `"coverage"`,
#'   `"effective_classes"`, `"granularity"`, `"informativeness"`,
#'   `"agreement"`. Default: the first three.
#' @param tiebreaker How tied `composite_score`s are resolved: `"year"`
#'   (default, more recent publication ranks higher), `"classes"` (more
#'   classes ranks higher), or `"none"` (do not break ties — tied layers
#'   share a rank, dense ranking). With `"year"` and `"classes"` the
#'   other key serves as a further fallback, alphabetical `layer_name`
#'   resolves any remaining ties, and ranks are strict 1..N. With
#'   `"none"` multiple layers may carry `is_best = TRUE`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @examples
#' data("biomes_example")
#'
#' # Default call: coverage + effective_classes + granularity, equally weighted
#' r <- biomes_rank(biomes_example, verbose = FALSE)
#' head(r)
#' attr(r, "best_layer")
#'
#' # Restrict to a subset of criteria
#' r2 <- biomes_rank(
#'   biomes_example,
#'   criteria = c("coverage", "effective_classes"),
#'   verbose  = FALSE
#' )
#'
#' @export
biomes_rank <- function(
    x,
    layer      = NULL,
    biome      = NULL,
    lon         = "decimalLongitude",
    lat         = "decimalLatitude",
    scheme_type = "all",
    criteria    = c("coverage", "effective_classes", "granularity"),
    tiebreaker = c("year", "classes", "none"),
    verbose    = TRUE
) {

  # ---------------------------------------------------------------- input
  checkmate::assert_true(
    any(c("data.frame", "sf", "SpatVector") %in% class(x)),
    .var.name = "x"
  )
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    checkmate::assert_subset(c(lon, lat), choices = names(x), .var.name = "x")
    checkmate::assert_numeric(x[[lon]], any.missing = TRUE)
    checkmate::assert_numeric(x[[lat]], any.missing = TRUE)
  }
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

  # ---- scheme_type: restrict to one methodological group of layers --------
  scheme_types <- c("all", "climate", "vegetation", "land_cover",
                    "ecoregion", "integrative", "anthropogenic")
  checkmate::assert_choice(scheme_type, scheme_types, .var.name = "scheme_type")
  if (scheme_type != "all") {
    if (!is.null(biome)) {
      warning("`scheme_type` is ignored because `biome` was supplied.",
              call. = FALSE)
    } else {
      if (!"scheme_type" %in% names(biomes::biomes_information)) {
        stop("biomes_information has no `scheme_type` column; reinstall the ",
             "package to use `scheme_type`.", call. = FALSE)
      }
      type_layers <- which(biomes::biomes_information$scheme_type == scheme_type)
      if (length(type_layers) == 0L) {
        stop("No layers found for scheme_type = '", scheme_type, "'.",
             call. = FALSE)
      }
      layer <- if (is.null(layer)) type_layers else intersect(layer, type_layers)
      if (length(layer) == 0L) {
        stop("`layer` and `scheme_type` together select no layers.",
             call. = FALSE)
      }
    }
  }

  all_criteria <- c("coverage", "effective_classes", "granularity",
                    "informativeness", "agreement")
  checkmate::assert_subset(criteria, choices = all_criteria,
                           empty.ok = FALSE, .var.name = "criteria")
  criteria   <- unique(criteria)
  tiebreaker <- match.arg(tiebreaker)
  checkmate::assert_flag(verbose)

  # rows with NA coords cannot be classified -> drop with a warning
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    bad <- !is.finite(x[[lon]]) | !is.finite(x[[lat]])
    if (any(bad)) {
      warning(sprintf(
        "Dropping %d record(s) with non-finite coordinates.", sum(bad)
      ))
      x <- x[!bad, , drop = FALSE]
    }
  }

  n_total <- if (inherits(x, "data.frame")) nrow(x) else length(x)

  # ---------------------------------------------------------------- empty
  if (n_total == 0) {
    warning("Input has zero usable records; returning an empty ranking.")
    return(.empty_rank(criteria, tiebreaker))
  }

  # ---------------------------------------------------------- classify
  if (verbose) message("Classifying ", n_total,
                       " record(s) against biome layers ...")
  ids <- suppressMessages(suppressWarnings(
    biomes_classify(x, layer = layer, biome = biome,
                    lon = lon, lat = lat,
                    value = "ID", append = FALSE, na = NA)
  ))
  # biomes_classify returns *_value columns
  layer_cols <- names(ids)
  layer_idx  <- suppressWarnings(readr::parse_number(layer_cols))

  use_default_legend <- all(!is.na(layer_idx)) &&
    all(layer_idx >= 1 & layer_idx <= nrow(biomes::biomes_information))

  # layer-level metadata (year, total classes, layer_name)
  info <- .layer_info(layer_idx, use_default_legend)

  # ---------------------------------------------------------- per-layer
  if (verbose) message("Computing per-layer criteria ...")
  per_layer <- lapply(seq_along(layer_cols), function(i) {
    vals <- ids[[i]]
    n_hit <- sum(!is.na(vals))
    n_na  <- n_total - n_hit
    raw <- list(
      coverage          = n_hit / n_total,
      granularity       = NA_real_,
      informativeness   = NA_real_,
      effective_classes = NA_real_,
      agreement         = NA_real_   # filled in below
    )
    if (n_hit > 0) {
      used <- table(vals, useNA = "no")
      k_used <- length(used)
      total_classes <- info$total_classes[i]
      raw$granularity <- if (!is.na(total_classes) && total_classes > 0) {
        min(k_used / total_classes, 1)
      } else {
        NA_real_
      }
      shannon <- .compute_shannon(used)
      raw$informativeness <- if (k_used > 1) shannon / log(k_used) else 0
      raw$effective_classes <- exp(shannon)
    }
    list(
      n_total = n_total,
      n_hit   = n_hit,
      n_na    = n_na,
      raw     = raw
    )
  })

  # ---------------------------------------------------------- agreement
  if ("agreement" %in% criteria) {
    if (verbose) message("Computing pairwise Cohen's kappa across ",
                         length(layer_cols), " layers ...")
    agree <- .compute_pairwise_kappa(ids)
    for (i in seq_along(per_layer)) {
      per_layer[[i]]$raw$agreement <- agree[i]
    }
  }

  # ---------------------------------------------------------- assemble
  raw_mat <- do.call(rbind, lapply(per_layer, function(z) {
    unlist(z$raw[criteria])
  }))
  colnames(raw_mat) <- criteria

  scaled_mat <- apply(raw_mat, 2, .minmax)
  if (is.null(dim(scaled_mat))) {
    # apply collapses to a vector when only 1 row -> reshape
    scaled_mat <- matrix(scaled_mat, nrow = nrow(raw_mat),
                         dimnames = list(NULL, criteria))
  }

  # composite is the equal-weight mean of the available scaled criteria;
  # layers with NA on one criterion are not punished twice.
  composite <- vapply(seq_len(nrow(scaled_mat)), function(i) {
    s <- scaled_mat[i, ]
    ok <- !is.na(s)
    if (!any(ok)) return(NA_real_)
    mean(s[ok])
  }, numeric(1))

  out <- data.frame(
    layer      = layer_idx,
    layer_name = info$layer_name,
    year       = info$year,
    n_total    = vapply(per_layer, `[[`, integer(1), "n_total"),
    n_hit      = vapply(per_layer, `[[`, integer(1), "n_hit"),
    n_na       = vapply(per_layer, `[[`, integer(1), "n_na"),
    stringsAsFactors = FALSE
  )
  out$pct_na <- round(100 * out$n_na / pmax(out$n_total, 1), 2)
  for (cr in criteria) {
    out[[paste0(cr, "_raw")]]    <- raw_mat[, cr]
    out[[paste0(cr, "_scaled")]] <- scaled_mat[, cr]
  }
  out$composite_score <- composite

  # ranks + tiebreaker on rank 1
  out <- .apply_tiebreaker(out, tiebreaker)
  best_layer <- out$layer[out$is_best][1]

  attr(out, "criteria")    <- criteria
  attr(out, "tiebreaker")  <- tiebreaker
  attr(out, "best_layer")  <- best_layer
  attr(out, "scheme_type") <- scheme_type
  class(out) <- c("biomes_rank", "data.frame")

  if (verbose) {
    message(sprintf(
      "Best layer: %s -- %s (composite = %.3f)",
      best_layer,
      out$layer_name[out$is_best][1],
      out$composite_score[out$is_best][1]
    ))
  }
  out
}


#' Visualise a `biomes_rank` result
#'
#' Builds a `ggplot` from the data frame returned by [biomes_rank()].
#'
#' @param ranked A `biomes_rank` object returned by [biomes_rank()].
#' @param type One of `"composite"` (default, horizontal bar plot of
#'   `composite_score` with the top-1 layer highlighted), `"na"`
#'   (percent-NA per layer, sorted ascending), or `"criteria"` (heatmap
#'   of all scaled criteria per layer).
#'
#' @examples
#' data("biomes_example")
#' r <- biomes_rank(biomes_example, verbose = FALSE)
#' biomes_show_rank(r, type = "composite")
#' biomes_show_rank(r, type = "na")
#' biomes_show_rank(r, type = "criteria")
#'
#' @export
biomes_show_rank <- function(ranked, type = c("composite", "na", "criteria")) {
  checkmate::assert_class(ranked, "biomes_rank")
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for biomes_show_rank(). ",
         "Install it with install.packages('ggplot2').", call. = FALSE)
  }

  df <- as.data.frame(ranked)
  df$layer_id  <- as.character(df$layer)
  df$bar_label <- sprintf("%s (%s%% NA, %s)",
                          df$layer_id,
                          format(df$pct_na, nsmall = 1),
                          ifelse(is.na(df$year), "n/a", df$year))

  if (type == "composite") {
    df <- df[order(df$composite_score, na.last = FALSE), , drop = FALSE]
    df$bar_label <- factor(df$bar_label, levels = df$bar_label)
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$composite_score,
                   y = .data$bar_label,
                   fill = .data$is_best)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(
        values = c(`TRUE` = "#1b9e77", `FALSE` = "grey70"),
        guide  = "none"
      ) +
      ggplot2::labs(
        x = "Composite score",
        y = NULL,
        title = "Composite score per biome layer",
        subtitle = sprintf("Best: layer %s -- %s",
                           attr(ranked, "best_layer"),
                           df$layer_name[df$is_best][1])
      ) +
      ggplot2::theme_minimal(base_size = 11)
    return(p)
  }

  if (type == "na") {
    df <- df[order(df$pct_na), , drop = FALSE]
    df$bar_label <- factor(df$bar_label, levels = rev(df$bar_label))
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$pct_na, y = .data$bar_label)
    ) +
      ggplot2::geom_col(fill = "grey55") +
      ggplot2::labs(
        x = "Records not classified (% NA)",
        y = NULL,
        title = "Coverage gap per biome layer"
      ) +
      ggplot2::theme_minimal(base_size = 11)
    return(p)
  }

  # type == "criteria"
  criteria <- attr(ranked, "criteria")
  scaled_cols <- paste0(criteria, "_scaled")
  long <- do.call(rbind, lapply(seq_along(criteria), function(i) {
    data.frame(
      layer_id  = df$layer_id,
      bar_label = df$bar_label,
      criterion = criteria[i],
      value     = df[[scaled_cols[i]]],
      stringsAsFactors = FALSE
    )
  }))
  # order layers by composite for a readable heatmap
  ord <- df$bar_label[order(df$composite_score)]
  long$bar_label <- factor(long$bar_label, levels = ord)
  long$criterion <- factor(long$criterion, levels = criteria)

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$criterion, y = .data$bar_label,
                 fill = .data$value)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(
      low = "#f7fbff", high = "#08306b", na.value = "grey90",
      limits = c(0, 1), name = "Scaled\nscore"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Scaled criterion scores per biome layer"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  p
}


# =====================================================================
# Internal helpers
# =====================================================================

#' Min-max scale a numeric vector to `[0, 1]`.
#'
#' NA values are preserved. If all non-NA values are equal, the result
#' is 1 for non-NA entries (every layer is equally good on this
#' criterion, so we do not punish any of them in the composite).
#'
#' @keywords internal
#' @noRd
.minmax <- function(x) {
  if (all(is.na(x))) return(x)
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (isTRUE(all.equal(mn, mx))) {
    out <- rep(1, length(x))
    out[is.na(x)] <- NA_real_
    return(out)
  }
  (x - mn) / (mx - mn)
}

#' Shannon entropy of a frequency table (natural log).
#'
#' @keywords internal
#' @noRd
.compute_shannon <- function(counts) {
  counts <- counts[counts > 0]
  if (length(counts) == 0) return(0)
  p <- counts / sum(counts)
  -sum(p * log(p))
}

#' Mean pairwise Cohen's kappa per layer across all other layers.
#'
#' For two layers (vectors of class IDs at the same records), kappa is
#' computed on records non-NA in both, with raw class IDs as labels
#' (Monserud & Leemans 1992). Layers whose label space is fully
#' disjoint from another layer's are still scored (kappa close to 0).
#'
#' @param ids Data frame: one column per layer of class IDs (NA = miss).
#' @keywords internal
#' @noRd
.compute_pairwise_kappa <- function(ids) {
  L <- length(ids)
  if (L < 2) return(rep(NA_real_, L))
  K <- matrix(NA_real_, L, L)
  for (i in seq_len(L - 1)) {
    a <- ids[[i]]
    for (j in (i + 1):L) {
      b <- ids[[j]]
      ok <- !is.na(a) & !is.na(b)
      if (!any(ok)) next
      K[i, j] <- K[j, i] <- .kappa_pair(a[ok], b[ok])
    }
  }
  rowMeans(K, na.rm = TRUE)
}

#' Cohen's kappa between two equal-length, complete categorical vectors.
#'
#' @keywords internal
#' @noRd
.kappa_pair <- function(a, b) {
  n <- length(a)
  if (n == 0) return(NA_real_)
  cats <- union(unique(a), unique(b))
  if (length(cats) < 2) {
    # only one label in common -> agreement is trivial
    return(if (all(a == b)) 1 else 0)
  }
  fa <- factor(a, levels = cats)
  fb <- factor(b, levels = cats)
  po <- sum(a == b) / n
  pa <- as.numeric(table(fa)) / n
  pb <- as.numeric(table(fb)) / n
  pe <- sum(pa * pb)
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}

#' Layer-level metadata: layer_name, publication year, class count.
#'
#' @keywords internal
#' @noRd
.layer_info <- function(layer_idx, use_default_legend) {
  layer_name <- rep(NA_character_, length(layer_idx))
  year       <- rep(NA_integer_,   length(layer_idx))
  total_cls  <- rep(NA_integer_,   length(layer_idx))
  if (!use_default_legend) return(list(layer_name = layer_name,
                                       year = year,
                                       total_classes = total_cls))

  info <- biomes::biomes_information
  leg  <- biomes::biomes_legend
  for (i in seq_along(layer_idx)) {
    k <- layer_idx[i]
    if (is.na(k) || k < 1 || k > nrow(info)) next
    layer_name[i] <- info[[k, "name_of_classification"]]
    pub <- info[[k, "publication"]]
    yr  <- suppressWarnings(as.integer(regmatches(
      pub, regexpr("(18|19|20)[0-9]{2}", pub)
    )))
    if (length(yr) == 1 && !is.na(yr)) year[i] <- yr
    leg_row <- leg[k, -c(1, 2), drop = FALSE]
    total_cls[i] <- sum(!is.na(unlist(leg_row)))
  }
  list(layer_name = layer_name, year = year, total_classes = total_cls)
}

#' Assign ranks per the chosen tiebreaker.
#'
#' - `"year"`  : strict 1..N, order chain composite -> year -> classes -> name
#' - `"classes"`: strict 1..N, order chain composite -> classes -> year -> name
#' - `"none"`  : dense ranks, ties on `composite_score` share a rank;
#'                multiple layers may carry `is_best = TRUE`.
#' Layers with NA `composite_score` get NA rank.
#'
#' @keywords internal
#' @noRd
.apply_tiebreaker <- function(df, tiebreaker) {
  cls    <- .total_classes_from_df(df)
  non_na <- !is.na(df$composite_score)

  if (tiebreaker == "none") {
    rank_vec <- rep(NA_integer_, nrow(df))
    ord <- order(-df$composite_score[non_na])
    positions <- which(non_na)[ord]
    scores <- df$composite_score[positions]
    dense_rank <- integer(length(scores))
    current <- 0L
    prev <- NA_real_
    for (k in seq_along(scores)) {
      if (k == 1L || !isTRUE(abs(scores[k] - prev) < 1e-9)) {
        current <- current + 1L
      }
      dense_rank[k] <- current
      prev <- scores[k]
    }
    rank_vec[positions] <- dense_rank
    df$rank    <- rank_vec
    df$is_best <- !is.na(rank_vec) & rank_vec == 1L
    return(df)
  }

  if (tiebreaker == "year") {
    sort_idx <- order(
      -df$composite_score[non_na],
      -df$year[non_na],
      -cls[non_na],
      df$layer_name[non_na],
      na.last = TRUE
    )
  } else {  # "classes"
    sort_idx <- order(
      -df$composite_score[non_na],
      -cls[non_na],
      -df$year[non_na],
      df$layer_name[non_na],
      na.last = TRUE
    )
  }

  rank_vec  <- rep(NA_integer_, nrow(df))
  positions <- which(non_na)[sort_idx]
  rank_vec[positions] <- seq_along(positions)

  df$rank    <- rank_vec
  df$is_best <- !is.na(rank_vec) & rank_vec == 1L
  df
}

#' Compute "total classes" for a ranked data frame, even if the user
#' passed a custom raster (legend unknown) — fall back to NA there.
#'
#' @keywords internal
#' @noRd
.total_classes_from_df <- function(df) {
  k <- df$layer
  leg <- biomes::biomes_legend
  out <- rep(NA_integer_, length(k))
  ok <- !is.na(k) & k >= 1 & k <= nrow(leg)
  if (any(ok)) {
    out[ok] <- vapply(k[ok], function(i) {
      sum(!is.na(unlist(leg[i, -c(1, 2), drop = FALSE])))
    }, integer(1))
  }
  out
}

#' Return an empty `biomes_rank` data frame with the right shape.
#'
#' @keywords internal
#' @noRd
.empty_rank <- function(criteria, tiebreaker) {
  base <- data.frame(
    layer = integer(), layer_name = character(), year = integer(),
    n_total = integer(), n_hit = integer(), n_na = integer(),
    pct_na = numeric(),
    stringsAsFactors = FALSE
  )
  for (cr in criteria) {
    base[[paste0(cr, "_raw")]]    <- numeric()
    base[[paste0(cr, "_scaled")]] <- numeric()
  }
  base$composite_score <- numeric()
  base$rank            <- integer()
  base$is_best         <- logical()
  attr(base, "criteria")   <- criteria
  attr(base, "tiebreaker") <- tiebreaker
  attr(base, "best_layer") <- NA_integer_
  class(base) <- c("biomes_rank", "data.frame")
  base
}
