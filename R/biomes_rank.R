#' Rank biome schemes for a given occurrence dataset
#'
#' Compares the biome classification layers for a user-supplied set of
#' occurrences and proposes a single "best" layer for that dataset. Each
#' layer is scored on several data-driven criteria that are combined into
#' one `composite_score`, which drives the ranking.
#'
#' By default, three equally weighted criteria are used:
#' \enumerate{
#'   \item \strong{coverage}: fraction of records that the layer places in
#'     a biome at all (the rest fall on unclassified, NA cells).
#'   \item \strong{effective_classes}: \eqn{\exp(H')} (Hill number of
#'     order 1), i.e. the effective number of biomes the records spread
#'     across, weighted by evenness.
#'   \item \strong{granularity}: biome classes actually used, divided by
#'     the classes available in the layer.
#' }
#'
#' Two further criteria can be requested via `criteria`:
#' \itemize{
#'   \item \strong{informativeness}: Pielou's evenness
#'     \eqn{J' = H' / \log(k_{used})}.
#'   \item \strong{agreement}: mean pairwise Cohen's \eqn{\kappa} against
#'     the other layers (Monserud & Leemans 1992).
#' }
#'
#' All raw scores are min-max scaled to \eqn{[0, 1]} across the compared
#' layers and averaged into the `composite_score`. Layers are then ordered
#' by this score and ties resolved according to `tiebreaker`.
#'
#' @note
#' `biomes_rank()` gives a data-driven ranking, not an authoritative
#' "best" classification. The criteria favour layers that cover your
#' records and split them into many, evenly-used classes, but the
#' top-ranked layer is not necessarily the most suitable one for your
#' question. For best results, narrow the comparison to a meaningful
#' group via `scheme_type`, and treat the ranking as a shortlist rather
#' than a verdict: inspect the per-criterion columns in the result and
#' use [biomes_info()] to choose the layer whose definition and
#' resolution actually match your data.
#'
#' @param x A data frame with longitude / latitude columns, an `sf`
#'   spatial object, or a `terra::SpatVector` of point geometries.
#' @param scheme Optional integer vector in `1:31` (biome scheme numbers)
#'   to restrict the ranking to a subset of the packaged schemes (e.g.
#'   `scheme = c(1, 5, 25)`). `NULL` (default) ranks all 31 schemes.
#'   Ignored when `biome` is supplied.
#' @param biome Optional `terra::SpatRaster` stack of biome schemes. Use
#'   this for custom rasters; for the packaged stack prefer
#'   `scheme = <int>` instead.
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
#'   classes ranks higher), or `"none"` (do not break ties; tied layers
#'   share a rank, dense ranking). With `"year"` and `"classes"` the
#'   other key serves as a further fallback, alphabetical `scheme_name`
#'   resolves any remaining ties, and ranks are strict 1..N. With
#'   `"none"` multiple layers may carry `is_best = TRUE`.
#' @param verbose Logical. Print progress messages? Default `TRUE`.
#'
#' @return A data frame of classes `biomes_rank` and `data.frame`, with
#'   one row per compared biome scheme. Columns: `scheme` (the biome
#'   scheme number, 1-31), `scheme_name`, `year` (publication year of
#'   the scheme), `n_total`, `n_hit` and `n_na` (number of records in
#'   total, classified, and unclassified), `pct_na` (percentage of
#'   unclassified records), then one `*_raw` and one `*_scaled` column
#'   per requested criterion (the raw score and its min-max scaled
#'   version), `composite_score` (mean of the scaled criteria, drives
#'   the ranking), `rank` (1 = best), and `is_best` (`TRUE` for the
#'   top-ranked scheme). The result carries the attributes `criteria`,
#'   `tiebreaker`, `scheme_type`, and `best_scheme` (the biome scheme
#'   number of the top-ranked scheme, ready to be used as the `scheme`
#'   argument of [biomes_classify()] or [biomes_full()]).
#'
#' @examples
#' data("biomes_example")
#'
#' \donttest{
#' # Ranks layers of the biome raster (~36 MB), downloaded on first use.
#'
#' # Default call: coverage + effective_classes + granularity, equally weighted
#' r <- biomes_rank(biomes_example, verbose = FALSE)
#' head(r)
#' attr(r, "best_scheme")
#'
#' # Restrict to a subset of criteria
#' r2 <- biomes_rank(
#'   biomes_example,
#'   criteria = c("coverage", "effective_classes"),
#'   verbose  = FALSE
#' )
#' }
#'
#' @export
biomes_rank <- function(
    x,
    scheme     = NULL,
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
  if (!is.null(scheme)) {
    checkmate::assert_integerish(scheme, lower = 1L, upper = 31L,
                                 any.missing = FALSE, min.len = 1L,
                                 .var.name = "scheme")
    if (!is.null(biome)) {
      warning("`scheme` is ignored because `biome` was supplied.",
              call. = FALSE)
    }
  }

  # ---- scheme_type: restrict to one methodological group of schemes -------
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
        stop("No schemes found for scheme_type = '", scheme_type, "'.",
             call. = FALSE)
      }
      scheme <- if (is.null(scheme)) type_layers else intersect(scheme, type_layers)
      if (length(scheme) == 0L) {
        stop("`scheme` and `scheme_type` together select no schemes.",
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
                       " record(s) against biome schemes ...")
  ids <- suppressMessages(suppressWarnings(
    biomes_classify(x, scheme = scheme, biome = biome,
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
    scheme      = layer_idx,
    scheme_name = info$layer_name,
    year        = info$year,
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
  best_scheme <- out$scheme[out$is_best][1]

  attr(out, "criteria")    <- criteria
  attr(out, "tiebreaker")  <- tiebreaker
  attr(out, "best_scheme") <- best_scheme
  attr(out, "scheme_type") <- scheme_type
  class(out) <- c("biomes_rank", "data.frame")

  if (verbose) {
    message(sprintf(
      "Best scheme: %s, %s (composite = %.3f)",
      best_scheme,
      out$scheme_name[out$is_best][1],
      out$composite_score[out$is_best][1]
    ))
  }
  out
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
      df$scheme_name[non_na],
      na.last = TRUE
    )
  } else {  # "classes"
    sort_idx <- order(
      -df$composite_score[non_na],
      -cls[non_na],
      -df$year[non_na],
      df$scheme_name[non_na],
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
#' passed a custom raster (legend unknown); fall back to NA there.
#'
#' @keywords internal
#' @noRd
.total_classes_from_df <- function(df) {
  k <- df$scheme
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
    scheme = integer(), scheme_name = character(), year = integer(),
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
  attr(base, "best_scheme") <- NA_integer_
  class(base) <- c("biomes_rank", "data.frame")
  base
}
