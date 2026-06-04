#' Download and clean GBIF occurrences for a taxon
#'
#' Retrieves occurrence records for a given taxon (species, genus, family,
#' ...) from GBIF and, optionally, runs standard coordinate cleaning with
#' [CoordinateCleaner::clean_coordinates()].
#'
#' By default, `biomes_occ()` first asks GBIF how many records exist for
#' the taxon and then prompts the user (in interactive sessions):
#'
#' 1. Use `rgbif::occ_search()`? (no login required, capped at 100,000
#'    records.) If yes, the user is then asked for the number of records.
#'    If no, the function switches to `rgbif::occ_download()`, which
#'    needs a save directory and GBIF credentials and downloads
#'    everything (you get a DOI for citation).
#'
#' The only GBIF predicate applied is `hasCoordinate = TRUE`. The result
#' is slim by default: `family`, `genus`, `species`, `year`,
#' `countryCode`, `decimalLongitude`, `decimalLatitude`. Set
#' `slim = FALSE` to keep every GBIF column. With `occ_download()` the
#' downloaded data and the GBIF citation are written to `save_dir`.
#'
#' @param taxon Scientific name(s) to query (species, genus, family, ...).
#'   Accepts a single name or a character vector of names. All matching
#'   keys are bundled into one `occ_download()` job; the `occ_search()`
#'   path loops over the taxa and splits the user-requested number of
#'   records evenly across them.
#' @param use_download Logical. Force the GBIF download workflow even if
#'   the total record count is below 100,000. Default: `FALSE`.
#' @param username GBIF username (used when the download workflow is
#'   triggered, either via `use_download = TRUE` or by the interactive
#'   prompt). If `NULL`, the user is asked at the console.
#' @param pwd GBIF password (same logic as `username`).
#' @param email GBIF account email (same logic as `username`).
#' @param save_dir Directory used for outputs when `occ_download()` is
#'   triggered (both the data CSV and a `*_citation.txt`). If `NULL`
#'   (default), the user is asked at the console; an empty answer falls
#'   back to the current working directory.
#' @param filter_clean Logical. If `TRUE`, run basic spatial cleaning on
#'   coordinates with [CoordinateCleaner::clean_coordinates()].
#'   Default: `TRUE`.
#' @param filter_sea Logical. If `TRUE`, also flag occurrences in the sea
#'   (`"seas"` test). Default: `FALSE`.
#' @param year_min Optional integer. If supplied, only records with
#'   `year >= year_min` are kept on the GBIF side. `NULL` (default)
#'   means no year filter.
#' @param year_max Optional integer. Same as `year_min` but for the
#'   upper bound.
#' @param country Optional character vector. One or more ISO 3166-1
#'   alpha-2 country codes (only used in the download workflow).
#' @param limit Optional integer. Number of records to download. When
#'   `NULL` (default), the user is asked interactively in the
#'   `occ_search()` path; the `occ_download()` path always returns all
#'   available records.
#' @param slim Logical. If `TRUE` (default), the result is trimmed to
#'   `family, genus, species, year, countryCode, decimalLongitude,
#'   decimalLatitude`. Set to `FALSE` to keep all GBIF columns.
#'
#' @return A data frame of GBIF occurrence records, optionally cleaned.
#'   Always contains `decimalLongitude` and `decimalLatitude` columns
#'   (when records are returned), so the result can be passed directly to
#'   [biomes_classify()], [biomes_rank()] or [biomes_full()].
#'
#' @examples
#' \dontrun{
#' # interactive: prompted for occ_search vs occ_download
#' occ <- biomes_occ(taxon = "Solemyida")
#'
#' # force the GBIF download workflow up front (requires credentials)
#' occ <- biomes_occ(
#'   taxon        = "Fagus sylvatica",
#'   use_download = TRUE,
#'   username     = "xxx",
#'   pwd          = "xxx",
#'   email        = "you@example.org",
#'   save_dir     = "data/GBIF"
#' )
#' }
#'
#' @export
biomes_occ <- function(
    taxon,
    use_download = FALSE,
    username     = NULL,
    pwd          = NULL,
    email        = NULL,
    save_dir     = NULL,
    filter_clean = TRUE,
    filter_sea   = FALSE,
    year_min     = NULL,
    year_max     = NULL,
    country      = NULL,
    limit        = NULL,
    slim         = TRUE
) {

  # ---------------------------------------------------------------- input
  checkmate::assert_character(taxon, any.missing = FALSE, min.chars = 1L,
                              min.len = 1L)
  taxon <- unique(trimws(taxon))
  checkmate::assert_flag(use_download)
  checkmate::assert_flag(filter_clean)
  checkmate::assert_flag(filter_sea)
  checkmate::assert_flag(slim)
  checkmate::assert_string(save_dir, null.ok = TRUE)
  checkmate::assert_int(year_min, null.ok = TRUE)
  checkmate::assert_int(year_max, null.ok = TRUE)
  checkmate::assert_character(country, null.ok = TRUE,
                              any.missing = FALSE, min.len = 1L)
  checkmate::assert_int(limit, lower = 1L, null.ok = TRUE)

  # --------------------------------------------------------- soft deps
  if (!requireNamespace("rgbif", quietly = TRUE)) {
    stop("Package 'rgbif' is required for biomes_occ(). ",
         "Install it with install.packages('rgbif').", call. = FALSE)
  }
  if (filter_clean && !requireNamespace("CoordinateCleaner", quietly = TRUE)) {
    stop("Package 'CoordinateCleaner' is required when filter_clean = TRUE. ",
         "Install it with install.packages('CoordinateCleaner').",
         call. = FALSE)
  }

  # -------------------------------------------------------- taxon keys
  message(sprintf("Resolving %d taxon name(s) from GBIF backbone ...",
                  length(taxon)))
  resolved <- lapply(taxon, function(nm) {
    info <- tryCatch(rgbif::name_backbone(name = nm),
                     error = function(e) NULL)
    if (is.null(info) || is.null(info$usageKey)) {
      return(list(name = nm, key = NA_integer_,
                  scientific = NA_character_, rank = NA_character_))
    }
    list(name       = nm,
         key        = as.integer(info$usageKey),
         scientific = as.character(info$scientificName),
         rank       = as.character(info$rank))
  })
  taxon_keys <- vapply(resolved, `[[`, integer(1),  "key")
  bad <- is.na(taxon_keys)
  if (any(bad)) {
    stop("GBIF could not resolve: ",
         paste(taxon[bad], collapse = ", "), call. = FALSE)
  }
  for (r in resolved) {
    message(sprintf("  - %s -> %s (rank: %s, key: %s)",
                    r$name, r$scientific, r$rank, r$key))
  }

  # -------------------------------------------------------- year filter
  # Only applied when the user explicitly passed year_min/year_max.
  year_str <- NULL
  if (!is.null(year_min) && !is.null(year_max)) {
    year_str <- sprintf("%d,%d", as.integer(year_min), as.integer(year_max))
  } else if (!is.null(year_min)) {
    year_str <- sprintf("%d,3000", as.integer(year_min))
  } else if (!is.null(year_max)) {
    year_str <- sprintf("0,%d", as.integer(year_max))
  }

  # -------------------------------------------------------- count check
  if (!use_download) {
    message("Asking GBIF how many records are available ...")
    counts <- vapply(taxon_keys, function(k) {
      peek <- tryCatch(
        rgbif::occ_search(taxonKey = k, hasCoordinate = TRUE,
                          year = year_str, limit = 1L),
        error = function(e) NULL
      )
      n <- if (is.null(peek)) NA_integer_ else {
        tryCatch(as.integer(peek$meta$count), error = function(e) NA_integer_)
      }
      if (is.na(n)) 0L else n
    }, integer(1))
    if (length(taxon_keys) > 1L) {
      for (i in seq_along(taxon_keys)) {
        message(sprintf("  - %s: %d records", taxon[i], counts[i]))
      }
    }
    n_avail <- sum(counts)
    message(sprintf("Total available: %d records.", n_avail))

    if (n_avail == 0L) {
      message("No records returned by occ_search().")
      return(data.frame())
    }

    # Step 1: backend (always asked in interactive sessions).
    if (interactive()) {
      use_search <- .prompt_use_search(n_avail)
      if (is.null(use_search)) {
        message("Aborted by user.")
        return(invisible(data.frame()))
      }
      use_download <- !use_search
    } else if (n_avail > 100000L) {
      message("Non-interactive session: defaulting to occ_search() ",
              "capped at 100,000 records. Pass use_download = TRUE + ",
              "credentials to download everything.")
    }

    # Step 2: how many records (occ_search path only; occ_download
    # always returns the full set).
    if (!use_download && is.null(limit)) {
      if (interactive()) {
        limit <- .prompt_search_amount(n_avail)
        if (is.null(limit)) {
          message("Aborted by user.")
          return(invisible(data.frame()))
        }
      } else {
        limit <- min(n_avail, 100000L)
      }
    }
    if (!use_download && !is.null(limit) && limit > 100000L) {
      message(sprintf(
        "Capping occ_search() at 100,000 records (asked for %d).", limit))
      limit <- 100000L
    }
  }

  # -------------------------------------------------------- download path
  # Interactive prompt: save directory, then credentials.
  if (use_download) {
    if (is.null(save_dir)) {
      if (interactive()) {
        save_dir <- .prompt_save_dir()
      } else {
        save_dir <- getwd()
      }
    }
    save_dir <- normalizePath(save_dir, winslash = "/", mustWork = FALSE)
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    message(sprintf("Saving outputs to: %s", save_dir))

    creds <- .get_gbif_credentials(username, pwd, email)
    username <- creds$username; pwd <- creds$pwd; email <- creds$email
  }

  # -------------------------------------------------------- fetch
  cit_obj <- NULL
  if (!use_download) {
    # Distribute the user-requested total evenly across the taxa.
    # Each taxon is capped at 100,000 (occ_search hard cap) and at its
    # own availability.
    per_taxon <- min(100000L,
                     as.integer(ceiling(limit / length(taxon_keys))))
    message(sprintf(
      "Querying GBIF via occ_search() for up to %d records per taxon (%d taxa) ...",
      per_taxon, length(taxon_keys)))

    occ_list <- lapply(seq_along(taxon_keys), function(i) {
      .gbif_stdout <- utils::capture.output(
        res <- rgbif::occ_search(
          taxonKey      = taxon_keys[i],
          hasCoordinate = TRUE,
          year          = year_str,
          limit         = per_taxon
        ),
        type = "output"
      )
      res$data
    })
    # rbind, but be defensive if columns differ across taxa.
    occ_list <- Filter(function(d) !is.null(d) && nrow(d) > 0L, occ_list)
    if (length(occ_list) == 0L) {
      message("No records returned by occ_search().")
      return(data.frame())
    }
    occ_data <- .row_bind_compat(occ_list)
  } else {
    message("Requesting GBIF download via occ_download() ...")
    taxon_pred <- if (length(taxon_keys) == 1L) {
      rgbif::pred("taxonKey", taxon_keys)
    } else {
      rgbif::pred_in("taxonKey", taxon_keys)
    }
    preds <- list(taxon_pred,
                  rgbif::pred("hasCoordinate", TRUE))
    if (!is.null(year_min)) {
      preds <- append(preds, list(rgbif::pred_gte("year", as.integer(year_min))))
    }
    if (!is.null(year_max)) {
      preds <- append(preds, list(rgbif::pred_lte("year", as.integer(year_max))))
    }
    if (!is.null(country)) {
      preds <- if (length(country) > 1L) {
        append(preds, list(rgbif::pred_in("country", country)))
      } else {
        append(preds, list(rgbif::pred("country", country)))
      }
    }

    dl <- do.call(rgbif::occ_download, c(
      preds,
      list(format = "SIMPLE_CSV", user = username, pwd = pwd, email = email)
    ))
    rgbif::occ_download_wait(dl)
    dl_key <- dl[[1]]

    cit_obj <- tryCatch(rgbif::gbif_citation(dl_key),
                        error = function(e) NULL)
    if (!is.null(cit_obj)) {
      cit_text <- utils::capture.output(print(cit_obj))
      writeLines(cit_text,
                 file.path(save_dir, paste0(dl_key, "_citation.txt")))
      message(sprintf("Citation saved to: %s",
                      file.path(save_dir, paste0(dl_key, "_citation.txt"))))
    }

    message("Fetching and unpacking GBIF download ...")
    rgbif::occ_download_get(key = dl_key, overwrite = TRUE, path = save_dir)
    zip_path <- file.path(save_dir, paste0(dl_key, ".zip"))
    files    <- utils::unzip(zipfile = zip_path, exdir = save_dir)
    csv_file <- files[grepl("\\.csv$", files)][1]
    if (length(csv_file) == 0L || is.na(csv_file)) {
      stop("Could not locate CSV in the GBIF download archive.",
           call. = FALSE)
    }
    occ_data <- readr::read_tsv(csv_file, guess_max = 100000,
                                show_col_types = FALSE)
    # Honour an explicit user-supplied `limit` even in the download
    # path (otherwise return all records, which is the typical use).
    if (!is.null(limit) && nrow(occ_data) > limit) {
      message(sprintf("Truncating download to first %d of %d records.",
                      limit, nrow(occ_data)))
      occ_data <- occ_data[seq_len(limit), , drop = FALSE]
    }
  }

  message(sprintf("Retrieved %d occurrence records.", nrow(occ_data)))

  # -------------------------------------------------------- cleaning
  if (filter_clean) {
    message("Cleaning coordinates with CoordinateCleaner ...")
    keep <- !is.na(occ_data[["decimalLatitude"]]) &
            !is.na(occ_data[["decimalLongitude"]])
    occ_data <- occ_data[keep, , drop = FALSE]

    if (nrow(occ_data) == 0L) {
      warning("No records with finite coordinates remain after NA filter.",
              call. = FALSE)
      return(occ_data)
    }

    cc_tests <- c("capitals", "centroids", "equal", "gbif",
                  "institutions", "outliers", "zeros")
    if (filter_sea) cc_tests <- c(cc_tests, "seas")

    flags <- CoordinateCleaner::clean_coordinates(
      x         = occ_data,
      lat       = "decimalLatitude",
      lon       = "decimalLongitude",
      countries = if ("countryCode" %in% names(occ_data)) "countryCode" else NULL,
      species   = if ("species" %in% names(occ_data)) "species" else NULL,
      tests     = cc_tests
    )
    occ_data <- occ_data[flags$.summary, , drop = FALSE]
    message(sprintf("Kept %d records after coordinate cleaning.",
                    nrow(occ_data)))
  }

  # -------------------------------------------------------- slim columns
  if (slim) {
    keep_cols <- c("family", "genus", "species", "year", "countryCode",
                   "decimalLongitude", "decimalLatitude")
    have      <- intersect(keep_cols, names(occ_data))
    occ_data  <- occ_data[, have, drop = FALSE]
  }

  # -------------------------------------------------------- save data
  if (use_download && !is.null(save_dir)) {
    out_csv <- file.path(save_dir, "gbif_occurrences.csv")
    readr::write_csv(occ_data, out_csv)
    message(sprintf("Saved data to: %s", out_csv))
  }

  # -------------------------------------------------------- citation
  if (!is.null(cit_obj)) {
    message("\nGBIF citation (cite this download in publications):")
    print(cit_obj)
  }

  occ_data
}


#' Interactive prompt: download all records via occ_download (yes/no).
#'
#' Returns `TRUE`  to use occ_search (no login, max 100k),
#'         `FALSE` to switch to occ_download (login required),
#'         `NULL`  if the user cancels (Esc / 0 / empty selection).
#'
#' @keywords internal
#' @noRd
.prompt_use_search <- function(n_avail) {
  choice <- utils::menu(
    choices = c(
      "Yes  (occ_download() -- login data needed)",
      "No   (occ_search()   -- no login, but records max. 100,000)"
    ),
    title = sprintf("Download all %d records?", n_avail)
  )
  switch(as.character(choice),
         "1" = FALSE,  # yes, download all -> use occ_download
         "2" = TRUE,   # no, keep it small -> use occ_search
         NULL)
}


#' Interactive prompt: number of records for occ_search.
#'
#' Returns a positive integer (max 100,000) or `NULL` if invalid / abort.
#'
#' @keywords internal
#' @noRd
.prompt_search_amount <- function(n_avail) {
  cap <- min(n_avail, 100000L)
  choice <- utils::menu(
    choices = c(
      sprintf("All %d available records", cap),
      "Custom number"
    ),
    title = "How many records do you want to download?"
  )
  if (identical(as.character(choice), "1")) return(cap)
  if (!identical(as.character(choice), "2")) return(NULL)

  message(sprintf("Maximum available: %d records.", cap))
  raw <- readline("Number of records to download: ")
  n   <- suppressWarnings(as.integer(trimws(raw)))
  if (is.na(n) || n < 1L) {
    message("Invalid number; aborting.")
    return(NULL)
  }
  if (n > cap) {
    message(sprintf("Capping at %d (maximum available).", cap))
    n <- cap
  }
  n
}


#' Interactive prompt: save directory (default = current working dir).
#'
#' @keywords internal
#' @noRd
.prompt_save_dir <- function() {
  default_dir <- getwd()
  raw <- readline(sprintf("Save directory [%s]: ", default_dir))
  raw <- trimws(raw)
  if (!nzchar(raw)) default_dir else raw
}


#' rbind a list of data frames that may have different column sets.
#'
#' rgbif returns tibbles with per-taxon column sets that can differ
#' (e.g. some taxa carry `infraspecificEpithet`, others don't). Use
#' `dplyr::bind_rows` if available, otherwise fall back to a union-of-
#' columns rbind.
#'
#' @keywords internal
#' @noRd
.row_bind_compat <- function(lst) {
  if (length(lst) == 1L) return(lst[[1]])
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(dplyr::bind_rows(lst))
  }
  cols <- unique(unlist(lapply(lst, names)))
  filled <- lapply(lst, function(d) {
    missing <- setdiff(cols, names(d))
    if (length(missing) > 0L) d[, missing] <- NA
    d[, cols, drop = FALSE]
  })
  do.call(rbind, filled)
}


#' Collect GBIF credentials, asking at the console if needed.
#'
#' @keywords internal
#' @noRd
.get_gbif_credentials <- function(username, pwd, email) {
  ask <- function(prompt, hide = FALSE) {
    if (!interactive()) {
      stop("GBIF credentials are required for the download workflow ",
           "but the session is non-interactive. Pass username, pwd ",
           "and email explicitly.", call. = FALSE)
    }
    if (hide && requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      val <- rstudioapi::askForPassword(prompt)
    } else {
      val <- readline(prompt)
    }
    val <- trimws(val)
    if (!nzchar(val)) stop("Empty value for: ", prompt, call. = FALSE)
    val
  }
  if (is.null(username) || !nzchar(username)) username <- ask("GBIF username: ")
  if (is.null(pwd)      || !nzchar(pwd))      pwd      <- ask("GBIF password: ", hide = TRUE)
  if (is.null(email)    || !nzchar(email))    email    <- ask("GBIF email: ")
  list(username = username, pwd = pwd, email = email)
}
