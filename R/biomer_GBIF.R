#' Download and clean GBIF occurrences for a taxon
#'
#' Downloads GBIF occurrence data using the GBIF API, filters by year range,
#' limits number of records, applies coordinate cleaning, and stores citation.
#'
#' @param taxon Scientific name of a taxon (e.g., species, genus, family, etc.).
#' @param username GBIF username.
#' @param pwd GBIF password.
#' @param email Email address associated with the GBIF account.
#' @param save_dir Directory to store GBIF download and citation (default = "data/GBIF").
#' @param filter_clean Logical; if TRUE, cleans coordinates using CoordinateCleaner.
#' @param filter_sea Logical; if TRUE, filters sea coordinates (default = FALSE).
#' @param year_min Optional integer. Minimum year to include.
#' @param year_max Optional integer. Maximum year to include.
#'
#' @return A table with (optionally cleaned) GBIF occurrence records.
#'
#' @examples
#' data <- biomer_GBIF(taxon = "Solemyida",
#'                     username = "XXX",
#'                     pwd = "XXX",
#'                     email = "XXX",
#'                     save_dir = "test_environment/data",
#'                     filter_Sclean = TRUE,
#'                     filter_sea = FALSE,
#'                     year_min = 2000,
#'                     year_max = 2024)
#' @export
#'
#' @importFrom rgbif name_backbone occ_download occ_download_wait occ_download_get occ_download_import pred pred_gte pred_lte gbif_citation
#' @importFrom CoordinateCleaner clean_coordinates
#' @importFrom dplyr filter slice_head mutate
#' @importFrom readr read_tsv write_csv
#' @importFrom utils unzip write.table
#' @importFrom countrycode countrycode

biomer_GBIF <- function(
    taxon,
    username,
    pwd,
    email,
    save_dir = "data/GBIF",
    filter_clean = TRUE,
    filter_sea = FALSE,
    year_min = NULL,
    year_max = NULL
) {
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

  # Get taxonKey
  name_info <- rgbif::name_backbone(name = taxon)
  if (is.null(name_info$usageKey)) stop("❌ Could not find taxonKey.")
  taxonKey <- name_info$usageKey
  rank <- name_info$rank
  message("→ Found taxon: ", name_info$scientificName, " (rank: ", rank, ")")

  # Build predicates
  pred_list <- list(
    rgbif::pred("taxonKey", taxonKey),
    rgbif::pred("hasCoordinate", TRUE),
    rgbif::pred("hasGeospatialIssue", FALSE)
  )
  if (!is.null(year_min)) pred_list <- append(pred_list, list(rgbif::pred_gte("year", year_min)))
  if (!is.null(year_max)) pred_list <- append(pred_list, list(rgbif::pred_lte("year", year_max)))

  # Request download
  message("→ Downloading GBIF data...")
  dl <- do.call(rgbif::occ_download, c(
    pred_list,
    list(format = "SIMPLE_CSV", user = username, pwd = pwd, email = email)
  ))

  # Wait for download
  rgbif::occ_download_wait(dl)
  dl_key <- dl[[1]]

  # Save citation
  message("→ Saving GBIF citation...")
  cit <- rgbif::gbif_citation(dl_key)
  cit_text <- capture.output(print(cit))
  writeLines(cit_text, file.path(save_dir, paste0(dl_key, "_citation.txt")))

  # Download ZIP + extract
  zip_path <- file.path(save_dir, paste0(dl_key, ".zip"))
  rgbif::occ_download_get(key = dl_key, overwrite = TRUE, path = save_dir)
  unzipped_files <- unzip(zipfile = zip_path, exdir = save_dir)
  csv_file <- unzipped_files[grepl("\\.csv$", unzipped_files)][1]

  # Import data
  occ_data <- readr::read_tsv(csv_file, guess_max = 100000)
  message("→ Done. Returned ", nrow(occ_data), " occurrence records.")

  # Coordinate cleaning
  if (filter_clean) {
    message("→ Cleaning coordinates...")

    occ_data <- dplyr::filter(occ_data, !is.na(decimalLatitude), !is.na(decimalLongitude))

    cc_tests <- if (filter_sea) {
      c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "zeros", "seas")
    } else {
      c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "zeros")
    }

    flags <- CoordinateCleaner::clean_coordinates(
      x = occ_data,
      lat = "decimalLatitude",
      lon = "decimalLongitude",
      countries = "countryCode",
      species = "species",
      tests = cc_tests
    )

    occ_data <- occ_data[flags$.summary, ]

    # Save filtered file
    filtered_path <- sub("\\.csv$", "_filtered.csv", csv_file)
    readr::write_csv(occ_data, filtered_path)
    message("→ Saved filtered data to: ", normalizePath(filtered_path))
  }

  return(occ_data)
}
