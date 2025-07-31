#'  Occurrence records by biome
#'
#' Adds biome ID, cell ID and biome name for each point from the selected biome layer.
#' Also creates a summary table of occurrences per species and biome.
#'
#' @param df Data frame with at least longitude, latitude and taxon columns.
#' @param species_col Column name for species (or other grouping).
#' @param lon_col Name of longitude column. Default: "decimalLongitude"
#' @param lat_col Name of latitude column. Default: "decimalLatitude"
#' @param biome_layer Integer indicating raster layer to use (1–31)
#' @param raster_path Path to raster stack (default from package)
#' @param legend_path Path to legend file (default from package)
#' @param save_path Path to export CSVs (without extension). If NULL, nothing is saved.
#' @param include_summary Logical; whether to generate and export a summary table.
#'
#' @return Adds two tables: df_biom and summary_table
#'
#' @examples
#' biomer_count(df = data,
#'              species_col = "species",
#'              biome_layer = 1,
#'              lon_col = "decimalLongitude",
#'              lat_col = "decimalLatitude",
#'              include_summary = TRUE,
#'              save_path = NULL)
#' @export
#'
#' @importFrom terra rast crs vect project extract cellFromXY
#' @importFrom dplyr left_join group_by summarise %>%
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_match str_match_all str_trim
#' @importFrom utils write.csv
#' @importFrom tools file_path_sans_ext

biomer_count <- function(df,
                         species_col = "species",
                         lon_col = "decimalLongitude",
                         lat_col = "decimalLatitude",
                         biome_layer = 1,
                         raster_path = system.file("extdata", "Biome_Inventory_RasterStack.tif", package = "biomer"),
                         legend_path = system.file("extdata", "Biome_Inventory_Legends.txt", package = "biomer"),
                         save_path = NULL,
                         include_summary = TRUE) {

  df_name <- deparse(substitute(df))
  df_biom_name <- paste0(df_name, "_biom")

  # Load raster and select layer
  r_stack <- terra::rast(raster_path)
  r_biome <- r_stack[[biome_layer]]

  # Convert to spatial points and project
  points_wgs <- terra::vect(df, geom = c(lon_col, lat_col), crs = "EPSG:4326")
  points_proj <- terra::project(points_wgs, terra::crs(r_biome))

  # Extract biome cell and biome ID
  df[[paste0("biome_cell_", biome_layer)]] <- terra::cellFromXY(r_biome, terra::geom(points_proj)[, c("x", "y")])
  df[[paste0("biome_id_", biome_layer)]] <- terra::extract(r_biome, points_proj)[, 2]

  # Parse legend
  legend_text <- paste(readLines(legend_path, encoding = "latin1"), collapse = "\n")
  pattern <- sprintf("Biome Inventory layer %02d.*?:\\s*(.+?)(?=(\\n|\\r\\n)Biome Inventory layer|$)", biome_layer)
  match <- stringr::str_match(legend_text, pattern)[, 2]
  if (is.na(match)) stop("could not parse legend for biome_layer = ", biome_layer)

  biome_defs <- stringr::str_match_all(match, "(\\d+),\\s*([^;]+)")
  biome_id_col <- paste0("biome_id_", biome_layer)
  biome_name_col <- paste0("biome_name_", biome_layer)

  legend_df <- data.frame(
    biome_id = as.integer(biome_defs[[1]][, 2]),
    biome_name = stringr::str_trim(biome_defs[[1]][, 3]),
    stringsAsFactors = FALSE
  )

  colnames(legend_df) <- c(biome_id_col, biome_name_col)


  # Join biome name
  df <- dplyr::left_join(df, legend_df, by = paste0("biome_id_", biome_layer))

  # Save or return full table
  assign(df_biom_name, df, envir = .GlobalEnv)

  # Save annotated table if requested
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    base <- tools::file_path_sans_ext(save_path)
    utils::write.csv(df, paste0(base, "_biom.csv"), row.names = FALSE)
  }

  # Create and export summary table
  if (include_summary) {
    biome_name_col <- paste0("biome_name_", biome_layer)
    summary_tab <- df %>%
      dplyr::group_by(.data[[species_col]], .data[[biome_name_col]]) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = biome_name_col, values_from = n, values_fill = 0)

    summary_tab$count <- rowSums(summary_tab[,-1])
    summary_tab <- summary_tab[, c(1, ncol(summary_tab), 2:(ncol(summary_tab)-1))]

    assign("summary_table", summary_tab, envir = .GlobalEnv)

    if (!is.null(save_path)) {
      utils::write.csv(summary_tab, paste0(base, "_summary.csv"), row.names = FALSE)
    }
  }

  invisible(NULL)
}
