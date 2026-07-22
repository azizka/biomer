#' @keywords internal
#' @noRd
info_grabber <- function(num) {

  out <- biomes::biomes_information[num, ]

  legend_row <- biomes::biomes_legend[num, -c(1:2)]
  legend_vec <- unlist(legend_row)
  ok         <- !is.na(legend_vec)

  # Build a value -> biome name table for the printed listing.
  biome_listing <- if (any(ok)) {
    paste0(
      "    ",
      sprintf("%2d", seq_along(legend_vec)[ok]),
      ": ",
      legend_vec[ok],
      collapse = "\n"
    )
  } else {
    "    (no class names available)"
  }

  cat(sprintf("\nName: %s (%s)\n", out$name_of_classification, out$publication))
  cat("\n")
  cat(sprintf("Biome scheme number: %s\n", out$scheme_number))
  cat("\n")
  cat(sprintf("Criteria: %s\n", out$criteria_for_class_assignment))
  cat("\n")
  cat(sprintf("Methodology: %s\n", out$methodology))
  cat("\n")
  cat(sprintf("Description: %s\n", out$background_and_specifications))
  cat("\n")
  cat(sprintf("Number of biome classes: %s\n", out$number_of_classes_zonal_azonal))
  cat("\n")
  cat("Biome classes (raster value: name):\n")
  cat(biome_listing, "\n", sep = "")
  cat("\n-----\n")
}
