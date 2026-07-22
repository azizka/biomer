#' Print metadata for selected biome definitions
#'
#' Prints a human-readable summary of the biome classifications shipped
#' with the package. For each requested classification the function prints
#' the publication, the criteria and methodology used to define the
#' classes, a short description, the number of biome classes, the biome
#' scheme number, and a list of biome-class names with their raster values.
#'
#' This is the *interactive* sibling of the `biomes_information` data set:
#' use [`biomes_information`] when you want the raw metadata table (e.g.
#' to subset, filter, or join programmatically), and `biomes_info()` when
#' you want a quick read of the most relevant fields for a specific
#' biome scheme.
#'
#' @param x Integer vector of biome scheme numbers between 1 and 31. If
#'   `NULL` (the default), information for all 31 schemes is printed.
#'
#' @return Invisibly returns the integer vector of biome scheme numbers that
#'   was printed. The function is called for its side effect of printing
#'   to the console.
#'
#' @seealso [`biomes_information`] for the underlying metadata table and
#'   [`biomes_legend`] for the mapping from raster values to biome names.
#'
#' @examples
#' # Print information for all biome definitions
#' biomes_info()
#'
#' # Print information for the first three biomes
#' biomes_info(1:3)
#'
#' @export
biomes_info <- function(x = NULL) {
  # Assertions: validate x (biome indices 1-31)
  if (!is.null(x)) {
    checkmate::assert_integerish(
      x,
      lower = 1,
      upper = 31,
      any.missing = FALSE,
      .var.name = "x"
    )
  }

  idx <- if (is.null(x)) 1:31 else x

  for (i in idx) {
    info_grabber(i)
  }

  invisible(idx)
}
