#' Metadata for the 31 biome classifications
#'
#' A data frame containing descriptive metadata for each of the 31 biome
#' classifications shipped with the package. Each row corresponds to one
#' biome layer in the raster stack returned by [biomes_get()], in the same
#' order. The metadata is derived from the inventory compiled by
#' Fischer et al. (2022).
#'
#' This is the raw metadata table. For an interactive, human-readable
#' summary of one or more classifications, see [biomes_info()].
#'
#' @format A data frame with 31 rows and 12 columns:
#' \describe{
#'   \item{publication}{Original publication of the biome classification.}
#'   \item{name_of_classification}{Full name of the classification scheme.}
#'   \item{criteria_for_class_assignment}{Criteria used to assign biome classes.}
#'   \item{methodology}{Methodology used to derive the biome classification.}
#'   \item{layer_in_raster_stack}{Index of the corresponding layer in the
#'     raster stack returned by [biomes_get()].}
#'   \item{background_and_specifications}{Free-text background information
#'     about the classification scheme.}
#'   \item{number_of_classes_zonal_azonal}{Total number of biome classes
#'     in the classification, with the split between zonal and azonal
#'     classes in parentheses.}
#'   \item{cover_deviation_percent}{Deviation of the total area covered by
#'     this classification from the mean area of all 31 classifications,
#'     in percent.}
#'   \item{original_file_format}{File format of the original data source
#'     (e.g. raster, shapefile).}
#'   \item{source}{URL or citation of the original data source.}
#'   \item{access_date}{Date on which the original data source was accessed.}
#'   \item{scheme_type}{Methodological group the classification belongs to,
#'     one of `"climate"`, `"vegetation"`, `"land_cover"`, `"ecoregion"`,
#'     `"integrative"` (combined climate-vegetation schemes), or
#'     `"anthropogenic"`. Used by [biomes_rank()] to rank layers within a
#'     conceptually comparable group.}
#' }
#' @source Fischer J-C, Walentowitz A, Beierkuhnlein C (2022) The biome
#'   inventory - Standardizing global biogeographical units.
#'   Global Ecology and Biogeography 31(11): 2172-2183.
#'   \doi{10.1111/geb.13574}
"biomes_information"


#' Example species occurrence dataset
#'
#' A cleaned subset of species occurrence records downloaded from GBIF,
#' used in examples and vignettes to demonstrate biome classification.
#'
#' @format A data frame with 29,104 rows and 5 columns:
#' \describe{
#'   \item{genus}{Genus name.}
#'   \item{species}{Scientific species name.}
#'   \item{countryCode}{ISO 3166-1 alpha-2 country code of the record.}
#'   \item{decimalLongitude}{Decimal longitude in WGS84.}
#'   \item{decimalLatitude}{Decimal latitude in WGS84.}
#' }
#' @source Records downloaded and cleaned from the Global Biodiversity
#'   Information Facility (GBIF). See
#'   \code{inst/extdata/GBIF_example_citation.txt} for the full citation.
"biomes_example"


#' Legend (biome class names) for the 31 biome classifications
#'
#' A data frame mapping the raster values used in each of the 31 biome
#' layers to human-readable biome class names. Each row corresponds to one
#' layer in the raster stack returned by [biomes_get()], in the same order.
#' Columns `id_1`, `id_2`, ... give the biome class names for raster values
#' 1, 2, ..., respectively. Cells are `NA` for classifications with fewer
#' classes than the maximum across all classifications.
#'
#' @format A data frame with 31 rows and 41 columns:
#' \describe{
#'   \item{layer}{Index of the layer in the raster stack returned by
#'     [biomes_get()].}
#'   \item{source}{Short reference to the publication that defines the
#'     classification.}
#'   \item{id_1, id_2, id_3, id_4, id_5, id_6, id_7, id_8, id_9, id_10,
#'     id_11, id_12, id_13, id_14, id_15, id_16, id_17, id_18, id_19,
#'     id_20, id_21, id_22, id_23, id_24, id_25, id_26, id_27, id_28,
#'     id_29, id_30, id_31, id_32, id_33, id_34, id_35, id_36, id_37,
#'     id_38, id_39}{Biome class names for raster values 1 through 39.
#'     `NA` if the classification has fewer classes.}
#' }
#' @source Fischer J-C, Walentowitz A, Beierkuhnlein C (2022) The biome
#'   inventory - Standardizing global biogeographical units.
#'   Global Ecology and Biogeography 31(11): 2172-2183.
#'   \doi{10.1111/geb.13574}
"biomes_legend"
