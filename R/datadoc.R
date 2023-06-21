#' Global Biome Schemes
#'
#' A dataset containing 31 different global biome schemes in a Mollweide 10 x10 km raster
#'
#' @format a projected terra SpatRaster stack with 31 layers and 1800 x 3600 cells
#' @docType data
#' @keywords datasets
#' @name biomes
#' @usage data(biomes)
#' @references 
#' \enumerate{
#' \item Fischer J-C, Waltenowitz A, Beierkuhnlein C (2022) The biome inventory - Standardizing global biogeographical units. Global Ecology and Biogeography31(11):2172-2183. \url{https://doi.org/10.1111/geb.13574}
#' \item Beierkuhnlein C and Fischer, J-C (2021) Global biomes and ecozones - conceptual and spatial communalities and discrepancies. Erdkunde 75(4):249-270. \url{https://doi.org/10.3112/erdkunde.2021.04.01}
#' }
#' 
NULL


#' Biome Encoding and Names for the biomes Dataset
#'
#'  a dataframe containing metadata for the biomes rasterrs, specifically the biome encoding and names.
#'
#' @format a data.frame with 3 columns and 632 rows 
#' \describe{
#'   \item{layer}{player name as in the biomes data}
#'   \item{biome_code}{encoding of different biomes within the SpatRaster}
#'   \item{biome_name}{the respective free-text name for each biome encoding the biomes SpatRaster}
#' }
#' @docType data
#' @keywords datasets
#' @name biomes_legend
#' @usage data(biomes_legend)
NULL


#' Publication Sources and Description of the Biome Schemes in the biomes Data
#'
#'  a dataframe containing metadata for the biomes rasters, specifically the literature references and description of the methodology
#'
#' @format a data.frame with 8 columns and 632 rows 
#' \describe{
#'   \item{layer}{player name as in the biomes data}
#'   \item{source}{publicatoin identifier}
#'   \item{description}{background of the biome classification scheme}
#'   \item{criteria}{criteria used for biome classification}
#'   \item{methodology}{methodology used for biome classification}
#'   \item{n_layers}{number of layers (biomes) in the classification scheme}
#'   \item{publication}{literature reference}
#'   \item{publication_year}{year of publication}
#' }
#' @references Beierkuhnlein C and Fischer, J-C (2021) Global biomes and ecozones - conceptual and spatial communalities and discrepancies. Erdkunde 75(4):249-270. \url{https://doi.org/10.3112/erdkunde.2021.04.01}
#' @docType data
#' @keywords datasets
#' @name biomes_meta
#' @usage data(biomes_meta)
NULL
