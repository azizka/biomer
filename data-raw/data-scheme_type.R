# data-raw/data-scheme_type.R
# Add a `scheme_type` column to biomes_information, grouping the 31 biome
# definitions by methodological approach. The grouping is derived from the
# `criteria_for_class_assignment` and `methodology` fields (Fischer et al.
# 2022). Run from the package root.

load("data/biomes_information.rda")   # loads object `biomes_information`

scheme_type <- c(
  "vegetation",    #  1 Global vegetation patterns of the past 140,000 yrs (LPJ-GUESS DGVM)
  "land_cover",    #  2 Copernicus Global Land Cover
  "climate",       #  3 Koeppen-Geiger climate classification
  "vegetation",    #  4 Global mapping of potential natural vegetation (ML)
  "ecoregion",     #  5 Ecoregion-based approach (Dinerstein et al.)
  "climate",       #  6 Global vegetation classification from NDVI + climate clustering
  "climate",       #  7 Climate clustering (dynamic time warping)
  "climate",       #  8 Climate clustering (Euclidean)
  "vegetation",    #  9 Functional biomes
  "vegetation",    # 10 Earth's vegetation (potential natural dominant vegetation)
  "climate",       # 11 Climate K-means clustering
  "climate",       # 12 High-resolution bioclimate map (42 parameters)
  "ecoregion",     # 13 FAO global ecological zones
  "land_cover",    # 14 Global Land Cover by national mapping organizations
  "land_cover",    # 15 ISLSCP II UMD global land cover
  "anthropogenic", # 16 Anthropogenic transformation of the biomes (anthromes, Ellis et al.)
  "land_cover",    # 17 GlobCover
  "land_cover",    # 18 MODIS collection 5 global land cover
  "ecoregion",     # 19 Terrestrial ecoregions of the world
  "climate",       # 20 Updated Koeppen-Geiger climate classification
  "land_cover",    # 21 GLC2000
  "vegetation",    # 22 Potential natural vegetation + biogeochemical modelling
  "ecoregion",     # 23 Terrestrial ecoregions of the world (Olson et al. 2001)
  "land_cover",    # 24 Global land cover characteristics from NDVI
  "vegetation",    # 25 Historical changes in global land cover (potential natural vegetation)
  "climate",       # 26 Holdridge life zones (bioclimatic)
  "integrative",   # 27 Ecozones of the Earth (Schultz)
  "integrative",   # 28 Landscape belts of the Earth
  "integrative",   # 29 Atlas of biogeography
  "integrative",   # 30 Communities and ecosystems (Whittaker)
  "integrative"    # 31 Vegetation and climate (Walter)
)

stopifnot(length(scheme_type) == nrow(biomes_information))
biomes_information$scheme_type <- scheme_type

save(biomes_information, file = "data/biomes_information.rda", compress = "xz")

cat("scheme_type counts:\n")
print(table(scheme_type))
