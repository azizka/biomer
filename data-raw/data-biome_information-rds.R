# Build data/biomes_information.rda from the source xlsx file.
# Run this from the project root, e.g. with:
#   source("data-raw/data-biome_information-rds.R")

library(readxl)
library(usethis)

biomes_information <- readxl::read_excel(
  "data-raw/biomes_information.xlsx"
)
biomes_information <- as.data.frame(biomes_information)

# Snake-case column names so they can be accessed without backticks (#60).
names(biomes_information) <- c(
  "publication",
  "name_of_classification",
  "criteria_for_class_assignment",
  "methodology",
  "scheme_number",
  "background_and_specifications",
  "number_of_classes_zonal_azonal",
  "cover_deviation_percent",
  "original_file_format",
  "source",
  "access_date"
)

usethis::use_data(biomes_information, overwrite = TRUE)
