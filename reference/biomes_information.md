# Metadata for the 31 biome classifications

A data frame containing descriptive metadata for each of the 31 biome
classifications shipped with the package. Each row corresponds to one
biome layer in the raster stack returned by
[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md),
in the same order. The metadata is derived from the inventory compiled
by Fischer et al. (2022).

## Usage

``` r
biomes_information
```

## Format

A data frame with 31 rows and 12 columns:

- publication:

  Original publication of the biome classification.

- name_of_classification:

  Full name of the classification scheme.

- criteria_for_class_assignment:

  Criteria used to assign biome classes.

- methodology:

  Methodology used to derive the biome classification.

- scheme_number:

  Biome scheme number (1-31); index of the corresponding layer in the
  raster stack returned by
  [`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md).

- background_and_specifications:

  Free-text background information about the classification scheme.

- number_of_classes_zonal_azonal:

  Total number of biome classes in the classification, with the split
  between zonal and azonal classes in parentheses.

- cover_deviation_percent:

  Deviation of the total area covered by this classification from the
  mean area of all 31 classifications, in percent.

- original_file_format:

  File format of the original data source (e.g. raster, shapefile).

- source:

  URL or citation of the original data source.

- access_date:

  Date on which the original data source was accessed.

- scheme_type:

  Methodological group the classification belongs to, one of
  `"climate"`, `"vegetation"`, `"land_cover"`, `"ecoregion"`,
  `"integrative"` (combined climate-vegetation schemes), or
  `"anthropogenic"`. Used by
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  to rank layers within a conceptually comparable group.

## Source

Fischer J-C, Walentowitz A, Beierkuhnlein C (2022) The biome inventory -
Standardizing global biogeographical units. Global Ecology and
Biogeography 31(11): 2172-2183.
[doi:10.1111/geb.13574](https://doi.org/10.1111/geb.13574)

## Details

This is the raw metadata table. For an interactive, human-readable
summary of one or more classifications, see
[`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md).
