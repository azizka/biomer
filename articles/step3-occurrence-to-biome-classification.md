# Step 3: Occurrences-to-biome classification

## Goal

With a biome scheme chosen in [Step
2](https://azizka.github.io/biomes/articles/step2-choosing-a-biome-scheme.md),
[`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md)
assigns **one biome class per occurrence record**. You select the scheme
by its **biome scheme number** (1-31), the same number
[`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
returns as the best scheme.

> **Terms.** *Classifying* here means assigning each **occurrence
> record** to a **biome class** (e.g. *savanna*) of the chosen **biome
> scheme** (identified by its biome scheme number).

------------------------------------------------------------------------

## 1. Classify occurrence records into biome classes

[`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md)
takes a table of points (or an `sf` / `SpatVector`) and returns the
**input data with the biome-class assignment appended on the right**.
Pick the scheme with the `scheme` argument; you never handle
`SpatRaster` objects yourself.

``` r

classified <- biomes_classify(biomes_example, scheme = 1)
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 1 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
head(classified)
#>      genus          species countryCode decimalLongitude decimalLatitude
#> 1    Felis      Felis catus          US        -74.60746       40.634316
#> 2    Felis      Felis catus          US        -74.60794       40.634346
#> 3 Acinonyx Acinonyx jubatus          KE         35.47705       -1.228576
#> 4     Lynx       Lynx rufus          US       -110.90994       32.298303
#> 5     Lynx       Lynx rufus          US        -81.57826       38.397055
#> 6 Panthera     Panthera leo          KE         35.44188       -1.372002
#>   Biome_Inventory_layer_01_name
#> 1  Temperate summergreen forest
#> 2  Temperate summergreen forest
#> 3     Tropical raingreen forest
#> 4       Warm temperate woodland
#> 5  Temperate summergreen forest
#> 6     Tropical raingreen forest
```

A new column `Biome_Inventory_layer_01_name` has been added (the column
names carry the raster layer names of the packaged stack). The appended
columns use the suffixes `_value` (raster code) and `_name` (biome-class
name). Records that fall **outside** every biome class of a scheme
(e.g. coastal records or small islands missing from a coarse map) are,
by default, labelled `"no_biome"` rather than dropped, so the counts
stay complete:

``` r

table(classified$Biome_Inventory_layer_01_name, useNA = "ifany")
#> 
#>    Boreal evergreen needleleaf forest                       Boreal parkland 
#>                                  2217                                   367 
#>   Boreal summergreen broadleaf forest                                Desert 
#>                                   384                                   221 
#>                              no_biome                               Savanna 
#>                                  4652                                   447 
#>                            Semidesert                          Shrub tundra 
#>                                   583                                   483 
#>                                Steppe  Temperate broadleaf evergreen forest 
#>                                   148                                  5171 
#>                Temperate mixed forest Temperate needleleaf evergreen forest 
#>                                  1687                                   323 
#>                    Temperate parkland                   Temperate shrubland 
#>                                   407                                   729 
#>          Temperate summergreen forest             Tropical evergreen forest 
#>                                  6139                                  1860 
#>                    Tropical grassland             Tropical raingreen forest 
#>                                   118                                   824 
#>                                Tundra               Warm temperate woodland 
#>                                     2                                  2342
```

Handling off-map records **explicitly and identically across schemes**
matters, because the amount and spatial pattern of unassigned records
differs between schemes, and is itself one of the ranking criteria in
Step 2.

------------------------------------------------------------------------

## 2. Common variations

``` r

# Several schemes at once, one column per scheme
biomes_classify(biomes_example, scheme = c(1, 25)) |> head(3)
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 2 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
#>   - Biome_Inventory_layer_25 (Ramankutty & Foley, 1999)
#>      genus          species countryCode decimalLongitude decimalLatitude
#> 1    Felis      Felis catus          US        -74.60746       40.634316
#> 2    Felis      Felis catus          US        -74.60794       40.634346
#> 3 Acinonyx Acinonyx jubatus          KE         35.47705       -1.228576
#>   Biome_Inventory_layer_01_name Biome_Inventory_layer_25_name
#> 1  Temperate summergreen forest  Temperate deciduous woodland
#> 2  Temperate summergreen forest  Temperate deciduous woodland
#> 3     Tropical raingreen forest                       Savanna

# Keep both the raster value and the biome-class name
biomes_classify(biomes_example, scheme = 1, value = "both") |> head(3)
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 1 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
#>      genus          species countryCode decimalLongitude decimalLatitude
#> 1    Felis      Felis catus          US        -74.60746       40.634316
#> 2    Felis      Felis catus          US        -74.60794       40.634346
#> 3 Acinonyx Acinonyx jubatus          KE         35.47705       -1.228576
#>   Biome_Inventory_layer_01_value Biome_Inventory_layer_01_name
#> 1                             13  Temperate summergreen forest
#> 2                             13  Temperate summergreen forest
#> 3                              2     Tropical raingreen forest

# Return only the classification columns (drop the input)
biomes_classify(biomes_example, scheme = 1, append = FALSE) |> head(3)
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 1 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
#>   Biome_Inventory_layer_01_name
#> 1  Temperate summergreen forest
#> 2  Temperate summergreen forest
#> 3     Tropical raingreen forest

# Keep NA for off-map points instead of the "no_biome" label
class_na <- biomes_classify(biomes_example, scheme = 1, na = NA)
#> Coordinates provided as data.frame, assuming WGS84 as CRS.
#> Classified 29104 record(s) against 1 biome layer(s):
#>   - Biome_Inventory_layer_01 (Allen et al., 2020)
sum(is.na(class_na$Biome_Inventory_layer_01_name))
#> [1] 4652
```

For a scheme outside the packaged stack, pass your own single-layer
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
via `biome =` instead of a scheme number.

------------------------------------------------------------------------

## Next

Your records now carry a biome-class assignment. Continue with [Step 4:
Output and
visualisation](https://azizka.github.io/biomes/articles/step4-output-and-visualisation.md)
to tabulate and map them.
