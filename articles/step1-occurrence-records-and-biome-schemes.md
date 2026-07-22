# Step 1: Assembling occurrence records and biome schemes

## The four-step biomes workflow

`biomes` follows a four-step workflow, mirrored by these four vignettes
and by Figure 1 of the companion paper:

1.  **Assembling occurrence records and biome schemes** *(this
    vignette)*.
2.  [Choosing a biome
    scheme](https://azizka.github.io/biomes/articles/step2-choosing-a-biome-scheme.md).
3.  [Occurrences-to-biome
    classification](https://azizka.github.io/biomes/articles/step3-occurrence-to-biome-classification.md).
4.  [Output and
    visualisation](https://azizka.github.io/biomes/articles/step4-output-and-visualisation.md).

Throughout, we use the packaged example dataset `biomes_example` so you
can run everything without a download.

> **Terms.** A **biome scheme** is one of the 31 published
> classification systems. A **biome class** is a category within a
> scheme (e.g. *savanna*). A **biome scheme number** (1-31) identifies a
> scheme; it is the value you pass to the `scheme` argument of the
> classification and visualisation functions.

------------------------------------------------------------------------

## 1. Occurrence records

Every downstream function works on a **table of occurrence records**,
one row per record, with a **longitude** and a **latitude** column in
**decimal degrees, WGS84 (EPSG:4326)**. You may also pass an `sf` object
or a
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

| Column | Required | Notes |
|----|----|----|
| longitude | yes | numeric, decimal degrees, WGS84. Default name `decimalLongitude`. |
| latitude | yes | numeric, decimal degrees, WGS84. Default name `decimalLatitude`. |
| `species` | for species counts | needed only to count species per biome class (Step 4). |
| anything else | no | carried through untouched. |

If your columns are named differently, pass their names via `lon` and
`lat`:

``` r

biomes_rank(occ, lon = "decimallongitude", lat = "decimallatitude")
```

The packaged example set:

``` r

data(biomes_example)
nrow(biomes_example)
#> [1] 29104
head(biomes_example)
#> # A tibble: 6 × 5
#>   genus    species          countryCode decimalLongitude decimalLatitude
#>   <chr>    <chr>            <chr>                  <dbl>           <dbl>
#> 1 Felis    Felis catus      US                     -74.6           40.6 
#> 2 Felis    Felis catus      US                     -74.6           40.6 
#> 3 Acinonyx Acinonyx jubatus KE                      35.5           -1.23
#> 4 Lynx     Lynx rufus       US                    -111.            32.3 
#> 5 Lynx     Lynx rufus       US                     -81.6           38.4 
#> 6 Panthera Panthera leo     KE                      35.4           -1.37
```

If you do not already have a dataset,
[`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
can download and clean one from GBIF for a taxon (needs the `rgbif` /
`CoordinateCleaner` packages and a network connection):

``` r

occ <- biomes_occ(taxon = "Fagus sylvatica")
```

------------------------------------------------------------------------

## 2. The 31 biome schemes

[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
returns the packaged raster stack: 31 biome schemes at 10 × 10 km,
globally.

``` r

schemes <- biomes_get()
schemes
#> class       : SpatRaster
#> size        : 1800, 3600, 31  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : -1.8e+07, 1.8e+07, -9000000, 9000000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#> source      : Biomes_Inventory_RasterStack.tif
#> names       : Biome~er_01, Biome~er_02, Biome~er_03, Biome~er_04, Biome~er_05, Biome~er_06, ...
#> min values  :           1,           1,           1,           1,           1,           1, ...
#> max values  :          21,          98,          30,          20,          15,          14, ...
```

Each layer of the stack matches one row of `biomes_information`, in the
same order. Use it (or the human-readable
[`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md))
to see which publication and methodology a scheme comes from:

``` r

data(biomes_information)
biomes_information[25, c("publication", "name_of_classification",
                         "scheme_type", "scheme_number")]
#> # A tibble: 1 × 4
#>   publication              name_of_classification      scheme_type scheme_number
#>   <chr>                    <chr>                       <chr>               <dbl>
#> 1 Ramankutty & Foley, 1999 Estimating historical chan… vegetation             25

biomes_info(25)   # readable summary for biome scheme no. 25
#> 
#> Name: Estimating historical changes in global land cover: croplands from 1700 to 1992 (Ramankutty & Foley, 1999)
#> 
#> Biome scheme number: 25
#> 
#> Criteria: Potential natural vegetation
#> 
#> Methodology: Informed classification of remotely sensed land cover
#> 
#> Description: Potential natural vegetation is derived by classifying DISCover land cover data following the Olson Global Ecosystems framework (Olson, 1994).
#> 
#> Number of biome classes: 12 (12/0)
#> 
#> Biome classes (raster value: name):
#>      1: Tropical evergreen woodland
#>      2: Tropical deciduous woodland
#>      3: Savanna
#>      4: Dense shrubland
#>      5: Desert and barren
#>      6: Open shrubland
#>      7: Grassland and steppe
#>      8: Temperate evergreen woodland
#>      9: Temperate deciduous woodland
#>     10: Mixed woodland
#>     11: Tundra
#>     12: Boreal woodland
#> 
#> -----
```

> **Scheme numbering.** Biome scheme numbers follow the order of the
> biome inventory of Fischer et al. (2022), i.e. the alphabetical order
> of the 31 schemes’ original publications. Scheme no. 25, for example,
> is the vegetation scheme of Ramankutty & Foley (1999).

The class-level lookup (raster value → biome-class name, per scheme)
lives in `biomes_legend`; the classification and visualisation functions
use it internally.

------------------------------------------------------------------------

## Next

You now have (a) occurrence records and (b) the 31 biome schemes and
their metadata. Continue with [Step 2: Choosing a biome
scheme](https://azizka.github.io/biomes/articles/step2-choosing-a-biome-scheme.md).
