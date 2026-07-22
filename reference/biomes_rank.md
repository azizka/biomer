# Rank biome schemes for a given occurrence dataset

Compares the biome classification layers for a user-supplied set of
occurrences and proposes a single "best" layer for that dataset. Each
layer is scored on several data-driven criteria that are combined into
one `composite_score`, which drives the ranking.

## Usage

``` r
biomes_rank(
  x,
  scheme = NULL,
  biome = NULL,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  scheme_type = "all",
  criteria = c("coverage", "effective_classes", "granularity"),
  tiebreaker = c("year", "classes", "none"),
  verbose = TRUE
)
```

## Arguments

- x:

  A data frame with longitude / latitude columns, an `sf` spatial
  object, or a
  [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  of point geometries.

- scheme:

  Optional integer vector in `1:31` (biome scheme numbers) to restrict
  the ranking to a subset of the packaged schemes (e.g.
  `scheme = c(1, 5, 25)`). `NULL` (default) ranks all 31 schemes.
  Ignored when `biome` is supplied.

- biome:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  stack of biome schemes. Use this for custom rasters; for the packaged
  stack prefer `scheme = <int>` instead.

- lon:

  Column name of longitude in `x` (only used if `x` is a non-spatial
  data frame). Default `"decimalLongitude"`.

- lat:

  Column name of latitude in `x` (only used if `x` is a non-spatial data
  frame). Default `"decimalLatitude"`.

- scheme_type:

  Character. Restrict the ranking to one methodological group of biome
  definitions: one of `"all"` (default; rank all 31 layers),
  `"climate"`, `"vegetation"`, `"land_cover"`, `"ecoregion"`,
  `"integrative"`, or `"anthropogenic"`. The grouping is taken from the
  `scheme_type` column of
  [biomes_information](https://azizka.github.io/biomes/reference/biomes_information.md).
  When a specific type is chosen, only the layers of that type are
  classified, scored and returned, so the scaled scores and the best
  layer are determined within that group. Ignored when `biome` is
  supplied.

- criteria:

  Character vector with one or more of `"coverage"`,
  `"effective_classes"`, `"granularity"`, `"informativeness"`,
  `"agreement"`. Default: the first three.

- tiebreaker:

  How tied `composite_score`s are resolved: `"year"` (default, more
  recent publication ranks higher), `"classes"` (more classes ranks
  higher), or `"none"` (do not break ties; tied layers share a rank,
  dense ranking). With `"year"` and `"classes"` the other key serves as
  a further fallback, alphabetical `scheme_name` resolves any remaining
  ties, and ranks are strict 1..N. With `"none"` multiple layers may
  carry `is_best = TRUE`.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

## Details

By default, three equally weighted criteria are used:

1.  **coverage**: fraction of records that the layer places in a biome
    at all (the rest fall on unclassified, NA cells).

2.  **effective_classes**: \\\exp(H')\\ (Hill number of order 1), i.e.
    the effective number of biomes the records spread across, weighted
    by evenness.

3.  **granularity**: biome classes actually used, divided by the classes
    available in the layer.

Two further criteria can be requested via `criteria`:

- **informativeness**: Pielou's evenness \\J' = H' / \log(k\_{used})\\.

- **agreement**: mean pairwise Cohen's \\\kappa\\ against the other
  layers (Monserud & Leemans 1992).

All raw scores are min-max scaled to \\\[0, 1\]\\ across the compared
layers and averaged into the `composite_score`. Layers are then ordered
by this score and ties resolved according to `tiebreaker`.

## Note

`biomes_rank()` gives a data-driven ranking, not an authoritative "best"
classification. The criteria favour layers that cover your records and
split them into many, evenly-used classes, but the top-ranked layer is
not necessarily the most suitable one for your question. For best
results, narrow the comparison to a meaningful group via `scheme_type`,
and treat the ranking as a shortlist rather than a verdict: inspect the
per-criterion columns in the result and use
[`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md)
to choose the layer whose definition and resolution actually match your
data.

## Examples

``` r
data("biomes_example")

# \donttest{
# Ranks layers of the biome raster (~36 MB), downloaded on first use.

# Default call: coverage + effective_classes + granularity, equally weighted
r <- biomes_rank(biomes_example, verbose = FALSE)
head(r)
#>   scheme
#> 1      1
#> 2      2
#> 3      3
#> 4      4
#> 5      5
#> 6      6
#>                                                                                                                  scheme_name
#> 1                                                                       Global vegetation patterns of the past 140,000 years
#> 2                                                  Dataset of the global component of the Copernicus Land Monitoring Service
#> 3                                            Present and future Köppen-Geiger climate classification maps at 1-km resolution
#> 4 Global mapping of potential natural vegetation: an assessment of machine learning algorithms for estimating land potential
#> 5                                                       An ecoregion-based approach to protecting half the terrestrial realm
#> 6                                              A global classification of vegetation based on NDVI, rainfall and temperature
#>   year n_total n_hit n_na pct_na coverage_raw coverage_scaled
#> 1 2020   29104 24452 4652  15.98    0.8401594       0.2844065
#> 2 2019   29104 27587 1517   5.21    0.9478766       0.7708301
#> 3 2018   29104 28023 1081   3.71    0.9628573       0.8384794
#> 4 2018   29104 27538 1566   5.38    0.9461930       0.7632273
#> 5 2017   29104 27943 1161   3.99    0.9601086       0.8260667
#> 6 2017   29104 22619 6485  22.28    0.7771784       0.0000000
#>   effective_classes_raw effective_classes_scaled granularity_raw
#> 1             10.080182                0.6202006       0.9047619
#> 2              7.637310                0.3138203       0.8500000
#> 3             11.659664                0.8182963       0.8333333
#> 4              9.012508                0.4862950       0.9500000
#> 5              7.159880                0.2539420       1.0000000
#> 6              6.163172                0.1289367       1.0000000
#>   granularity_scaled composite_score rank is_best
#> 1          0.4285714       0.4443928   26   FALSE
#> 2          0.1000000       0.3948835   28   FALSE
#> 3          0.0000000       0.5522586   23   FALSE
#> 4          0.7000000       0.6498408   20   FALSE
#> 5          1.0000000       0.6933362   13   FALSE
#> 6          1.0000000       0.3763122   29   FALSE
attr(r, "best_scheme")
#> [1] 16

# Restrict to a subset of criteria
r2 <- biomes_rank(
  biomes_example,
  criteria = c("coverage", "effective_classes"),
  verbose  = FALSE
)
# }
```
