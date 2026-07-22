# One-call workflow: from taxon (or dataset) to table (and optional figure)

Convenience wrapper that runs the full `biomes` workflow in a single
call. There are two entry paths:

## Usage

``` r
biomes_full(
  x = NULL,
  taxon = NULL,
  scheme = "best",
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  value = "name",
  plot = "none",
  show = FALSE,
  ...
)
```

## Arguments

- x:

  Optional. A data frame with longitude/latitude columns, an `sf`
  spatial object, or a
  [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).
  Mutually exclusive with `taxon`.

- taxon:

  Optional scientific name (species, genus, family, ...). Mutually
  exclusive with `x`.

- scheme:

  One of: an integer in `1:31` (biome scheme number) to force a specific
  scheme; `"best"` (default) to pick the best-fitting scheme across all
  31 via
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md);
  or a scheme type (`"climate"`, `"vegetation"`, `"land_cover"`,
  `"ecoregion"`, `"integrative"`, `"anthropogenic"`) to pick the
  best-fitting scheme within that methodological group.

- lon, lat:

  Column names of longitude / latitude in `x` (data frame only).
  Defaults `"decimalLongitude"`/`"decimalLatitude"`.

- value:

  Passed to
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md):
  `"name"` (default), `"ID"`, or `"both"`.

- plot:

  Which figure(s)
  [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  should build. `"none"` (default): no figure (the fastest option).
  `"all"`: the combined, lettered figure (rank + map + barplot) in
  `$plot`. A subset of `c("rank", "map", "barplot")`: the requested
  panels are returned **individually** (no panel letters) in `$rank`,
  `$map` and `$barplot` – e.g. `plot = c("rank", "map", "barplot")`
  fills all three, `plot = "map"` fills only `$map`. `NULL` is accepted
  as an alias for `"none"`.

- show:

  Logical. If `TRUE`, print the figure (if any) and the tabulation to
  the console as a side effect. The function always returns its result
  invisibly. Default: `FALSE`.

- ...:

  Further arguments passed to
  [`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
  when `taxon` is given (e.g. `limit`, `year_min`, `year_max`,
  `use_download`, GBIF credentials).

## Value

Invisibly, a `biomes_full` list with elements:

- `occ`:

  The occurrence data frame (downloaded or provided).

- `scheme`:

  The chosen biome scheme number.

- `ranking`:

  The ranking data frame (only when `scheme = "best"`), otherwise
  `NULL`.

- `classified`:

  The output of
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md).

- `table`:

  The biome occurrence table from
  [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md).

- `plot`:

  The combined, lettered figure (only when `plot = "all"`), otherwise
  `NULL`.

- `rank`, `map`, `barplot`:

  The individual panels (no letters), each present only when requested
  via `plot = c(...)`, otherwise `NULL`.

## Details

1.  **From a taxon name.** Pass a scientific name as `taxon`
    (`x = NULL`). `biomes_full()` calls
    [`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
    to download cleaned GBIF occurrences for the taxon and then proceeds
    as below.

2.  **From an occurrence dataset.** Pass a data frame, `sf` object or
    [`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
    as `x` (`taxon = NULL`).

Once occurrences are available the function:

- picks the biome scheme (either `scheme = <integer>` or, with the
  default `scheme = "best"`, by running
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  and selecting the top-1 scheme);

- classifies the records with
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md);

- tabulates them with
  [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md);

- optionally builds a figure with
  [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  (controlled by `plot`; skipped by default for speed).

## Examples

``` r
if (FALSE) { # \dontrun{
# Path 1: from a taxon name (downloads via GBIF)
res <- biomes_full(taxon = "Fagus sylvatica", limit = 2000)
res$table

# Path 2: from an existing data frame, pick the best scheme
data("biomes_example")
res <- biomes_full(x = biomes_example, scheme = "best")

# Path 2 with a fixed scheme
res <- biomes_full(x = biomes_example, scheme = 1)

# Path 2, best-fitting scheme within the vegetation group,
# and build the full figure
res <- biomes_full(x = biomes_example, scheme = "vegetation", plot = "all")
res$plot

# individual panels (no a-c letters) in $rank / $map / $barplot
res <- biomes_full(x = biomes_example, plot = c("map", "barplot"))
res$map
res$barplot
} # }
```
