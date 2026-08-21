# Download and clean GBIF occurrences for a taxon

Retrieves occurrence records for a given taxon (species, genus, family,
...) from GBIF and, optionally, runs standard coordinate cleaning with
[`CoordinateCleaner::clean_coordinates()`](https://ropensci.github.io/CoordinateCleaner/reference/clean_coordinates.html).

## Usage

``` r
biomes_occ(
  taxon,
  use_download = FALSE,
  username = NULL,
  pwd = NULL,
  email = NULL,
  save_dir = NULL,
  filter_clean = TRUE,
  filter_sea = FALSE,
  year_min = NULL,
  year_max = NULL,
  country = NULL,
  limit = NULL,
  slim = TRUE
)
```

## Arguments

- taxon:

  Scientific name(s) to query (species, genus, family, ...). Accepts a
  single name or a character vector of names. All matching keys are
  bundled into one `occ_download()` job; the `occ_search()` path loops
  over the taxa and splits the user-requested number of records evenly
  across them.

- use_download:

  Logical. Force the GBIF download workflow even if the total record
  count is below 100,000. Default: `FALSE`.

- username:

  GBIF username (used when the download workflow is triggered, either
  via `use_download = TRUE` or by the interactive prompt). If `NULL`,
  the user is asked at the console.

- pwd:

  GBIF password (same logic as `username`).

- email:

  GBIF account email (same logic as `username`).

- save_dir:

  Directory used for outputs when `occ_download()` is triggered (both
  the data CSV and a `*_citation.txt`). If `NULL` (default), the user is
  asked at the console; an empty answer and non-interactive sessions
  fall back to the session's temporary directory
  ([`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

- filter_clean:

  Logical. If `TRUE`, run basic spatial cleaning on coordinates with
  [`CoordinateCleaner::clean_coordinates()`](https://ropensci.github.io/CoordinateCleaner/reference/clean_coordinates.html).
  Default: `TRUE`.

- filter_sea:

  Logical. If `TRUE`, also flag occurrences in the sea (`"seas"` test).
  Default: `FALSE`.

- year_min:

  Optional integer. If supplied, only records with `year >= year_min`
  are kept on the GBIF side. `NULL` (default) means no year filter.

- year_max:

  Optional integer. Same as `year_min` but for the upper bound.

- country:

  Optional character vector. One or more ISO 3166-1 alpha-2 country
  codes (only used in the download workflow).

- limit:

  Optional integer. Number of records to download. When `NULL`
  (default), the user is asked interactively in the `occ_search()` path;
  the `occ_download()` path always returns all available records.

- slim:

  Logical. If `TRUE` (default), the result is trimmed to
  `family, genus, species, year, countryCode, decimalLongitude, decimalLatitude`.
  Set to `FALSE` to keep all GBIF columns.

## Value

A data frame of GBIF occurrence records, optionally cleaned. Always
contains `decimalLongitude` and `decimalLatitude` columns (when records
are returned), so the result can be passed directly to
[`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md),
[`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
or
[`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md).

## Details

By default, `biomes_occ()` first asks GBIF how many records exist for
the taxon and then prompts the user (in interactive sessions):

1.  Use
    [`rgbif::occ_search()`](https://docs.ropensci.org/rgbif/reference/occ_search.html)?
    (no login required, capped at 100,000 records.) If yes, the user is
    then asked for the number of records. If no, the function switches
    to
    [`rgbif::occ_download()`](https://docs.ropensci.org/rgbif/reference/occ_download.html),
    which needs a save directory and GBIF credentials and downloads
    everything (you get a DOI for citation).

The only GBIF predicate applied is `hasCoordinate = TRUE`. The result is
slim by default: `family`, `genus`, `species`, `year`, `countryCode`,
`decimalLongitude`, `decimalLatitude`. Set `slim = FALSE` to keep every
GBIF column. With `occ_download()` the downloaded data and the GBIF
citation are written to `save_dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
# interactive: prompted for occ_search vs occ_download
occ <- biomes_occ(taxon = "Solemyida")

# force the GBIF download workflow up front (requires credentials)
occ <- biomes_occ(
  taxon        = "Fagus sylvatica",
  use_download = TRUE,
  username     = "xxx",
  pwd          = "xxx",
  email        = "you@example.org",
  save_dir     = file.path(tempdir(), "GBIF")
)
} # }
```
