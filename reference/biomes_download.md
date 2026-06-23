# Download the packaged biome raster stack

The 31-layer biome raster stack (`Biomes_Inventory_RasterStack.tif`, ~36
MB) is too large to ship inside the package on CRAN. It is hosted as a
release asset on GitHub instead. `biomes_download()` fetches it once
into a persistent, per-user cache directory (see
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)); every
later call - and every internal use by
[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
or
[`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md) -
reuses the cached copy, so the download happens only the first time.

## Usage

``` r
biomes_download(overwrite = FALSE, quiet = FALSE)
```

## Arguments

- overwrite:

  Logical flag; if `TRUE`, re-download the raster even when a cached
  copy already exists. Defaults to `FALSE`.

- quiet:

  Logical flag; if `TRUE`, suppress the informational message and the
  download progress bar. Defaults to `FALSE`.

## Value

The local file path to the cached raster, invisibly.

## See also

[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
to load the raster as a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Examples

``` r
# \donttest{
# Downloads ~36 MB on first run, then reuses the cached copy.
raster_path <- biomes_download()
raster_path
#> [1] "/home/runner/.cache/R/biomes/Biomes_Inventory_RasterStack.tif"
# }
```
