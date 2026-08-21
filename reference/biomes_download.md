# Download the packaged biome raster stack

The 31-layer biome raster stack (`Biomes_Inventory_RasterStack.tif`, ~36
MB) is too large to ship inside the package on CRAN. It is hosted as a
release asset on GitHub instead. `biomes_download()` fetches it once and
reuses the local copy on every later call, including every internal use
by
[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
or
[`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md).

## Usage

``` r
biomes_download(path = NULL, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- path:

  Optional character string: directory in which to store the raster.
  Default `NULL`: use the persistent per-user cache directory if the
  user has agreed to it, otherwise
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) (see Details).

- overwrite:

  Logical flag; if `TRUE`, re-download the raster even when a local copy
  already exists. Defaults to `FALSE`.

- quiet:

  Logical flag; if `TRUE`, suppress the informational message and the
  download progress bar. Defaults to `FALSE`.

## Value

The local file path to the raster, invisibly.

## Details

The storage location is chosen as follows:

- If `path` is supplied, the raster is written to that directory and no
  other location is touched.

- If `path` is `NULL` and a copy already exists in the persistent
  per-user cache directory (see
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)), that
  copy is reused.

- Otherwise, in interactive sessions, the function asks once for
  permission to store the raster in the persistent per-user cache
  directory, so that the download happens only once across R sessions.

- If permission is declined, or in non-interactive sessions, the raster
  is stored under [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  and is removed automatically when the R session ends.

The package therefore never writes outside
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) without the user's
explicit consent (an interactive confirmation or an explicit `path`).

## See also

[`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
to load the raster as a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Examples

``` r
# \donttest{
# Downloads ~36 MB into the session's temporary directory.
raster_path <- biomes_download(path = tempdir())
#> Downloading biome raster stack (~36 MB) to:
#>   /tmp/Rtmpt3MgSm/Biomes_Inventory_RasterStack.tif
raster_path
#> [1] "/tmp/Rtmpt3MgSm/Biomes_Inventory_RasterStack.tif"
# }
```
