# Load the packaged biome raster stack

Loads the 31 biome layers shipped with the package as a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
stack.

## Usage

``` r
biomes_get(...)
```

## Arguments

- ...:

  Reserved for future use. Currently no arguments are accepted; passing
  any will raise an error.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with 31 layers, one per biome classification (in the same order as the
rows of
[`biomes_information`](https://azizka.github.io/biomes/reference/biomes_information.md)).

## Examples

``` r
# \donttest{
# Load the default biome raster stack (downloads ~36 MB on first use)
biomes_raster <- biomes_get()
biomes_raster
#> class       : SpatRaster
#> size        : 1800, 3600, 31  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : -1.8e+07, 1.8e+07, -9000000, 9000000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#> source      : Biomes_Inventory_RasterStack.tif
#> names       : Biome~er_01, Biome~er_02, Biome~er_03, Biome~er_04, Biome~er_05, Biome~er_06, ...
#> min values  :           1,           1,           1,           1,           1,           1, ...
#> max values  :          21,          98,          30,          20,          15,          14, ...
# }
```
