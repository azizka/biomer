# Visualise a `biomes_rank` result

Builds a `ggplot` from the data frame returned by
[`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md).

## Usage

``` r
biomes_show_rank(ranked, type = c("composite", "na", "criteria"))
```

## Arguments

- ranked:

  A `biomes_rank` object returned by
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md).

- type:

  One of `"composite"` (default, horizontal bar plot of
  `composite_score` with the top-1 layer highlighted), `"na"`
  (percent-NA per layer, sorted ascending), or `"criteria"` (heatmap of
  all scaled criteria per layer).

## Examples

``` r
data("biomes_example")
# \donttest{
# biomes_rank() downloads and caches the biome raster (~36 MB).
r <- biomes_rank(biomes_example, verbose = FALSE)
biomes_show_rank(r, type = "composite")

biomes_show_rank(r, type = "na")

biomes_show_rank(r, type = "criteria")

# }
```
