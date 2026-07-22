# *biomes*: An R package for reproducibly classifying occurrence records using 31 global biome schemes

*biomes* ships spatially explicit raster layers of **31 published global
terrestrial biome schemes** (compiled by Fischer et al. 2022) in one
harmonised format, at a native **10 × 10 km** resolution, together with
functions to classify occurrence records into biome classes and to
choose the most suitable scheme for a dataset in a transparent,
reproducible way.

Because the layers are global at 10 × 10 km, *biomes* is built for
**macroecological and large-scale biogeographical questions**
(continental to global). It is **not** intended for fine-grained,
national-extent analyses, where the 10 × 10 km grid is coarse.

The workflow follows four steps (see the four vignettes below):

1.  **Assembling occurrence records and biome schemes**.
2.  **Choosing a biome scheme**: rank the 31 schemes for your data.
3.  **Occurrences-to-biome classification**: assign records to biome
    classes.
4.  **Output and visualisation**: tabulate and visualise the result.

A single wrapper,
[`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md),
runs all four steps in one call.

------------------------------------------------------------------------

## Terms used in this package

- **biome scheme**: one of the 31 published classification systems
  (e.g. the vegetation scheme of Ramankutty & Foley 1999).
- **biome class**: a single category within a scheme (e.g. *savanna*,
  *tropical evergreen woodland*).
- **biome scheme number**: the number (1-31) that identifies a scheme;
  it is the value you pass to the `scheme` argument. Numbers follow the
  order of the biome inventory (Fischer et al. 2022).

------------------------------------------------------------------------

## Installation

``` r

# install.packages("devtools")
devtools::install_github("azizka/biomes")
```

`biomes` depends on **terra** and installs **ggplot2**, **sf**,
**viridis** and **tidyterra** automatically. The combined figure
(`cowplot`) and the GBIF download path (`rgbif`, `CoordinateCleaner`)
live in `Suggests` and can be installed on demand.

------------------------------------------------------------------------

## Quick start: the one-call workflow

[`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md)
runs the whole four-step workflow. Give it an occurrence dataset (or a
taxon name); it chooses the best-fitting biome scheme, classifies the
records, tabulates them, and (optionally) builds a figure. No figure is
drawn by default (`plot = "none"`, the fastest option). `plot = "all"`
returns the combined figure in `res$plot`, while a subset of
`c("rank", "map", "barplot")` returns the panels **individually** (no
panel letters) in `res$rank`, `res$map` and `res$barplot`.

``` r

library(biomes)

data(biomes_example)

# from an occurrence dataset:
res <- biomes_full(x = biomes_example)   # scheme = "best"; no figure by default

res            # short summary
res$scheme     # the chosen biome scheme number
res$table      # occurrence records per biome class

# add a figure with `plot`; the combined figure is returned in res$plot:
res <- biomes_full(x = biomes_example, plot = "all")
res$plot
ggplot2::ggsave("biomes_figure.png", res$plot, width = 9, height = 12, dpi = 300)

# the panels individually (no a/b/c letters) in res$rank / res$map / res$barplot:
res <- biomes_full(x = biomes_example, plot = c("rank", "map", "barplot"))
res$rank; res$map; res$barplot

# from a taxon name instead of a dataset (downloads from GBIF), e.g. a whole order:
res <- biomes_full(taxon = "Fagales", plot = "all")
```

To force a specific scheme, pass its biome scheme number; to rank within
one methodological group, pass a scheme type:

``` r

biomes_full(x = biomes_example, scheme = 1)            # fixed scheme
biomes_full(x = biomes_example, scheme = "vegetation") # best vegetation scheme
```

The same pipeline as individual building blocks, following the four
workflow steps:

``` r

# Step 1: Assembling occurrence records and biome schemes
data(biomes_example)
schemes <- biomes_get()

# Step 2: Choose a biome scheme
ranking <- biomes_rank(biomes_example, scheme_type = "vegetation")
best <- attr(ranking, "best_scheme")

# Step 3: Occurrences-to-biome classification
cls <- biomes_classify(biomes_example, scheme = best)

# Step 4: Output & visualisation
biomes_tab(cls)
biomes_visualise(biomes_example, scheme = best)
```

[`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
draws up to three panels (`rank`, `map` and `barplot`) combined into one
figure; select any subset with `panels`, e.g. `panels = "map"`.

------------------------------------------------------------------------

## Vignettes: one per workflow step

Read them on the [package
website](https://azizka.github.io/biomes/articles/), or open them
locally with `browseVignettes("biomes")`.

------------------------------------------------------------------------

## Citation

Please cite both:

1.  Groß H, Zizka A (2025): *biomes: An R package for reproducibly
    classifying occurrence records using 31 global biome schemes.* R
    package. <https://github.com/azizka/biomes>. Cite this for the R
    package.
2.  Fischer J-C, Walentowitz A, Beierkuhnlein C (2022): *The biome
    inventory: Standardizing global biogeographical units.* Global
    Ecology and Biogeography 31(11): 2172-2183.
    <https://doi.org/10.1111/geb.13574>. Cite this for the compilation
    of the biome schemes.

``` r

citation("biomes")
```
