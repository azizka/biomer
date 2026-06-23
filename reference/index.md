# Package index

## Biome data

Load raster layers and metadata shipped with the package.

- [`biomes_download()`](https://azizka.github.io/biomes/reference/biomes_download.md)
  : Download the packaged biome raster stack
- [`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
  : Load the packaged biome raster stack
- [`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md)
  : Print metadata for selected biome definitions
- [`biomes_information`](https://azizka.github.io/biomes/reference/biomes_information.md)
  : Metadata for the 31 biome classifications
- [`biomes_legend`](https://azizka.github.io/biomes/reference/biomes_legend.md)
  : Legend (biome class names) for the 31 biome classifications
- [`biomes_example`](https://azizka.github.io/biomes/reference/biomes_example.md)
  : Example species occurrence dataset

## Get occurrences from GBIF

Download and clean GBIF occurrences for a taxon.

- [`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
  : Download and clean GBIF occurrences for a taxon

## Classify and summarize occurrences

- [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md)
  : Classify occurrences into biomes
- [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md)
  : Tabulate the number of occurrences per biome

## Rank biome layers for a dataset

Data-driven scoring of the 31 biome layers for a given occurrence set.

- [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  : Rank biome layers for a given occurrence dataset

- [`biomes_show_rank()`](https://azizka.github.io/biomes/reference/biomes_show_rank.md)
  :

  Visualise a `biomes_rank` result

## Visualize

Map occurrence records over a chosen biome layer.

- [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  : Map occurrences over a biome layer

## One-call workflow

Single-call wrapper from taxon or dataset to table + map.

- [`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md)
  : One-call workflow: from taxon (or dataset) to table + map
