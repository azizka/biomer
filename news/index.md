# Changelog

## biomes 0.9.2

- Initial CRAN submission.
- Provides raster layers of 31 biome definitions from Fischer et al.
  (2022, *Global Ecology and Biogeography* 31(11): 2172-2183) at 10 x 10
  km resolution globally.
- Core functions:
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md)
  (assign occurrences to biomes),
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  (rank layers by coverage, effective number of classes, and
  granularity),
  [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md)
  (tabulate records per biome),
  [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  (publication-style map),
  [`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md)
  (one-call wrapper),
  [`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
  (load the raster stack),
  [`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md)
  (per-layer metadata),
  [`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
  (optional GBIF download with coordinate cleaning).
- The ~36 MB biome raster stack is not bundled inside the package. It is
  hosted as a GitHub release asset and downloaded once into a per-user
  cache directory
  ([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) on
  first use;
  [`biomes_download()`](https://azizka.github.io/biomes/reference/biomes_download.md)
  performs (or refreshes) this download explicitly. This keeps the
  installed package well under CRAN’s size limit.
- Three vignettes cover the data and ranking, the building-block
  workflow, and the one-call wrapper.
