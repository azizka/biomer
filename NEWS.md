# biomes 0.9.2

* Initial CRAN submission.
* Provides raster layers of 31 biome definitions from Fischer et al.
  (2022, *Global Ecology and Biogeography* 31(11): 2172-2183) at
  10 x 10 km resolution globally.
* Core functions: `biomes_classify()` (assign occurrences to biomes),
  `biomes_rank()` (rank layers by coverage, effective number of
  classes, and granularity), `biomes_tab()` (tabulate records per
  biome), `biomes_visualise()` (publication-style map),
  `biomes_full()` (one-call wrapper), `biomes_get()` (load the
  raster stack), `biomes_info()` (per-layer metadata),
  `biomes_occ()` (optional GBIF download with coordinate cleaning).
* Three vignettes cover the data and ranking, the building-block
  workflow, and the one-call wrapper.
