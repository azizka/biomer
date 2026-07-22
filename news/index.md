# Changelog

## biomes 0.9.3

- Initial CRAN submission.
- Provides raster layers of 31 global biome schemes from Fischer et al.
  (2022, *Global Ecology and Biogeography* 31(11): 2172-2183) at 10 x 10
  km resolution globally.
- Terminology: schemes are addressed by their **biome scheme number**
  (1-31). The `scheme` argument of
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md),
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md),
  [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  and
  [`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md)
  replaces the former `layer` argument;
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  returns the columns `scheme`/`scheme_name` and the attribute
  `best_scheme`;
  [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md)
  returns a `scheme` column; `biomes_information` uses the column
  `scheme_number`.
- Core functions:
  [`biomes_classify()`](https://azizka.github.io/biomes/reference/biomes_classify.md)
  (assign occurrence records to biome classes),
  [`biomes_rank()`](https://azizka.github.io/biomes/reference/biomes_rank.md)
  (rank schemes by coverage, effective number of classes, and
  granularity),
  [`biomes_tab()`](https://azizka.github.io/biomes/reference/biomes_tab.md)
  (tabulate records per biome class),
  [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  (combined figure with `rank`, `map` and `barplot` panels, selectable
  via `panels`),
  [`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md)
  (one-call wrapper),
  [`biomes_get()`](https://azizka.github.io/biomes/reference/biomes_get.md)
  (load the raster stack),
  [`biomes_info()`](https://azizka.github.io/biomes/reference/biomes_info.md)
  (per-scheme metadata),
  [`biomes_occ()`](https://azizka.github.io/biomes/reference/biomes_occ.md)
  (optional GBIF download with coordinate cleaning).
- [`biomes_visualise()`](https://azizka.github.io/biomes/reference/biomes_visualise.md)
  now reproduces the full workflow figure: the `rank` panel shows the
  composite score plus the raw criteria it averages (coverage, effective
  classes, granularity), the `map` panel shows the occurrence map (with
  an adaptive legend), and the `barplot` panel shows records and species
  per biome class back-to-back with centred labels. With
  `combine = FALSE` the individual panels are returned as a named list
  instead of one lettered figure. The former `biomes_show_rank()` has
  been removed (its ranking view is the `rank` panel).
- [`biomes_full()`](https://azizka.github.io/biomes/reference/biomes_full.md):
  the `scheme` argument also accepts a scheme type (`"climate"`,
  `"vegetation"`, `"land_cover"`, `"ecoregion"`, `"integrative"`,
  `"anthropogenic"`) to pick the best-fitting scheme within that group,
  in addition to an integer `1:31` and `"best"`. A new `plot` argument
  controls which figure(s) are built: `"none"` (default, no figure, the
  fastest option), `"all"` (the combined lettered figure in `$plot`), or
  a subset of `c("rank", "map", "barplot")` (returned individually,
  without panel letters, in `$rank`, `$map` and `$barplot`).
- The ~36 MB biome raster stack is not bundled inside the package. It is
  hosted as a GitHub release asset and downloaded once into a per-user
  cache directory
  ([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) on
  first use;
  [`biomes_download()`](https://azizka.github.io/biomes/reference/biomes_download.md)
  performs (or refreshes) this download explicitly. This keeps the
  installed package well under CRAN’s size limit.
- Four vignettes follow the four-step workflow (`step1-` .. `step4-`):
  1.  assembling occurrence records and biome schemes, (2) choosing a
      biome scheme, (3) occurrences-to-biome classification, and (4)
      output and visualisation.
