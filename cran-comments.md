## R CMD check results

This is a new submission.

Local check: 0 errors | 0 warnings | 0 notes
(Windows 11, R 4.6.0, `devtools::check()`).

win-builder check: 0 errors | 0 warnings | 2 notes
(both R-release 4.6.0 and R-devel; results identical).

## Test environments

- local: Windows 11 x64, R 4.6.0 (`devtools::check()`)
- win-builder: Windows Server 2022, R 4.6.0 (release)
- win-builder: Windows Server 2022, R-devel (2026-06-03 r90099)

## Notes from the win-builder check

### NOTE 1 - CRAN incoming feasibility

```
Maintainer: 'Hans Christian Groß <hc1gross@googlemail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  al (7:92)
  et (7:89)
```

* **"New submission"** is expected. This is the first release of the
  package on CRAN.
* **"al" and "et"** are false positives from the reference *"Fischer
  et al, 2022"* in the Description field. They are not misspellings.

### NOTE 2 - Examples runtime

```
Examples with CPU (user + system) or elapsed time > 10s
                 user system elapsed
biomes_classify 11.72   3.87   15.59
```

* `biomes_classify()` is the workhorse function and is benchmarked
  here against the full bundled occurrence dataset
  (`biomes_example`, ~29,000 records) classified through one biome
  raster layer. The runtime reflects realistic use of the function
  rather than a pathological case. The remaining examples and the
  test suite finish well below the CRAN time limits.

## Note on installed package size

`R CMD check --as-cran` may flag an installed size of ~36 MB, with
~35.7 MB located in `inst/extdata/`.

* **The raster stack is the core of the package.** `biomes` ships the
  harmonised global biome raster stack of Fischer et al. (2022,
  *Global Ecology and Biogeography* 31(11): 2172-2183,
  <https://doi.org/10.1111/geb.13574>), 31 biome layers at
  10 x 10 km resolution. Every classification, ranking, tabulation
  and mapping function in the package operates on this stack;
  without it the package cannot fulfil its stated purpose.
* **All bundled files are essential.** None of the 31 layers are
  redundant: each represents a distinct, peer-reviewed biome
  classification scheme. Removing layers would silently change which
  schemes are available to the user and break the rank-and-pick
  workflow that is the package's headline feature.
* **The data are pre-compressed and accessed lazily.** Layers are
  stored as Cloud-Optimised GeoTIFFs and read on demand via `terra`;
  the package only materialises the subsets it needs at runtime.
* **Reproducibility and offline use.** Bundling the stack lets users
  run the full workflow without an internet connection and pins a
  citable, versioned data snapshot to each package release.
* Further size reduction is not feasible without losing core
  functionality.

## Vignette re-build (Windows / OpenBLAS)

All three vignettes set `OPENBLAS_NUM_THREADS = 1` and
`OMP_NUM_THREADS = 1` in their setup chunks. On Windows with
R >= 4.6 the bundled OpenBLAS occasionally fails with
*"Memory allocation still failed after 10 retries"* during
vignette re-building when multiple threads contend for matrix
memory (triggered by the raster extracts behind `biomes_classify()`
and the pairwise kappa loop in `biomes_rank()`). Capping threads
at one avoids the contention without changing any numerical
result; the fix is applied uniformly to all three vignettes
because the error is non-deterministic.

## Reverse dependencies

There are no reverse dependencies (new submission).
