## R CMD check results

This is a new submission.

Local check: 0 errors | 0 warnings | 0 notes
(Windows 11, R 4.6.0, `devtools::check()`).

win-builder check: 0 errors | 0 warnings | 1 note
(both R-release 4.6.0 and R-devel; results identical).

NOTE: checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Hans Christian Groß <hc.gross@gmx.de>'
  
  New submission

## Test environments

- local: Windows 11 x64, R 4.6.0 (`devtools::check()`)
- win-builder: Windows Server 2022, R 4.6.0 (release)
- win-builder: Windows Server 2022, R-devel (2026-06-03 r90099)

## Notes from the win-builder check

### NOTE 1 - CRAN incoming feasibility

```
Maintainer: 'Hans Christian Groß <hc.gross@gmx.de>'

New submission

```

* **"New submission"** is expected. This is the first release of the
  package on CRAN.

## Note on the biome raster data (download on first use)

The ~36 MB biome raster stack (Fischer et al. 2022,
*Global Ecology and Biogeography* 31(11): 2172-2183,
<https://doi.org/10.1111/geb.13574>) is **not** bundled inside the
package, so the installed size stays well under CRAN's limit.

* The stack is hosted as a release asset on the package's GitHub
  repository and downloaded once into a per-user cache directory
  obtained from `tools::R_user_dir("biomes", "cache")`. Subsequent
  calls reuse the cached copy.
* The download is triggered explicitly by `biomes_download()` and,
  transparently, on first use of `biomes_get()` / `biomes_classify()`.
  Nothing is written outside the standard per-user cache location, and
  the package downloads nothing at install or load time.
* Functions that require the raster fail gracefully with an
  informative message if the resource is unavailable (e.g. offline).
* Examples that need the raster are wrapped in `\donttest{}`, tests
  that need it call `testthat::skip_on_cran()` (and skip when
  offline), and vignette chunks that need it are evaluated only when
  the raster can be fetched. The package therefore builds and checks
  without network access on CRAN.


