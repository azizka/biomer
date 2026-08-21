## Resubmission

This is a resubmission (0.9.4). It addresses all points raised in the
CRAN review of 0.9.3:

* DESCRIPTION: the GBIF link is now formatted for auto-linking as
  <https://www.gbif.org>.
* Documentation: `biomes_rank()` now documents its return value
  (\value in biomes_rank.Rd), describing the class, every column, and
  the attached attributes.
* Examples: \dontrun{} was replaced with \donttest{} wherever the
  example is executable by the user (`biomes_visualise()`, and the
  data-frame path of `biomes_full()`). The remaining \dontrun{}
  examples (`biomes_occ()`, and the taxon path of `biomes_full()`)
  query the GBIF web service and require an interactive console prompt
  and, for the download workflow, personal GBIF credentials, so they
  cannot be executed unattended.
* Writing to the user filespace: `biomes_download()` no longer writes
  to a persistent location by default. The raster is stored under
  `tempdir()` unless the user either supplies a path explicitly or
  actively consents, via an interactive confirmation, to the per-user
  cache directory from `tools::R_user_dir()`. `biomes_occ()` now falls
  back to `tempdir()` (instead of the working directory) when no save
  directory is given. Examples, vignettes, and tests only ever write
  to `tempdir()`.

## R CMD check results

This is a new submission.

Local check: 0 errors | 0 warnings | 0 notes
(Windows 11, R 4.6.0, `devtools::check()`).

win-builder check: 0 errors | 0 warnings | 1 note
(both R-release and R-devel; results identical).

NOTE: checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Hans Christian Groß <hc.gross@gmx.de>'

  New submission

## Test environments

- local: Windows 11 x64, R 4.6.0 (`devtools::check()`)
- win-builder: Windows Server 2022, R-release
- win-builder: Windows Server 2022, R-devel

## Notes from the win-builder check

### NOTE 1 - CRAN incoming feasibility

```
Maintainer: 'Hans Christian Groß <hc.gross@gmx.de>'

New submission

```

* **"New submission"** is expected. This is the first release of the
  package on CRAN.

### NOTE 2 - possibly misspelled word

The word "Reproducibly" (Title and Description) is flagged as possibly
misspelled. It is a correctly spelled English adverb; this is a false
positive.

## Note on the biome raster data (download on first use)

The ~36 MB biome raster stack (Fischer et al. 2022,
*Global Ecology and Biogeography* 31(11): 2172-2183,
<https://doi.org/10.1111/geb.13574>) is **not** bundled inside the
package, so the installed size stays well under CRAN's limit.

* The stack is hosted as a release asset on the package's GitHub
  repository. By default it is downloaded to `tempdir()`; only with
  the user's explicit consent (interactive confirmation, or an
  explicit `path` argument) is it stored in the per-user cache
  directory obtained from `tools::R_user_dir("biomes", "cache")`,
  where subsequent sessions reuse it.
* The download is triggered explicitly by `biomes_download()` and,
  transparently, on first use of `biomes_get()` / `biomes_classify()`.
  Nothing is written outside `tempdir()` without the user's consent,
  and the package downloads nothing at install or load time.
* Functions that require the raster fail gracefully with an
  informative message if the resource is unavailable (e.g. offline).
* Examples that need the raster are wrapped in `\donttest{}`, tests
  that need it call `testthat::skip_on_cran()` (and skip when
  offline), and vignette chunks that need it are evaluated only when
  the raster can be fetched. The package therefore builds and checks
  without network access on CRAN.
