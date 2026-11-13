## Submission

This is a new submission (version 0.3.0).

## Test environments

* local macOS (Tahoe, aarch64), R 4.5.2: `R CMD check --as-cran`
* Ubuntu Linux (rocker/r-ver, current R release), without Bioconductor packages
  installed: `R CMD check --as-cran` (FlowSOM tests skip cleanly)
* win-builder, R-devel (2026-11-12 r90881 ucrt), Windows Server 2022

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* "Possibly misspelled words" (Cytometry, embeddings) are correct terms:
  flow cytometry, dimensionality-reduction embeddings.

## Notes for the reviewer

* `FlowSOM` is listed in `Suggests`. It is distributed through Bioconductor,
  not CRAN. Every use of `FlowSOM` is guarded by `requireNamespace()`, and the
  tests and examples skip when it is not installed. The alternative clustering
  method (`method = "SOM"`) needs only `EmbedSOM`, which is on CRAN.
