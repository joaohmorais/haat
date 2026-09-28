
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{haat}` - cumulative environmental exposures above a threshold

[![Preprint](https://img.shields.io/badge/medRxiv-Preprint-orange)](https://doi.org/10.64898/2026.08.26.26361449)

`{haat}` is an R function to calculate cumulative exposure areas above a
pre-defined threshold based on hourly observations. The function
approximates the exposure curve for each day and uses `flux::auc()` to
calculate the area under the curve. These cumulative metrics consider
both the exposure intensity and its persistence throughout the day -
which may better represent environmental exposures than traditional
summary metrics.

## Installation

This package can be installed or sourced.

``` r
remotes::install_github("joaohmorais/haat")
```

Or:

``` r
source("https://raw.githubusercontent.com/joaohmorais/haat/refs/heads/master/R/haat.R")
```

## Examples

## Details

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-09-28 17:12:22 -03"
```

## References

- Morais, J. H. de A., Cruz, D. M. de O. e, Saraceni, V., Ferreira, C.
  D., Aguilar, G. M. O., & Cruz, O. G. (2025). Quantifying heat exposure
  and its related mortality in Rio de Janeiro City: Evidence to support
  Rio’s recent heat protocol (p. 2025.01.17.25320740). medRxiv.
  <https://doi.org/10.1101/2025.01.17.25320740>
- De Araujo Morais, J. H., Medeiros De Oliveira E Cruz, D., Saraceni,
  V., Dias Ferreira, C., Mateus Oliveira Aguilar, G., & Gonçalves
  Cruz, O. (2026). Cause-specific heat-related mortality in Rio de
  Janeiro city: Comparing exposure metrics and the role of exposure
  duration. Environmental Epidemiology, 10(3), e473.
  <https://doi.org/10.1097/EE9.0000000000000473>
