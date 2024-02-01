
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyenv

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/shinyenv)](https://CRAN.R-project.org/package=shinyenv)
[![Codecov test
coverage](https://codecov.io/gh/shinyworks/shinyenv/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shinyworks/shinyenv?branch=main)
[![R-CMD-check](https://github.com/shinyworks/shinyenv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shinyworks/shinyenv/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An experimental package to deal with environment-like variables in
Shiny.

## Installation

You can install the development version of shinyenv from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shinyworks/shinyenv")
```

## Usage

Instead of `Sys.setenv()`, use `shinyenv::shiny_setenv()`,

``` r
# NO: Sys.setenv(foo = "bar", baz = "foo")
shinyenv::shiny_setenv(foo = "bar", baz = "foo")
```

Instead of `Sys.getenv()`, use `shinyenv::shiny_getenv()`,

``` r
# NO: Sys.getenv("foo")
shinyenv::shiny_getenv("foo")
```

## Code of Conduct

Please note that the shinyenv project is released with a [Contributor
Code of
Conduct](https://shinyworks.github.io/shinyenv/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
