
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alexmisc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/alexmisc)](https://CRAN.R-project.org/package=alexmisc)
<!-- badges: end -->

The `alexmisc` package contains Alexander Brenning’s miscellaneous
utility functions.

## Installation

<!---
You can install the released version of alexmisc from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("alexmisc")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alexanderbrenning/alexmisc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(alexmisc)
replace_umlaute("Jenalöbnitz")
#> [1] "Jenaloebnitz"
failed(try(sqrt("a"), silent = TRUE))
#> [1] TRUE
geocode_wiki_town("Jena")
#> Loading required package: magrittr
#> $name
#> [1] "Jena"
#> 
#> $latitude
#> [1] 50.92721
#> 
#> $longitude
#> [1] 11.58636
## basic example code
```
