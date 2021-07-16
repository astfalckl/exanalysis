
<!-- README.md is generated from README.Rmd. Please edit that file -->

\#\#NOTE: This code is still very much in development. If you have
somehow stumbled onto my GitHub to have a squizz, thanks for the
interest but please advert your eyes for a moment.

# exanalysis

<!-- badges: start -->

<!-- badges: end -->

This is the accompanying package to “Reconstruction of sea-surface
temperature and sea-ice concentration via exchangeability analysis”.

## Installation

You can install <tt> exanalysis </tt> direct from GitHub:

``` r
devtools::install_github("astfalckl/exanalysis")
```

## Code run-through

Let us first load <tt> exanalysis </tt> and some other dependencies.

``` r
library(exanalysis)
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 3.6.2
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.2     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.1
#> Warning: package 'ggplot2' was built under R version 3.6.2
#> Warning: package 'tibble' was built under R version 3.6.2
#> Warning: package 'tidyr' was built under R version 3.6.2
#> Warning: package 'readr' was built under R version 3.6.2
#> Warning: package 'purrr' was built under R version 3.6.2
#> Warning: package 'dplyr' was built under R version 3.6.2
#> Warning: package 'forcats' was built under R version 3.6.2
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

The package ships with a number of rds files that contain the data used
in the paper. Namely, there is \* <tt> pmip\_sst </tt> that contains the
simulation outputs of SST, \* <tt> margo\_sst </tt> that contains proxy
reconstructions of SST (note there are some added points to the original
MARGO dataset).

These files are all lists that contain useful information to the
analysis. The main contents of these lists are

``` r
as_tibble(pmip_sst$sst)
#> # A tibble: 696,360 x 6
#>      lat   lon  time   sst model       ice
#>    <dbl> <dbl> <dbl> <dbl> <chr>     <dbl>
#>  1 -73.8 -158.     1 -1.87 AWI_PMIP4 0.875
#>  2 -73.8 -158.     2 -1.89 AWI_PMIP4 0.882
#>  3 -73.8 -158.     3 -1.92 AWI_PMIP4 0.952
#>  4 -73.8 -158.     4 -1.92 AWI_PMIP4 0.987
#>  5 -73.8 -158.     5 -1.92 AWI_PMIP4 0.986
#>  6 -73.8 -158.     6 -1.92 AWI_PMIP4 0.987
#>  7 -73.8 -158.     7 -1.92 AWI_PMIP4 0.983
#>  8 -73.8 -158.     8 -1.92 AWI_PMIP4 0.978
#>  9 -73.8 -158.     9 -1.92 AWI_PMIP4 0.971
#> 10 -73.8 -158.    10 -1.92 AWI_PMIP4 0.966
#> # … with 696,350 more rows
```

``` r
as_tibble(margo_sst$data)
#> # A tibble: 766 x 7
#>       lon   lat sst_obs reliability    sd source    type 
#>     <dbl> <dbl>   <dbl>       <dbl> <dbl> <chr>     <chr>
#>  1  -4.31 36.2    11.3            2   1.5 Dinocysts ann  
#>  2 -16.6  62.1     4.68           1   1.5 Dinocysts ann  
#>  3 -21.9  50.7     9.83           2   1.5 Dinocysts ann  
#>  4   5.10 -2.20   26.7            2   1.5 Dinocysts ann  
#>  5 -16.7  52.4     7.08           2   1.5 Dinocysts ann  
#>  6   4.90 66.7     4.54           1   1.5 Dinocysts ann  
#>  7 -29.6  64.8     6.94           1   1.5 Dinocysts ann  
#>  8 -64.3  71.3    -0.16           1   1.5 Dinocysts ann  
#>  9 -57.5  58.4    -0.29           1   1.5 Dinocysts ann  
#> 10 -63.1  42.6     6.36           1   1.5 Dinocysts ann  
#> # … with 756 more rows
```
