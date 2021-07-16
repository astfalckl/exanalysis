
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

Let us first load <tt> exanalysis </tt>

``` r
library(exanalysis)
```

The package ships with a number of rds files that contain the data used
in the paper. Namely, there is \* <tt> pmip\_sst </tt> that contains the
simulation outputs of SST, \* <tt> margo\_sst </tt> that contains proxy
reconstructions of SST (note there are some added points to the original
MARGO dataset).

``` r
pmip_sst
#> List of 7
#>  $ sst   :Classes 'tbl_df', 'tbl' and 'data.frame':  696360 obs. of  6 variables:
#>   ..$ lat  : num [1:696360] -73.8 -73.8 -73.8 -73.8 -73.8 ...
#>   ..$ lon  : num [1:696360] -158 -158 -158 -158 -158 ...
#>   ..$ time : num [1:696360] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ sst  : num [1:696360] -1.87 -1.89 -1.92 -1.92 -1.92 ...
#>   ..$ model: chr [1:696360] "AWI_PMIP4" "AWI_PMIP4" "AWI_PMIP4" "AWI_PMIP4" ...
#>   ..$ ice  : num [1:696360] 0.875 0.882 0.952 0.987 0.986 ...
#>  $ means :Classes 'tbl_df', 'tbl' and 'data.frame':  49740 obs. of  4 variables:
#>   ..$ lat : num [1:49740] -68.8 -68.8 -68.8 -68.8 -68.8 ...
#>   ..$ lon : num [1:49740] -176 -176 -176 -176 -176 ...
#>   ..$ time: num [1:49740] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ sst : num [1:49740] -0.422 -0.12 -0.791 -1.388 -1.59 ...
#>  $ mask  :Classes 'tbl_df', 'tbl' and 'data.frame':  6912 obs. of  3 variables:
#>   ..$ lat : num [1:6912] 88.8 86.2 83.8 81.2 78.8 ...
#>   ..$ lon : num [1:6912] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ mask: num [1:6912] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ coords:Classes 'tbl_df', 'tbl' and 'data.frame':  4145 obs. of  2 variables:
#>   ..$ lon: num [1:4145] -176 -176 -176 -176 -176 ...
#>   ..$ lat: num [1:4145] -68.8 -66.2 -63.8 -61.2 -58.8 ...
#>  $ n     : int 4145
#>  $ ntime : num 12
#>  $ m     : int 14
#>  - attr(*, "class")= chr "sst_sim"
```

``` r
margo_sst
#> List of 5
#>  $ data  :Classes 'tbl_df', 'tbl' and 'data.frame':  766 obs. of  7 variables:
#>   ..$ lon        : num [1:766] -4.31 -16.62 -21.86 5.1 -16.66 ...
#>   ..$ lat        : num [1:766] 36.2 62.1 50.7 -2.2 52.4 ...
#>   ..$ sst_obs    : num [1:766] 11.28 4.68 9.83 26.69 7.08 ...
#>   ..$ reliability: num [1:766] 2 1 2 2 2 1 1 1 1 1 ...
#>   ..$ sd         : num [1:766] 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 1.5 ...
#>   ..$ source     : chr [1:766] "Dinocysts" "Dinocysts" "Dinocysts" "Dinocysts" ...
#>   ..$ type       : chr [1:766] "ann" "ann" "ann" "ann" ...
#>  $ coords:Classes 'tbl_df', 'tbl' and 'data.frame':  766 obs. of  2 variables:
#>   ..$ lon: num [1:766] -4.31 -16.62 -21.86 5.1 -16.66 ...
#>   ..$ lat: num [1:766] 36.2 62.1 50.7 -2.2 52.4 ...
#>  $ B     : num [1:766, 1] 0 2 0 0 0 2 2 2 0 0 ...
#>  $ varB  :
#> Loading required package: Matrix
#> Formal class 'dsCMatrix' [package "Matrix"] with 7 slots
#>   .. ..@ i       : int [1:1035] 1 1 5 1 5 6 1 5 6 7 ...
#>   .. ..@ p       : int [1:767] 0 0 1 1 1 1 3 6 10 10 ...
#>   .. ..@ Dim     : int [1:2] 766 766
#>   .. ..@ Dimnames:List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : NULL
#>   .. ..@ x       : num [1:1035] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..@ uplo    : chr "U"
#>   .. ..@ factors : list()
#>  $ n     : int 766
#>  - attr(*, "class")= chr "sst_data"
```
