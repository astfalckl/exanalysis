
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

You can install exanalysis direct from GitHub:

``` r
devtools::install_github("astfalckl/exanalysis")
```

## Code run-through

Let us first load <tt> exanalysis </tt>

``` r
library(exanalysis)
## basic example code
```

The package ships with a number of rds files that contain the data used
in the paper.

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
