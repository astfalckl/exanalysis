
# Coexchangeable process modelling for uncertainty quantification in joint climate reconstruction

This is the accompanying package to “Coexchangeable process modelling
for uncertainty quantification in joint climate reconstruction”. This
<tt>.rmd</tt> file pre-computes some results; to run the analysis from
scratch look in <tt>run\_analysis.R</tt>.

# Installation

To execute on your local machine you can install <tt>exanalysis</tt>
direct from GitHub:

``` r
devtools::install_github("astfalckl/exanalysis")
```

and load <tt> exanalysis
    </tt>.

``` r
library(exanalysis)
```

    #> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
    #> ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    #> ✔ tibble  3.1.2     ✔ dplyr   1.0.7
    #> ✔ tidyr   1.1.3     ✔ stringr 1.4.0
    #> ✔ readr   1.4.0     ✔ forcats 0.5.1
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> ✖ dplyr::filter() masks stats::filter()
    #> ✖ dplyr::lag()    masks stats::lag()

# Data

The package ships with a number of <tt>.rds</tt> files that contain the
data used in the paper. Namely,

  - <tt>model\_data</tt> that contains the PMIP simulation outputs of
    SST and SIC,
  - <tt>sst\_data</tt> that contains proxy reconstructions of SST (note
    the pseudo-observations mentioned in Section 4.2.1 are included
    here), and
  - <tt>sic\_data</tt> that contains binary estimates of SIC inferred
    from Northern and Southern hemisphere maximum sea-ice extents.

# Sea-surface temperature

# Update sea-ice concentration
