
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

and load:

``` r
library(exanalysis)
```

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

To calculate the SST belief updates detailed in Section 4.2.1. we use
two functions. The first, <tt>generate\_Hx()</tt>, calculates \(H_x\)
and the second, <tt>calculate\_sst\_update()</tt>, calculates the belief
updates.

``` r
H_list <- generate_Hx(pmip_sst, margo_pseudo)

params_prior <- tibble(
  tau = 6,
  alpha = 0.921,
  kappa = 1.61,
  beta = 1
)

sst_update <- calculate_sst_update(pmip_sst, margo_pseudo, H_list, params_prior)
```

# Update sea-ice concentration
