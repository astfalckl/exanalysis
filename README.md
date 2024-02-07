
# Coexchangeable process modelling for uncertainty quantification in joint climate reconstruction

This is the accompanying package to “Coexchangeable process modelling
for uncertainty quantification in joint climate reconstruction”. This
<tt>.rmd</tt> file pre-computes some results; to run the analysis from
scratch look at the top section of <tt>run_analysis.R</tt>. Relatedly,
<tt>run_analysis.R</tt> also contains all code to generate the paper’s
figures.

# Installation

To execute on your local machine you can install <tt>exanalysis</tt>
direct from GitHub:

``` r
devtools::install_github("astfalckl/exanalysis")
```

and load:

``` r
library(exanalysis)
library(tidyverse)
```

# Data

The package ships with a number of <tt>.rds</tt> files that contain the
data used in the paper. <tt>sst_data</tt> contains the proxy
reconstructions of SST (note the pseudo-observations mentioned in
Section 4.2.1 are also included here). The primary data source is [the
MARGO project](https://doi.pangaea.de/10.1594/PANGAEA.733406) with some
data supplemented from [Benz et
al. (2016)](https://www.sciencedirect.com/science/article/pii/S0277379116302062).
The data structure has five components:

- <tt>data</tt> that contains the latitude, logitude, SST proxy
  reconstructions, reliability and sd (as listed in the MARGO project),
  proxy source, and an identifier of annual or summer mean value.
- <tt>coords</tt> is simply the coordinate of the SST data subsetted
  into a seperate tibble used for plotting.
- <tt>B</tt> and <tt>varB</tt> are the belief specifications documented
  in Section~5.1.
- <tt>n</tt> is the number of data points used as a global parameter in
  the statistical analysis.

<tt>sic_data</tt> contains the binary observed sea ice values at each of
the model spatial locations. Finally, <tt>model_data</tt> contains the
output of the multi-model ensemble, projected onto the FAMOUS ocean
grid. The data structure has seven components:

- <tt>data</tt> that contains latitude, longitude, time instance, SST
  and SIC values and the MME member.
- <tt>means</tt> that contains the MME’s mean SST value (i.e. $M_X$ in
  the paper).
- <tt>mask</tt> masks the ocean output from the global spatial grid;
  this is used for plotting.
- <tt>coords</tt> the model’s oceanic spatial coordinates.
- <tt>n</tt>, <tt>ntime</tt> and <tt>m</tt> are global parameters of
  number of spatial locations, number of temporal locations and number
  of MME members.

These three data objects can be viewed by running

``` r
str(sst_data, give.attr = FALSE)
str(sic_data, give.attr = FALSE)
str(model_data, give.attr = FALSE)
```

# Sea-surface temperature

To calculate the SST belief updates detailed in Section 4.2.1. we use
two functions. The first, <tt>generate_Hx()</tt>, calculates $H_x$ and
the second, <tt>calculate_sst_update()</tt>, calculates the belief
updates. Our prior belief specifications are stored as a tibble in
<tt>sst_params_prior</tt>. To generate the SST belief updates, the code
is run as

``` r
sst_prior_params <- dplyr::tibble(tau = 6, c = 0.92, kappa = 1.61, alpha2 = 1)

h_list <- generate_Hx(model_data, sst_data)

sst_update <- calculate_sst_update(
  model_data, sst_data, h_list, sst_prior_params
)
```

<tt>h_list</tt> is a list data structure that composes of the spatial,
temporal, and spatio-temporal components of $H_x$. In the back of the
<tt>calculate_sst_update()</tt> code we use the different components of
$H_x$ to enable some algebraic tricks to speed computation. The main
results of the analysis are calculated by
<tt>calculate_sst_update()</tt> and stored, here, in
<tt>sst_update</tt>. The list <tt>sst_update</tt> stores most of the
information that has gone into the calculation as well as the output.
The first level items are

- <tt>simulation</tt>, the model data.
- <tt>proxy</tt>, the reconstructed proxy data.
- <tt>E</tt>, <tt>Vs</tt> and <tt>Vt</tt>, the adjusted expectation and
  variances (decomposed into spatial and temporal components) stored as
  matrix objects for future calculations.
- <tt>update_tbl</tt> is the main results of the analysis and is a
  tibble that stores first and second adjusted expectations and marginal
  variances linked to the spatio-temporal locations. This tibble is used
  to plot Figure 3.

# Update sea-ice concentration

Calculating SIC belief updates require a bit more involvement than the
SST updates. First, we set the spline basis functions in the
<tt>spline_params</tt> objects and specify the prior specfications in
<tt>sic_prior_params</tt>. As we model SIC as a function of SST, we also
require a spatio-temporal SST field; as detailed in the main body of the
paper, here we use simply use the adjusted expectation, and call this
quantity <tt>sst_field</tt>. The projection $\hat{\beta}_i$ are
calculated from <tt>project_betais()</tt> and finally, the main updates
of SIC beliefs are calculated in <tt>calculate_sic_update()</tt>.

``` r
spline_params <- list(
  bounds = c(-1.92, 10),
  knots = c(-1.15, -0.32, 1.44),
  degree = 1,
  prior_exp = c(0.16, 0.47, 0.27, 0.1, 0)
)

sic_prior_params <- list(
  corW_params = c(6, 4, 1, 0),
  varU_params = c(6, 4, 0.3, 0)
)

sst_field <- as.numeric(sst_update$update_tbl$Eadj2)

betais <- project_betais(sst_update, spline_params)

sic_update <- calculate_sic_update(
  sst_update, spline_params, sic_prior_params, betais, sst_field, sic_data
)
```

The output <tt>sic_update</tt> contains

- <tt>data</tt>, the SIC binary data.
- <tt>adj_exp_spline_tbl</tt> and <tt>adj_exp_spline_tbl</tt> the
  adjusted expectations and variances of the spline coefficients. These
  are used to generate the individual spline fits in Figure 4.
- <tt>E</tt>, <tt>V</tt> and <tt>V_param_update</tt>, the adjusted
  expectation and variances (of the parameter coeffecients and of the
  spatio-temporal SIC values) stored as matrix objects.
- <tt>update_tbl</tt> is, again, the main results of the analysis.
  Similarly to SST, this tibble stores first and second adjusted
  expectations and marginal variances of SIC linked to the
  spatio-temporal locations. This tibble is used to plot Figure 5.

# A note on couple climate model runs

The final secton of the paper contains outputs from a coupled climate
model. Providing code to run the model is not feasible as it requires
access to local supercomputing resources and the raw outputs are much
too large to store on github. Instead we have included the model outputs
at four distinct locations (as discussed in the paper) in the folder.
The unique model identification codes are included in the csv headers.
