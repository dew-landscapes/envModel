
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `envModel`: Model floristic clusters as a function of environmental variables

<!-- badges: start -->

<!-- badges: end -->

The goal of `envModel` is to help prepare data and run models (currently
only random forest) to predict group membership as a function of
environmental variables. Reduce correlated and ‘unimportant’
environmental variables, run quick random forest runs over many
clusterings outputting only confusion matrix, then choose an area of
clustering space for a ‘good’ model and run random forest, iteratively
adding trees until results stabilise between iterations.

## Installation

`envModel` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dew-landscapes/envModel")
```

## Contents of `envModel`

The following functions and data sets are provided in `envModel`.

| object | class | description |
|:---|:---|:---|
| `envModel::get_conf_metrics()` | function | Calculate diagnostics |
| `envModel::make_env_clust_df()` | function | Add explanatory (environmental) variables to ‘site’ dataframe. |
| `envModel::make_env_corr()` | function | Generate correlation matrix and select variables to remove |
| `envModel::make_rf_diagnostics()` | function | Run random forest, returning only diagnostic values. |
| `envModel::make_rf_diagnostics_old()` | function | Run random forest, usually returning only diagnostic values. |
| `envModel::make_rf_diagnostics_spatialcv()` | function | Run random forest, returning only diagnostic values from spatial cross |
| `envModel::make_rf_good()` | function | Iteratively add trees to random forest until predictions stabilise |
| `envModel::reduce_env()` | function | Reduce number of environmental variables |
