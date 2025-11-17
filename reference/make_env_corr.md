# Generate correlation matrix and select variables to remove

Generate correlation matrix and select variables to remove

## Usage

``` r
make_env_corr(
  env_df,
  env_cols,
  remove = TRUE,
  thresh = 0.95,
  always_remove = c("lat", "long"),
  always_keep = c("prec", "temp")
)
```

## Arguments

- env_df:

  Dataframe with environmental variables

- env_cols:

  Numeric or character vector defining columns in env_df that contain
  the variables to test for correlation.

- remove:

  Logical. Remove variables correlated at or above `thresh`. Correlation
  is found via
  [`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html).

- thresh:

  Numeric. Threshold correlation value before variable is removed.

- always_remove:

  Character vector. Any string in this vector, if matched, will always
  be removed (even if not correlated).

- always_keep:

  Character vector. Any string in this vector, if matched, will not be
  removed (even if correlated).

## Value

list with elements itemize
