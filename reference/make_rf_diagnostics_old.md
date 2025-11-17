# Run random forest, usually returning only diagnostic values.

Random forest is run via
[`caret::train`](https://rdrr.io/pkg/caret/man/train.html).

## Usage

``` r
make_rf_diagnostics_old(
  df,
  clust_col = "cluster",
  env_cols,
  trees = 999,
  folds = 3,
  reps = 5,
  tune_length = NULL,
  save_rf = NULL
)
```

## Arguments

- df:

  Dataframe with clusters, context and environmental columns.

- clust_col:

  Character. Name of column with cluster membership.

- env_cols:

  Character. Name of columns with environmental data.

- trees:

  Numeric. How many trees in the forest?

- folds:

  Numeric. How many folds to use in repeated cross-validation.

- reps:

  Numeric. How many reps to use in `rep`eated cross-validation.

- tune_length:

  Numeric. Passed to
  [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) argument
  `tuneLength`.

- save_rf:

  Character. File name for saving the `train` object as (.rds).

## Value

Tibble (row) of diagnostics
