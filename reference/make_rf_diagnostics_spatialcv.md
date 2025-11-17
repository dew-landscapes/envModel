# Run random forest, returning only diagnostic values from spatial cross validation.

Random forest is run via
[`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html).

## Usage

``` r
make_rf_diagnostics_spatialcv(
  env_df,
  clust_col = "cluster",
  folds = 3L,
  reps = 5L,
  range_min = 20000,
  range_max = 1e+05,
  coords = c("long", "lat"),
  crs_df = 4283,
  crs_analysis = 3577,
  ...
)
```

## Arguments

- env_df:

  Dataframe with clusters and environmental columns.

- clust_col:

  Character. Name of column with cluster membership.

- folds:

  Numeric. How many folds to use in cross-validation?

- reps:

  Numeric. How many repeats of cross-validation?

- range_min, range_max:

  Numeric. Metres. Minimum and maximum grid sizes for spatial tiles.
  Reps will be made using
  `range <- seq(range_min, range_max , length.out = reps)`

- coords:

  Character vector of length 2. Names of columns in `env_df` with x and
  y coordinates.

- crs_df:

  Coordinate reference system for `coords`. Passed to the `crs` argument
  of
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- ...:

  passed to
  [`make_rf_good()`](https://acanthiza.github.io/envModel/reference/make_rf_good.md).
