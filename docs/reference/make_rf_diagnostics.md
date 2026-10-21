# Run random forest, returning only diagnostic values.

Run random forest, returning only diagnostic values.

## Usage

``` r
make_rf_diagnostics(
  env_df,
  clust_col = "cluster",
  folds = 3L,
  reps = 5L,
  trees = 999,
  down_sample = TRUE,
  range_m = as.integer(seq(20000L, 100000L, length.out = reps)),
  set_min = FALSE,
  mlr3_cv_method = "repeated_cv",
  coords = c("long", "lat"),
  crs_df = 4283
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

- trees:

  Numeric. num.trees parameter in
  [`mlr3::lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  (with random classification forest using
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  from package ranger).

- down_sample:

  Logical or numeric. If TRUE, the `sample.fraction` argument to
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  is set to the minimum number of sites in any one cluster divided by
  the number of sites in each cluster. If numeric, it is provided as-is
  to the `sample.fraction` argument. If `FALSE` the default
  `down_sample` argument of
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  is used.

- range_m:

  Numeric. The distance in metres (regardless of the unit of the
  reference system of the input data) for block size(s) if using
  [`blockCV::spatialBlock()`](https://rdrr.io/pkg/blockCV/man/spatialBlock.html).
  If reps \> 1, an equivalent number of range_m values are required to
  ensure the folds are different between repetitions.
  `repeated_spcv_block`. Only needed if using spatial cross validation.

- set_min:

  FALSE or numeric. If numeric, classes in `clust_col` with less than
  `set_min` cases will be filtered.

- mlr3_cv_method:

  Method to use with
  [`mlr3::rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  (as character, e.g. "repeated_cv" or "repeated_spcv_block".

- coords:

  Character vector of length 2. Names of columns in `env_df` with x and
  y coordinates. Only needed if using spatial cross validation.

- crs_df:

  Coordinate reference system for `coords`. Passed to the `crs` argument
  of
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).
  Only needed if using spatial cross validation.
