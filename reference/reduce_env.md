# Reduce number of environmental variables

Given a data frame of environmental variables, flag correlated, and/or
important variables as precursor to further analysis. Importance is
taken from the variable importance result of
[`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html).
Thus it is not deterministic and can vary between runs.

## Usage

``` r
reduce_env(
  env_df,
  env_cols,
  y_col = "pa",
  y_col_factor = TRUE,
  imp_col = "1",
  thresh_corr = 0.9,
  quant_rf_imp = NULL,
  remove_always = c("lat", "long"),
  keep_always = NULL
)
```

## Arguments

- env_df:

  Dataframe with environmental variables

- env_cols:

  Numeric or character vector defining columns in env_df that contain
  the variables to test for correlation. Correlation is found via
  [`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html).

- y_col:

  Character. Name of a column in `env_df` to test for 'important'
  variables via
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html).
  Not used if `quant_rf_imp` is `NULL`.

- y_col_factor:

  Logical. Should `y_col` be considered a factor in
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)?
  Thus classification rather than regression. Not used if `quant_rf_imp`
  is `NULL`.

- imp_col:

  Character. Name of column in result of call to
  [`randomForest::importance()`](https://rdrr.io/pkg/randomForest/man/importance.html)
  to use in deciding 'importance' of a variable. Not used if
  `quant_rf_imp` is `NULL`.

- thresh_corr:

  Numeric or `NULL`. Threshold correlation value above which a variable
  is flagged as correlated. Set to `NULL` to skip removal of any
  correlated variables.

- quant_rf_imp:

  Numeric. Quantile below which a variable is flagged as not important.
  Set to `NULL` to skip removal of variables with low random forest
  importance.

- remove_always:

  Character. Any matches will always be removed (even if not
  correlated).

- keep_always:

  Character vector. Any string in this vector, if matched, will not be
  removed (even if correlated).

## Value

list with elements itemize

## Examples

``` r
# env variables, presence and background points code from:
# https://rspatial.org/sdm/3_sdm_absence-background.html#absence-and-background-points

# setup
library(predicts)
#> Error in library(predicts): there is no package called ‘predicts’
f <- system.file("ex/bio.tif", package="predicts")

# env variables
predictors <- rast(f)
#> Error in rast(f): could not find function "rast"
predictors
#> Error: object 'predictors' not found

# presences
file <- paste(system.file(package="predicts"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file,  header=TRUE,  sep=',')
#> Warning: cannot open file '/ex/bradypus.csv': No such file or directory
#> Error in file(file, "rt"): cannot open the connection
presvals <- extract(predictors, bradypus[,-1])[-1]
#> Error in extract(predictors, bradypus[, -1]): could not find function "extract"
presvals$pa <- 1
#> Error: object 'presvals' not found

# backgrounds
backgr <- spatSample(predictors, 500, "random", as.points=TRUE, na.rm=TRUE)
#> Error in spatSample(predictors, 500, "random", as.points = TRUE, na.rm = TRUE): could not find function "spatSample"
absvals <- values(backgr)
#> Error in values(backgr): could not find function "values"
absvals$pa <- 0
#> Error: object 'absvals' not found

# presences + background
pb <- rbind(presvals, absvals)
#> Error: object 'presvals' not found

# find correlated variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , thresh_corr = 0.9
                     )
#> Error: object 'predictors' not found

names(result)
#> Error: object 'result' not found

result$remove_corr
#> Error: object 'result' not found

result$remove_rf
#> Error: object 'result' not found

result$keep
#> Error: object 'result' not found

# find 'unimportant' variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , y_col = "pa"
                     , thresh_corr = NULL
                     , quant_rf_imp = 0.1
                     )
#> Error: object 'predictors' not found

names(result)
#> Error: object 'result' not found

result$remove_corr
#> Error: object 'result' not found

result$remove_rf
#> Error: object 'result' not found

result$keep
#> Error: object 'result' not found

# use both
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , y_col = "pa"
                     , thresh_corr = 0.95
                     , quant_rf_imp = 0.2
                     )
#> Error: object 'predictors' not found

names(result)
#> Error: object 'result' not found

result$remove_corr
#> Error: object 'result' not found

result$remove_rf
#> Error: object 'result' not found

result$keep
#> Error: object 'result' not found
```
