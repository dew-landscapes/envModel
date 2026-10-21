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
#> Loading required package: terra
#> terra 1.9.1
f <- system.file("ex/bio.tif", package="predicts")

# env variables
predictors <- rast(f)
predictors
#> class       : SpatRaster 
#> size        : 192, 186, 9  (nrow, ncol, nlyr)
#> resolution  : 0.5, 0.5  (x, y)
#> extent      : -125, -32, -56, 40  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : bio.tif 
#> names       : bio1, bio5, bio6, bio7, bio8, bio9, ... 
#> min values  :  -23,   61, -212,   60,  -66,  -23, ... 
#> max values  :  289,  422,  242,  461,  323,  289, ... 

# presences
file <- paste(system.file(package="predicts"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file,  header=TRUE,  sep=',')
presvals <- extract(predictors, bradypus[,-1])[-1]
presvals$pa <- 1

# backgrounds
backgr <- spatSample(predictors, 500, "random", as.points=TRUE, na.rm=TRUE)
absvals <- values(backgr)
absvals$pa <- 0

# presences + background
pb <- rbind(presvals, absvals)

# find correlated variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , thresh_corr = 0.9
                     )

names(result)
#>  [1] "env_cols"        "remove_always"   "keep_always"     "thresh_corr"    
#>  [5] "quant_rf_imp"    "check_low_var"   "remove_constant" "env_corr"       
#>  [9] "remove_corr"     "remove"          "keep"           

result$remove_corr
#> [1] "bio6"  "bio1"  "bio16"

result$remove_rf
#> NULL

result$keep
#> [1] "bio5"  "bio7"  "bio8"  "bio9"  "bio12" "bio17"

# find 'unimportant' variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , y_col = "pa"
                     , thresh_corr = NULL
                     , quant_rf_imp = 0.1
                     )

names(result)
#>  [1] "env_cols"        "remove_always"   "keep_always"     "thresh_corr"    
#>  [5] "quant_rf_imp"    "check_low_var"   "remove_constant" "env_corr"       
#>  [9] "rf"              "rf_imp"          "remove_rf"       "remove"         
#> [13] "keep"           

result$remove_corr
#> NULL

result$remove_rf
#> [1] "bio9"

result$keep
#> [1] "bio1"  "bio5"  "bio6"  "bio7"  "bio8"  "bio12" "bio16" "bio17"

# use both
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , y_col = "pa"
                     , thresh_corr = 0.95
                     , quant_rf_imp = 0.2
                     )

names(result)
#>  [1] "env_cols"        "remove_always"   "keep_always"     "thresh_corr"    
#>  [5] "quant_rf_imp"    "check_low_var"   "remove_constant" "env_corr"       
#>  [9] "rf"              "rf_imp"          "remove_rf"       "remove_corr"    
#> [13] "remove"          "keep"           

result$remove_corr
#> [1] "bio1"

result$remove_rf
#> [1] "bio9" "bio1"

result$keep
#> [1] "bio5"  "bio6"  "bio7"  "bio8"  "bio12" "bio16" "bio17"
```
