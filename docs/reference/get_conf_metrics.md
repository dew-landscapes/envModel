# Calculate diagnostics

Takes vectors of truth and predicted cases and returns a range of
`yardstick` metrics.

## Usage

``` r
get_conf_metrics(pred_df, truth_vec = NULL, pred_vec = NULL)
```

## Arguments

- pred_df:

  Dataframe of truth and predicted classes for each case.

- truth_vec:

  Vector of true classes.

- pred_vec:

  Vector of predicted classes.

## Value

Tibble of metrics.
