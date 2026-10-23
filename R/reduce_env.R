
#' Reduce number of environmental variables
#'
#' Given a data frame of environmental data flag variables that: are correlated;
#' have near-zero variance; and have a high proportion of `NA` values.
#'
#' @param env_data Dataframe with environmental variables
#' @param env_cols Numeric or character vector defining columns in `env_data`
#' that contain the variables to test for correlation. Correlation is found via
#' [caret::findCorrelation()].
#' @param thresh_corr Numeric or `NULL`. Threshold correlation value above which
#' a variable is flagged as correlated. Set to `NULL` to skip removal of
#' any correlated variables.
#' @param thresh_NA Numeric or `NULL`. Threshold proportion of `NA` values
#' within a variable before it is removed. i.e. with a `thresh_NA` value of 0.1,
#' a variable with more than 10% `NA` values would be flagged for removal.
#' @param remove_always Character. Any matches will always be removed (even if
#' not correlated).
#' @param keep_always Character vector. Any string in this vector, if matched,
#' will not be removed (even if correlated).
#'
#' @return list with elements
#' itemize{
#'  \item{env_cols}{as provided}
#'  \item{remove_always}{as provided}
#'  \item{keep_always}{as provided}
#'  \item{remove_constant}{columns with only one value (or near-zero variance)}
#'  \item{env_corr}{result of applying [stats::cor()] to `env_data[,env_cols]`}
#'  \item{remove_corr}{column names in `env_data` that are correlated with another
#'  column above `thresh_corr`.}
#'  \item{test_na}{tibble of results of testing each column for `NA` values}
#'  \item{remove_NA}{character vector of columns to remove as their proportion
#'  of `NA` values > `thresh_NA`}
#'  \item{remove}{character vector of columns to remove from further
#'  analysis, based on results from `reduce_env()`}
#'  \item{keep}{character vector of `env_cols` that are not in `remove`}
#' }
#' @export
#'
#' @example inst/examples/reduce_env_ex.R
reduce_env <- function(env_data
                       , env_cols
                       , thresh_corr = 0.90
                       , thresh_NA = 0.03
                       , remove_always = c("lat", "long", "^id$")
                       , keep_always = NULL
                       ) {

  if(!is.character(env_cols)) env_cols <- names(env_data)[env_cols]

  res <- list(env_cols = env_cols
              , keep_always = if(!is.null(keep_always)) keep_always else NULL
              , thresh_corr = thresh_corr
              , thresh_NA = thresh_NA
              )

  # always -------
  res$remove_always <- env_cols[grepl(paste0(remove_always, collapse = "|"), env_cols)]

  # const -------
  env_df_no_factor <- env_data |>
     dplyr::mutate(dplyr::across(dplyr::where(is.factor)
                                 , as.numeric
                                 )
                   )

  res$check_low_var <- env_data |>
    dplyr::select(tidyselect::any_of(env_cols)) |>
    caret::nearZeroVar(saveMetrics = TRUE
                       , freqCut = 1000
                       , uniqueCut = 0.5
                       ) |>
    tibble::as_tibble(rownames = "env")

  res$remove_constant <- res$check_low_var$env[res$check_low_var$nzv]

  res$env_corr <- env_data |>
      dplyr::select(tidyselect::any_of(env_cols)) |>
      dplyr::select(!tidyselect::any_of(res$remove_constant)) |>
      stats::cor(use = "pairwise.complete.obs")

  # corr -------
  if(!is.null(thresh_corr)) {

    if(dim(res$env_corr)[2]) {

      res$remove_corr <- caret::findCorrelation(res$env_corr[!rownames(res$env_corr) %in% res$remove_constant
                                                             ,!colnames(res$env_corr) %in% res$remove_constant
                                                             ]
                                                , cutoff = thresh_corr
                                                , names = TRUE
                                                , exact = TRUE
                                                )

    }

  } else res$remove_corr <- NULL

  # NA ----------
  res$test_na <- env_data |>
    tidyr::pivot_longer(cols = tidyselect::any_of(env_cols)) |>
    dplyr::summarise(.by = name
                     , nas = sum(is.na(value))
                     , n = dplyr::n()
                     , prop_na = nas / n
                     )

  if(!is.null(thresh_NA)) {

    res$remove_na <- res$test_na$name[res$test_na$prop_na > thresh_NA]

  } else res$remove_na <- NULL

  # remove-------
  res$remove <- unique(c(res$remove_always
                         , res$remove_constant
                         , res$remove_corr
                         , res$remove_na
                         )
                       )

  if(!is.null(keep_always)) {

    res$remove <- res$remove[!grepl(paste0(keep_always, collapse = "|"), res$remove)]

  }

  res$remove <- sort(unique(res$remove))

  # keep -------
  res$keep <- res$env_cols[! res$env_cols %in% res$remove]

  return(res)

}
