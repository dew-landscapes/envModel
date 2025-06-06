
#' Reduce number of environmental variables
#'
#' Given a data frame of environmental variables, flag correlated, and/or
#' important variables as precursor to further analysis. Importance is taken
#' from the variable importance result of `randomForest::randomForest()`. Thus
#' it is not deterministic and can vary between runs.
#'
#' @param env_df Dataframe with environmental variables
#' @param env_cols Numeric or character vector defining columns in env_df
#' that contain the variables to test for correlation. Correlation is found via
#' [caret::findCorrelation()].
#' @param y_col Character. Name of a column in `env_df` to test for
#' 'important' variables via `randomForest::randomForest()`. Not used if
#' `quant_rf_imp` is `NULL`.
#' @param y_col_factor Logical. Should `y_col` be considered a
#' factor in `randomForest::randomForest()`? Thus classification rather than
#' regression. Not used if `quant_rf_imp` is `NULL`.
#' @param imp_col Character. Name of column in result of call to
#' `randomForest::importance()` to use in deciding 'importance' of a variable.
#' Not used if `quant_rf_imp` is `NULL`.
#' @param thresh_corr Numeric or `NULL`. Threshold correlation value above which
#' a variable is flagged as correlated. Set to `NULL` to skip removal of
#' any correlated variables.
#' @param quant_rf_imp Numeric. Quantile below which a variable is flagged as
#' not important. Set to `NULL` to skip removal of variables with low random
#' forest importance.
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
#'  \item{remove_constant}{columns with only one value}
#'  \item{env_corr}{result of applying [stats::cor()] to `env_df[,env_cols]`}
#'  \item{remove_corr}{column names in `env_df` that are correlated with another
#'  column above `thresh_corr`.}
#'  \item{rf}{result of applying `randomforest::randomforest()` in the form of
#'  y_col ~ env_cols}
#'  \item{rf_vi}{variable importance from rf}
#'  \item{remove}{character vector of columns to remove from further
#'  analysis, based on results from `reduce_env()`}
#' }
#' @export
#'
#' @example inst/examples/reduce_env_ex.R
reduce_env <- function(env_df
                       , env_cols
                       , y_col = "pa"
                       , y_col_factor = TRUE
                       , imp_col = "1"
                       , thresh_corr = 0.90
                       , quant_rf_imp = NULL
                       , remove_always = c("lat", "long")
                       , keep_always = NULL
                       ) {

  if(!is.character(env_cols)) env_cols <- names(env_df)[env_cols]

  res <- list(env_cols = env_cols
              , remove_always = remove_always
              , keep_always = if(!is.null(keep_always)) keep_always else NULL
              , thresh_corr = thresh_corr
              , quant_rf_imp = quant_rf_imp
              )

  # const -------
  env_df_no_factor <- env_df %>%
     dplyr::mutate(dplyr::across(dplyr::where(is.factor)
                                 , as.numeric
                                 )
                   )

  res$remove_constant <- names(env_df_no_factor[sapply(env_df_no_factor, function(v) var(v, na.rm=TRUE)==0)])

  res$env_corr <- env_df %>%
      dplyr::select(tidyselect::any_of(env_cols)) %>%
      dplyr::select(!tidyselect::any_of(res$remove_constant)) %>%
      stats::cor(use = "complete.obs")

  # rf -------
  if(!is.null(quant_rf_imp)) {

    rf_dat <- env_df %>%
      dplyr::select(!tidyselect::any_of(c(res$remove_constant)))

    y <- rf_dat %>%
      dplyr::pull(!!rlang::ensym(y_col)) %>%
      {if(y_col_factor) as.factor(.) else (.)}

    x <- rf_dat %>%
      dplyr::select(tidyselect::any_of(env_cols)) %>%
      as.matrix()

    tab <- table(y)
    min_class <- min(tab)
    classes <- length(tab)

    res$rf <- randomForest::randomForest(
      x = x
      , y = y
      , strata = y
      , importance = TRUE
      , sampsize = rep(min_class, classes)
      )

    res$rf_imp <- randomForest::importance(res$rf) %>%
      tibble::as_tibble(rownames = "env") %>%
      dplyr::arrange(!!rlang::ensym(imp_col)) %>%
      dplyr::mutate(imp = !!rlang::ensym(imp_col) > stats::quantile(!!rlang::ensym(imp_col), probs = quant_rf_imp))

    res$remove_rf <- res$rf_imp %>% dplyr::filter(!imp) %>% dplyr::pull(env)

  } else res$remove_rf <- NULL

  # corr -------
  if(!is.null(thresh_corr)) {

    if(dim(res$env_corr)[2]) {

      res$remove_corr <- caret::findCorrelation(res$env_corr[!rownames(res$env_corr) %in% res$remove_constant
                                                             ,!colnames(res$env_corr) %in% res$remove_constant
                                                             ]
                                                , cutoff = thresh_corr
                                                , names = TRUE
                                                )

    }

  } else res$remove_corr <- NULL

  # remove-------
  res$remove <- unique(c(res$remove_corr, res$remove_constant, res$remove_rf, remove_always))

  if(!is.null(keep_always)) {

    res$remove <- res$remove[!grepl(paste0(keep_always, collapse = "|"), res$remove)]

  }

  res$remove <- sort(unique(res$remove))

  # keep -------
  res$keep <- res$env_cols[! res$env_cols %in% res$remove]

  return(res)

}
