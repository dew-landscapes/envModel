
#' Generate correlation matrix and select variables to remove
#'
#' @param env_df Dataframe with environmental variables
#' @param env_cols Numeric or character vector defining columns in env_df
#' that contain the variables to test for correlation.
#' @param remove Logical. Remove variables correlated at or above `thresh`.
#' Correlation is found via [caret::findCorrelation()].
#' @param thresh Numeric. Threshold correlation value before variable is removed.
#' @param always_remove Character vector. Any string in this vector, if matched,
#' will always be removed (even if not correlated).
#' @param always_keep Character vector. Any string in this vector, if matched,
#' will not be removed (even if correlated).
#'
#' @return list with elements
#' itemize{
#'  \item{env_cols}{as provided}
#'  \item{always_remove}{as provided}
#'  \item{always_keep}{as provided}
#'  \item{remove}{as provided}
#'  \item{env_corr}{result of applying [stats::cor()] to `env_df[,env_cols]`}
#'  \item{highly_corr}{column names in `env_df` that are correlated with another
#'  column above `thresh`.}
#'  \item{remove_env}{column names to remove, based on inputs}
#' }
#' @export
#'
#' @examples
make_env_corr <- function(env_df
                          , env_cols
                          , remove = TRUE
                          , thresh = 0.95
                          , always_remove = c("lat", "long")
                          , always_keep = c("prec", "temp")
                          ) {

  if(!is.character(env_cols)) env_cols <- names(env_df)[env_cols]

  res <- list(env_cols = env_cols
              , always_remove = always_remove
              , always_keep = always_keep
              , thresh = thresh
              , remove = remove
              )

  dat_check <- env_df %>%
    dplyr::select(tidyselect::any_of(env_cols))

  res$remove_near_zero_var <- names(dat_check)[caret::nearZeroVar(dat_check)]

  dat_check <- dat_check |>
    dplyr::select(! res$remove_near_zero_var)

  res$env_corr <- dat_check |>
    stats::cor(use = "complete.obs")

  if(dim(res$env_corr)[2]) {

    res$highly_corr <- caret::findCorrelation(res$env_corr[!rownames(res$env_corr) %in% res$remove_near_zero_var
                                                           ,!colnames(res$env_corr) %in% res$remove_near_zero_var
                                                           ]
                                             , cutoff = thresh
                                             , names = TRUE
                                             )

    if(remove) {

      res$remove_env <- c(always_remove, res$remove_near_zero_var, res$highly_corr)

      res$remove_env <- res$remove_env[!res$remove_env %in% always_keep]

      res$remove_env <- sort(unique(res$remove_env))

    } else {

      res$remove_env <- c(res$remove_near_zero_var, always_remove)

    }

  }

  return(res)

}
