

#' Calculate diagnostics
#'
#' Takes vectors of truth and predicted cases and returns a range of `yardstick`
#' metrics.
#'
#' @param pred_df Dataframe of truth and predicted classes for each case.
#' @param truth_vec Vector of true classes.
#' @param pred_vec Vector of predicted classes.
#'
#' @return Tibble of metrics.
#' @export
#'
#' @examples
get_conf_metrics <- function(pred_df
                             , truth_vec = NULL
                             , pred_vec = NULL
                             ) {

  tib <- if(isTRUE(is.null(pred_df))) {

    tibble::tibble(truth = truth_vec
                   , pred = pred_vec
                   )

  } else pred_df

  tib_fac <- tib %>%
    dplyr::mutate(pred = factor(pred, levels = levels(.$truth)))

  mets <- yardstick::conf_mat(tib_fac
                              , truth
                              , pred
                              ) %>%
    summary() %>%
    dplyr::select(.metric,.estimate) %>%
    tidyr::pivot_wider(names_from = ".metric"
                       , values_from = ".estimate"
                       )

}
