

#' Calculate diagnostics
#'
#' Takes a data frame of truth and predicted cases. Returns overall or per
#' class results.
#'
#' @param df_pred Data frame containing truth and predicted classes.
#' @param by_class Logical. Return a row per class, or mean values across all
#' classes.
#'
#' @return Tibble
#' @export
#'
#' @examples
make_model_diagnostics <- function(df_pred, by_class) {



  #return all statistics
  if(by_class) {

    tibble::tibble(cluster = names(ua)
                   , sum_n = n
                   , sum_naive = th1
                   , sum_var = th1v
                   , sum_kappa = kh
                   , sum_kvar = khv
                   , user_naive = ua
                   , prod_naive = pa
                   , user_kappa = kpu
                   , user_kvar = kpuv
                   , prod_kappa = kpp
                   , prod_kvar = kppv
                   ) %>%
      dplyr::mutate(across(where(is.numeric),unname))

  } else {

    tibble::tibble(sum_n = n
                   , accuracy = th1
                   , accuracy_var = th1v
                   , kappa = kh
                   , kappa_var = khv
                   , user_naive = ua
                   , prod_naive = pa
                   , user_kappa = kpu
                   , user_kvar = kpuv
                   , prod_kappa = kpp
                   , prod_kvar = kppv
                   ) %>%
      dplyr::summarise(across(where(is.numeric),mean,na.rm = TRUE))


  }


}
