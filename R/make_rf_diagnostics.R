

#' Run random forest, returning only diagnostic values.
#'
#' Random forest is run via [randomForest::randomForest()].
#'
#' @param env_df Dataframe with clusters and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param summarise_folds Logical. Return mean values for folds or each fold.
#' @param ... passed to [envModel::make_rf_good()].
#'
#' @return
#' @export
#'
#' @examples
  make_rf_diagnostics <- function(env_df
                           , clust_col = "cluster"
                           , folds = 3
                           , reps = 5
                           , set_min = FALSE
                           , summarise_folds = TRUE
                           , ...
                           ) {

    .clust_col = clust_col

    env_df_use <- if(!isFALSE(set_min)) {

      env_df %>%
        dplyr::add_count(!!rlang::ensym(clust_col)) %>%
        dplyr::filter(n > set_min) %>%
        dplyr::select(-n) %>%
        dplyr::mutate(!!clust_col := factor(!!rlang::ensym(clust_col)))

    } else env_df

    # getting ellipses (...) to work with purrr::map, see
    # https://stackoverflow.com/questions/48215325/passing-ellipsis-arguments-to-map-function-purrr-package-r
    # answer by Matifou

    splits <- rsample::vfold_cv(env_df_use
                               , v = folds
                               , repeats = reps
                               , strata = !!rlang::ensym(clust_col)
                               ) %>%
      dplyr::mutate(rf = purrr::map(splits
                                    , function(x, ...) make_rf_good(rsample::analysis(x)
                                                                    , internal_metrics = rsample::assessment(x)
                                                                    , clust_col = .clust_col
                                                                    , ...
                                                                    )
                                    , ...
                                    )
                    ) %>%
      dplyr::mutate(metrics = purrr::map_chr(rf,"metrics")
                    , mtry = purrr::map_dbl(rf,"mtry")
                    , seconds = purrr::map_dbl(rf, "seconds")
                    , rf_res = purrr::map(rf, "rf_res")
                    ) %>%
      dplyr::select(-rf) %>%
      tidyr::unnest(cols = c(rf_res)) %>%
      dplyr::select(negate(where(is.list)))

    splits <- if("id2" %in% names(splits)) {

        splits %>%
          dplyr::rename(folds = id2, reps = id)

      } else {

        splits %>%
          dplyr::rename(folds = id)

      }

    if(summarise_folds) {

      splits <- splits %>%
        dplyr::group_by(across(contains("reps"))
                        , metrics
                        , mtry
                        ) %>%
        dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
        dplyr::ungroup()

    }

    stuff <- ls() %>% grep("splits", ., value = TRUE, invert = TRUE)

    rm(list = stuff)

    return(splits)

  }

