

#' Run random forest, returning only diagnostic values.
#'
#'
#' @param env_df Dataframe with clusters and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param `spatial_cv` Logical. If `FALSE` workflow follows a tidymodels
#' workflow (with cv using [rsample::vfold_cv()]). If `TRUE` follows a `mlr3`
#' workflow (with cv using [mlr3::rsmp()] with "repeated_spcv_tiles").
#' @param x Character name of column containing x coordinates.
#' @param y Character name of column containing y coordinates.
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
                           , spatial_cv = FALSE
                           , x = NULL
                           , y = NULL
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

    if(!spatial_cv) {

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
      dplyr::select(Negate(where(is.list)))

    res <- if("id2" %in% names(splits)) {

        splits %>%
          dplyr::rename(folds = id2, reps = id)

      } else {

        splits %>%
          dplyr::rename(folds = id)

      }

    stuff <- ls() %>% grep("res", ., value = TRUE, invert = TRUE)

    rm(list = stuff)

    } else if(spatial_cv) {

      # sf object
      env_df_sf <- sf::st_as_sf(env_df_use
                                , coords = c(x, y)
                                )

      # task
      task <- mlr3spatiotempcv::TaskClassifST$new("env_sf"
                                                  , backend = env_df_sf
                                                  , target = "cluster"
                                                  )

      # learner
      learner <- mlr3::lrn("classif.ranger")


      # spatial resampling
      sp_re <- mlr3::rsmp("repeated_spcv_tiles"
                                          , nsplit = as.integer(c(ceiling(sqrt(folds)), floor(sqrt(folds))))
                                          , repeats = as.integer(reps)
                                          )

      # sample
      results <- mlr3::resample(task
                                , learner
                                , sp_re
                                )

      preds <- results$prediction()

      res <- envModel::get_conf_metrics(truth_vec = preds$truth
                                        , pred_vec = preds$response
                                        )

      stuff <- ls() %>% grep("res", ., value = TRUE, invert = TRUE)

      rm(list = stuff)

    }

    return(res)

  }

