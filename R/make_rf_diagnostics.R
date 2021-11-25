

#' Run random forest, returning only diagnostic values.
#'
#' Random forest is run via `parsnip` with engine `randomForest`.
#'
#' @param env_df Dataframe with clusters, context and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param context Character. Name of column(s) defining the context.
#' @param env_names Character. Name of columns with environmental data.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param use_mtry Numeric. `mtry` value used by random forest.
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param summarise_folds Logical. Return mean values for folds or each fold.
#' @param ... passed to `\link[envModel]{make_rf_good}`
#'
#' @return
#' @export
#'
#' @examples
  make_rf_diagnostics <- function(env_df
                           , clust_col = "cluster"
                           , context = "cell"
                           , env_names
                           , folds = 3
                           , reps = 5
                           , use_mtry = floor(sqrt(length(env_names)))
                           , set_min = FALSE
                           , summarise_folds = TRUE
                           , ...
                           ) {

    .clust_col = clust_col
    .context = context
    .env_names = env_names
    .use_mtry = use_mtry

    env_df_use <- if(!isFALSE(set_min)) {

      env_df %>%
        dplyr::add_count(!!ensym(clust_col)) %>%
        dplyr::filter(n > set_min) %>%
        dplyr::select(-n) %>%
        dplyr::mutate(!!clust_col := factor(!!ensym(clust_col)))

    } else env_df


    splits <- rsample::vfold_cv(env_df_use
                               , v = folds
                               , repeats = reps
                               , strata = !!ensym(clust_col)
                               ) %>%
      dplyr::mutate(rf = purrr::map(splits
                                    , ~make_rf_good(rsample::analysis(.)
                                                    , clust_col = .clust_col
                                                    , context = .context
                                                    , env_names = .env_names
                                                    , use_mtry = .use_mtry
                                                    , internal_metrics = rsample::assessment(.)
                                                    , ...
                                                    )
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

