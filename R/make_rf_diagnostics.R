

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
#' @param trees_start,trees_add,trees_max passed to `\link[envModel]{make_rf_good}`
#' @param accept_delta Proportion. What change in predictions (as trees are
#' added to the forest) is acceptable?
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
                           , trees_start = 499
                           , trees_add = 499
                           , trees_max = 9999
                           , accept_delta = formals(make_rf_good)$accept_prev_delta
                           ) {

    .context = context
    .env_names = env_names
    .trees_start = trees_start
    .trees_add = trees_add
    .trees_max = trees_max
    .use_mtry = use_mtry
    .accept_delta = accept_delta

    env_df_use <- if(!isFALSE(set_min)) {

      env_df %>%
        dplyr::add_count(!!ensym(clust_col)) %>%
        dplyr::filter(n > set_min) %>%
        dplyr::select(-n) %>%
        dplyr::mutate(!!clust_col := factor(!!ensym(clust_col)))

    } else env_df


    split <- rsample::vfold_cv(env_df_use
                               , v = folds
                               , repeats = reps
                               , strata = !!ensym(clust_col)
                               )


    rf_res <- split %>%
      dplyr::mutate(rf = map(splits
                             , ~make_rf_good(rsample::analysis(.)
                                             , context = .context
                                             , env_names = .env_names
                                             , use_mtry = .use_mtry
                                             , internal_metrics = rsample::assessment(.)
                                             , trees_start = .trees_start
                                             , trees_add = .trees_add
                                             , trees_max = .trees_max
                                             , accept_delta = .accept_delta
                                             )
                             )
                    ) %>%
      dplyr::mutate(metrics = map_chr(rf,"metrics")
                    , mtry = map_dbl(rf,"mtry")
                    , seconds = map_dbl(rf, "seconds")
                    , rf_res = map(rf, "rf_res")
                    # , rf_res = map(rf_res
                    #                , . %>%
                    #                  dplyr::slice(nrow(.))
                    #                )
                    ) %>%
      dplyr::select(-rf) %>%
      tidyr::unnest(cols = c(rf_res)) %>%
      dplyr::select(negate(where(is.list)))

    rf_res <- if("id2" %in% names(rf_res)) {

        rf_res %>%
          dplyr::rename(folds = id2, reps = id)

      } else {

        rf_res %>%
          dplyr::rename(folds = id)

      }

    if(summarise_folds) {

      rf_res <- rf_res %>%
        dplyr::group_by(across(contains("reps"))
                        , metrics
                        , mtry
                        ) %>%
        dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
        dplyr::ungroup()

    }

    return(rf_res)

  }

