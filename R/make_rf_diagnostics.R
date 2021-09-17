

#' Run random forest, returning only diagnostic values.
#'
#' Random forest is run via `parsnip` with engine `randomForest`.
#'
#' @param env_df Dataframe with clusters, context and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param context Character. Name of column(s) defining the context.
#' @param env_cols Character. Name of columns with environmental data.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param use_trees Numeric. How many trees in the random forest?
#' @param use_mtry Numeric. `mtry` value used by random forest.
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param summarise_folds Logical. Return mean values for folds or each fold.
#'
#' @return
#' @export
#'
#' @examples
  make_rf_diagnostics <- function(env_df
                           , clust_col = "cluster"
                           , context = "cell"
                           , env_cols
                           , folds = 3
                           , reps = 2
                           , use_trees = 499
                           , use_mtry = floor(sqrt(sum(df_recipe$var_info$role == "predictor")))
                           , set_min = FALSE
                           , summarise_folds = folds > 1
                           ) {

    env_df_use <- if(!isFALSE(set_min)) {

      env_df %>%
        dplyr::add_count(!!ensym(clust_col)) %>%
        dplyr::filter(n > set_min) %>%
        dplyr::select(-n) %>%
        dplyr::mutate(!!clust_col := factor(!!ensym(clust_col)))

    } else env_df


    df_recipe <- recipes::recipe(as.formula(paste0(clust_col
                                                   , " ~ ."
                                                   )
                                            )
                                 , data = env_df_use
                                 ) %>%
      recipes::update_role(any_of(context), new_role = "context")


    df_mod <- parsnip::rand_forest(mode = "classification"
                                   , trees = use_trees
                                   , mtry = use_mtry
                                   ) %>%
      parsnip::set_engine("randomForest")


    df_workflow <- workflows::workflow() %>%
      workflows::add_model(df_mod) %>%
      workflows::add_recipe(df_recipe)

    df_cv <- env_df_use %>%
      rsample::vfold_cv(v = folds
                        , repeats = reps
                        , strata = !!ensym(clust_col)
                        )

    mets <- yardstick::metric_set(yardstick::accuracy
                                  , yardstick::kap
                                  , yardstick::sens
                                  , yardstick::spec
                                  , yardstick::ppv
                                  , yardstick::npv
                                  , yardstick::mcc
                                  , yardstick::j_index
                                  , yardstick::bal_accuracy
                                  , yardstick::detection_prevalence
                                  , yardstick::precision
                                  , yardstick::recall
                                  , yardstick::f_meas
                                  )

    df_fit_rs <- df_workflow %>%
      tune::fit_resamples(df_cv
                          , metrics = mets
                          )

    df_res <- tune::collect_metrics(df_fit_rs
                                    , summarize = FALSE
                                    )

    if(summarise_folds) {

      fold_col <- names(which.max(lapply(df_res, function(x) sum(grepl("fold|Fold",x)))))

      df_res <- df_res %>%
        dplyr::group_by(across(-c(!!ensym(fold_col),.estimate))) %>%
        dplyr::summarise(.estimate = mean(.estimate)) %>%
        dplyr::ungroup()

    }

    return(df_res)

  }

