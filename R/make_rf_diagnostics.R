

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
#' @param ntrees Numeric. How many trees in the random forest?
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
                           , ntrees = 999
                           ) {

    df_recipe <- recipes::recipe(as.formula(paste0(clust_col
                                                   , " ~ ."
                                                   )
                                            )
                                 , data = env_df
                                 ) %>%
      recipes::update_role(any_of(context), new_role = "context") %>%
      recipes::prep()

    df_mod <- parsnip::rand_forest(mode = "classification"
                                   , trees = ntrees
                                   ) %>%
      parsnip::set_engine("randomForest")

    df_workflow <- workflows::workflow() %>%
      workflows::add_model(df_mod) %>%
      workflows::add_recipe(df_recipe)

    df_cv <- rsample::vfold_cv(env_df
                               , v = folds
                               , repeats = reps
                               , strata = all_of(clust_col)
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

    tune::collect_metrics(df_fit_rs) %>%
      dplyr::select(.metric,mean) %>%
      tidyr::pivot_wider(names_from = ".metric"
                         , values_from = "mean"
                         )

  }
