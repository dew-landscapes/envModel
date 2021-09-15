

#' Run random forest, usually returning only diagnostic values.
#'
#' Random forest is run via `caret::train`.
#'
#' @param df Dataframe with clusters, context and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param env_cols Character. Name of columns with environmental data.
#' @param trees Numeric. How many trees in the forest?
#' @param folds Numeric. How many folds to use in repeated cross-validation.
#' @param reps Numeric. How many reps to use in `rep`eated cross-validation.
#' @param tune_length Numeric. Passed to `caret::train` argument `tuneLength`.
#' @param save_rf Character. File name for saving the `train` object as (.rds).
#'
#' @return Tibble (row) of diagnostics
#'
#' @export
#'
#' @examples
make_rf_diagnostics_old <- function(df
                                , clust_col = "cluster"
                                , env_cols
                                , trees = 999
                                , folds = 3
                                , reps = 5
                                , tune_length = NULL
                                , save_rf = NULL
) {

  caret_train_method <- if(folds == 1) "none" else if (reps > 1) "repeatedcv" else "cv"

  reps <- if(caret_train_method != "repeatedcv") NA else reps
  folds <- if(!grepl("cv",caret_train_method)) NA else folds

  tune_length <- if(is.null(tune_length)) {

    ifelse(caret_train_method == "none", 1, 3)

  } else tune_length

  x_df <- df[,env_cols]
  y_vec <- df[clust_col][[1]]

  rf <- caret::train(x = x_df
                     , y = y_vec
                     , method = "rf"
                     , ntree = trees
                     , metric = "Kappa"
                     , tuneLength = tune_length
                     , trControl = caret::trainControl(method = caret_train_method
                                                       , number = folds
                                                       , repeats = reps
                                                       , allowParallel = FALSE
                     )
  )

  if(isTRUE(!is.null(save_rf))) rio::export(rf, save_rf)

  make_kappa_tibble(rf$finalModel$confusion[,-ncol(rf$finalModel$confusion)]
                    , by_class = FALSE
  )

}
