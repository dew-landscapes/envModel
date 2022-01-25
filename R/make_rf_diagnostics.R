

#' Run random forest, returning only diagnostic values.
#'
#'
#' @param env_df Dataframe with clusters and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param mlr3_cv_method Method to use with [mlr3::rmsp()] (as character, e.g.
#' "repeated_cv" or "repeated_cv".
#' @param x Character name of column containing x coordinates.
#' @param y Character name of column containing y coordinates.
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
                           , mlr3_cv_method = "repeated_cv"
                           , x = NULL
                           , y = NULL
                           ) {

    stopifnot(mlr3_cv_method %in% data.table::as.data.table(mlr_resamplings)$key)

    .clust_col = clust_col

    env_df_use <- if(!isFALSE(set_min)) {

      env_df %>%
        dplyr::add_count(!!rlang::ensym(clust_col)) %>%
        dplyr::filter(n > set_min) %>%
        dplyr::select(-n) %>%
        dplyr::mutate(!!clust_col := factor(!!rlang::ensym(clust_col)))

    } else env_df

    start_time <- Sys.time()

    if(!grepl("spcv|sptcv", mlr3_cv_method)) {

      #-----non-spatial------

      # task
      task <- mlr3::TaskClassif$new("env_rf"
                                    , backend = env_df_use
                                    , target = clust_col
                                    )

      task$col_roles$stratum <- clust_col

      # learner
      learner <- mlr3::lrn("classif.ranger")


      # spatial resampling
      re <- mlr3::rsmp(mlr3_cv_method
                          , folds = as.integer(folds)
                          , repeats = as.integer(reps)
                          )

      # sample
      results <- mlr3::resample(task
                                , learner
                                , re
                                )

      preds <- results$prediction()

      res <- envModel::get_conf_metrics(truth_vec = preds$truth
                                        , pred_vec = preds$response
                                        )

    } else if(grepl("spcv|sptcv", mlr3_cv_method)) {

      #-------spatial-------

      # sf object
      env_df_sf <- sf::st_as_sf(env_df_use
                                , coords = c(x, y)
                                )

      # task
      task <- mlr3spatiotempcv::TaskClassifST$new("env_rf"
                                                  , backend = env_df_sf
                                                  , target = clust_col
                                                  )

      task$col_roles$stratum <- clust_col

      # learner
      learner <- mlr3::lrn("classif.ranger")
      mlr3::set_threads(learner, n = )


      # spatial resampling
      re <- mlr3::rsmp(mlr3_cv_method
                       , nsplit = as.integer(c(ceiling(sqrt(folds)), floor(sqrt(folds))))
                       , repeats = as.integer(reps)
                       )

      # sample
      results <- mlr3::resample(task
                                , learner
                                , re
                                )

      preds <- results$prediction()

      res <- envModel::get_conf_metrics(truth_vec = preds$truth
                                        , pred_vec = preds$response
                                        )

    }

    res$seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    stuff <- ls() %>% grep("res", ., value = TRUE, invert = TRUE)

    rm(list = stuff)

    return(res)

  }

