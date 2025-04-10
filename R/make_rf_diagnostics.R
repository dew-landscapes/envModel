

#' Run random forest, returning only diagnostic values.
#'
#'
#' @param env_df Dataframe with clusters and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param down_sample Logical. If TRUE, the `sample.fraction` argument to
#' `ranger::ranger()` is set to the minimum number of sites in any one cluster
#' divided by the total number of sites.
#' @param range_m Numeric. The distance in metres (regardless of the unit
#' of the reference system of the input data) for block size(s) if using
#' [blockCV::spatialBlock()]. If reps > 1, an equivalent number of range_m
#' values are required to ensure the folds are different between repetitions.
#' `repeated_spcv_block`.
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param mlr3_cv_method Method to use with [mlr3::rsmp()] (as character, e.g.
#' "repeated_cv" or "repeated_spcv_block".
#' @param coords Character vector of length 2. Names of columns in `env_df` with
#' x and y coordinates.
#' @param crs_df Coordinate reference system for `coords`. Passed to the
#' `crs` argument of [sf::st_as_sf()].
#'
#' @return
#' @export
#'
#' @examples
  make_rf_diagnostics <- function(env_df
                           , clust_col = "cluster"
                           , folds = 3L
                           , reps = 5L
                           , down_sample = TRUE
                           , range_m = as.integer(seq(20000L, 100000L, length.out = reps))
                           , set_min = FALSE
                           , mlr3_cv_method = "repeated_cv"
                           , coords = c("long", "lat")
                           , crs_df = 4283
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

    samp_prop <- if(down_sample) {

        min(table(env_df_use[[clust_col]])) / nrow(env_df_use)

      } else {

        1 # sample.fraction = ifelse(replace, 1, 0.632) are the ranger::ranger defaults (and default replace = TRUE)

      }

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
      learner <- mlr3::lrn("classif.ranger"
                           , sample.fraction = samp_prop
                           )

      # resampling
      re <- mlr3::rsmp(mlr3_cv_method
                          , folds = as.integer(folds)
                          , repeats = as.integer(reps)
                          )

      # sample
      results <- mlr3::resample(task
                                , learner
                                , re
                                )

    } else if(grepl("spcv|sptcv", mlr3_cv_method)) {

      #-------spatial-------

      # sf object
      env_df_sf <- sf::st_as_sf(env_df_use
                                , coords = use_coords
                                , crs = crs_df
                                )

      # task
      task <- mlr3spatiotempcv::TaskClassifST$new("env_rf"
                                                  , backend = env_df_sf
                                                  , target = clust_col
                                                  )

      task$col_roles$stratum <- clust_col

      # learner
      learner <- mlr3::lrn("classif.ranger"
                           , sample.fraction = samp_prop
                           , mtry = to_tune(1, 10)
                           )

      # spatial resampling
      re <- mlr3::rsmp(mlr3_cv_method
                       , folds = as.integer(folds)
                       , repeats = as.integer(reps)
                       , range = range_m
                       )

      # sample
      results <- mlr3::resample(task
                                , learner
                                , re
                                )

    }

    res <- results$score() %>%
      dplyr::mutate(metrics = purrr::map(prediction_test
                                         , ~envModel::get_conf_metrics(truth_vec = .$truth
                                                                       , pred_vec = .$response
                                                                       )
                                         )
                    ) %>%
      dplyr::select(iteration, classif.ce, metrics) %>%
      tidyr::unnest(cols = c(metrics))

    res$seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    stuff <- ls() %>% grep("res", ., value = TRUE, invert = TRUE)

    rm(list = stuff)

    return(res)

  }

