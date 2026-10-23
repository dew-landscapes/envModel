

make_rf_quick <- function(x_df
                          , y_vec
                          , trees = 499
                          , cl_obj = NULL
                          , do_imp = FALSE
                          , ...
                          ) {

  if(isTRUE(!is.null(cl_obj))) {

    rf_cores <- length(cl_obj)

    trees_per_worker <- rep(trees %/% rf_cores
                            , rf_cores
                            )

    remainder <- trees %% rf_cores

    if (remainder > 0) {
      trees_per_worker[seq_len(remainder)] <- trees_per_worker[seq_len(remainder)] + 1L
    }

    `%dopar%` <- foreach::`%dopar%`

    foreach::foreach(ntree = trees_per_worker
                     , .combine = randomForest::combine
                     , .packages = c("randomForest")
                     ) %dopar%
      randomForest::randomForest(x = x_df
                                 , y = y_vec
                                 , importance = do_imp
                                 , ntree = ntree
                                 , ...
                                 )

  } else {

    randomForest::randomForest(x = x_df
                               , y = y_vec
                               , importance = do_imp
                               , ntree = trees
                               , ...
                               )

  }

}

#' Iteratively add trees to random forest
#'
#' Trees are added until predictions stabilise or a maximum number of trees is
#' reached.
#'
#' @param env_df Dataframe with `clust_col`, `site_col` and columns `env_names`.
#' @param clust_col Character. Name of the columns with clusters.
#' @param env_names Character. Name of the environmental variables (e.g.
#' `names(stack_list)`).
#' @param trees_start Number of trees in first random forest run.
#' @param trees_add Number of trees to add in each subsequent run.
#' @param trees_max Maximum number of trees in the random forest.
#' @param rf_cores Number of cores to use for parallel processing.
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` rows will be filtered
#' @param stopping_metric Character. Either "changed_pred" (proportion of
#' predictions that change between runs) or any metric returned by a summary of
#' `yardstick::conf_mat()`
#' @param stopping_tolerance What proportion change between runs is acceptable?
#' @param stopping_runs How many runs (in a row) need to beat
#' `stopping_tolerance`?
#' @param test_data Either `FALSE` to test against the training data, or new
#' data in same format as `env_df`, against which to calculate metrics.
#' @param do_imp Logical. Passed to `importance` argument of
#' `randomForest::randomForest()`.
#' @param keep_rf Logical. If true, `randomForest` object will be included in
#' output. Defaults to `FALSE` to save memory.
#' @param out_file Optional name (and path) of file to save results
#' @param force_new Logical. If `out_file` already exists, should that file be
#' loaded (`FALSE`) or run again (`TRUE`) from scratch (and written over).
#' @param ... Passed (via `make_rf_quick()`) to `randomForest::randomForest()`.
#'
#' @return
#' @export
#'
#' @examples
run_iterative_rf <- function(env_df
                             , clust_col = "cluster"
                             , env_names
                             , trees_start = 499
                             , trees_add = 249
                             , trees_max = 9999
                             , rf_cores = 1
                             , set_min = FALSE
                             , stopping_metric = "f_meas"
                             , stopping_tolerance = 0.005
                             , stopping_runs = 3
                             , test_data = FALSE
                             , do_imp = FALSE
                             , keep_rf = FALSE
                             , out_file = NULL
                             , force_new = FALSE
                             , verbose = base::interactive()
                             , ...
                             ) {

  .do_imp = do_imp

  go_time <- Sys.time()

  if(isTRUE(!is.null(out_file))) {

    out_file <- paste0(fs::path_ext_remove(out_file)
                       , ".rds"
                       )

  }

  if(all(!is.null(out_file) && file.exists(out_file), !force_new)) {

    result <- rio::import(out_file)

  } else {

    if(!isFALSE(set_min)) {

      env_df <- env_df |>
        dplyr::add_count(!!rlang::ensym(clust_col)) |>
        dplyr::filter(n > set_min) |>
        dplyr::select(-n) |>
        dplyr::mutate(!!clust_col := factor(!!rlang::ensym(clust_col)))

    }

    x <- env_df[,which(names(env_df) %in% env_names)]
    y <- factor(env_df[clust_col][[1]])

    result <- list()

    # setup parallel cluster
    if(rf_cores > 1) {

      cl <- parallel::makePSOCKcluster(rf_cores)
      doParallel::registerDoParallel(cl)

      on.exit(parallel::stopCluster(cl), add = TRUE)

    }

    get_truth_pred <- function(test, rf) {

      if(isFALSE(test)) {

        truth <- y
        pred <- rf$predicted


      } else {

        new_data <- test[, env_names]

        truth <- test[[clust_col]]

        pred <- predict(rf, newdata = new_data)

      }

      tibble::tibble(truth, pred)

    }

    rf <- make_rf_quick(x
                        , y
                        , trees = trees_start
                        , if(rf_cores > 1) cl_obj = cl
                        , do_imp = .do_imp
                        , ...
                        )

    metrics <- get_conf_metrics(get_truth_pred(test_data
                                               , rf
                                               )
                                )

    if(keep_rf) result$rf <- rf

    result$metrics <- tibble::tibble(trees = rf$ntree) |>
      dplyr::mutate(metrics = list(metrics)
                    , changed_pred = NA
                    ) |>
      tidyr::unnest(cols = c(metrics))

    counter <- 0

    if(verbose) {

      message("Initial forest trees: ", result$metrics$trees[[nrow(result$metrics)]]
              , "\n elapsed time: ", as.numeric(round(difftime(Sys.time(), go_time, units = "secs"),2)), " seconds\n\n"
              )

    }

    while(
      all(counter == 0
          , result$metrics$trees[[nrow(result$metrics)]] < trees_max
          )
      ) {

      old_rf <- rf

      next_rf <- make_rf_quick(x
                               , y
                               , trees = trees_add
                               , if(rf_cores > 1) cl_obj = cl
                               , do_imp = .do_imp
                               , ...
                               )

      rf <- randomForest::combine(rf, next_rf)

      metrics <- get_conf_metrics(get_truth_pred(test_data
                                                  , rf
                                                  )
                                   )

      if(keep_rf) result$rf <- rf

      result$metrics <- result$metrics |>
        dplyr::bind_rows(tibble::tibble(trees = rf$ntree) |>
                           dplyr::mutate(metrics = list(metrics)) |>
                           tidyr::unnest(cols = c(metrics))
                         )

      # add in latest 'changed_pred'
      result$metrics$changed_pred[nrow(result$metrics)] <- sum(old_rf$predicted != rf$predicted) / length(y)

      # running mean on stopping metric
      mean_name <- paste0(stopping_metric, "_run_mean")
      max_name <- paste0(stopping_metric, "_run_max")
      test_name <- paste0(stopping_metric, "_test")

      result$metrics <- result$metrics |>
        dplyr::mutate(!!rlang::ensym(mean_name) := slider::slide_dbl(result$metrics[[stopping_metric]]
                                                                    , mean
                                                                    , .before = stopping_runs - 1
                                                                    , .complete = TRUE
                                                                    )
                      , !!rlang::ensym(max_name) := slider::slide_dbl(result$metrics[[stopping_metric]]
                                                                      , max
                                                                      , .before = stopping_runs - 1
                                                                      , .complete = TRUE
                                                                      )
                      , !!rlang::ensym(test_name) := !!rlang::ensym(max_name) / !!rlang::ensym(mean_name)
                      , thresh = !!rlang::ensym(test_name) <= 1 + stopping_tolerance
                      )

      if(isTRUE(result$metrics$thresh[nrow(result$metrics)])) counter <- counter + 1

      if(verbose) {

        message("Combined forest trees: ", rf$ntree
                , "\n ", stopping_metric, ": ", round(tail(result$metrics[[stopping_metric]], 1), 5)
                , "\n running mean of ", stopping_metric, " over ", stopping_runs, " runs: ", round(tail(result$metrics[[mean_name]], 1), 5)
                , "\n running max of ", stopping_metric, " over ", stopping_runs, " runs: ", round(tail(result$metrics[[max_name]], 1), 5)
                , "\n stop when thresh (running max ", stopping_metric, " / running mean ", stopping_metric, "): ", round(tail(result$metrics[[test_name]], 1), 5)
                , " < ", 1 + stopping_tolerance
                , "\n elapsed time: ", as.numeric(round(difftime(Sys.time(), go_time, units = "secs"), 2)), " seconds\n"
                )

      }

    }

    if(! result$metrics$thresh[[nrow(result$metrics)]]) warning(rf$ntree, " > ", trees_max, " stopping without meeting stopping rules")

    result$testing <- if(isFALSE(test_data)) "internal" else "new data"

    result$seconds <- round(as.numeric(difftime(Sys.time(), go_time, units = "secs")),2)

    if(!isTRUE(is.null(out_file))) saveRDS(result, out_file)

  }

  return(result)

}



