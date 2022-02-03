#' Run random forest, returning only diagnostic values from spatial cross
#' validation.
#'
#' Random forest is run via [randomForest::randomForest()].
#'
#' @param env_df Dataframe with clusters and environmental columns.
#' @param clust_col Character. Name of column with cluster membership.
#' @param folds Numeric. How many folds to use in cross-validation?
#' @param reps Numeric. How many repeats of cross-validation?
#' @param range_min,range_max Numeric. Metres. Minimum and maximum grid sizes
#' for spatial tiles. Reps will be made using `range <- seq(range_min, range_max
#' , length.out = reps)`
#' @param coords Character vector of length 2. Names of columns in `env_df` with
#' x and y coordinates.
#' @param crs_df Coordinate reference system for `coords`. Passed to the
#' `crs` argument of [sf::st_as_sf()]
#' @param crs_analysis. Coordinate reference system for the analysis. Should be
#' a projected crs so that `range_min` and `range_max` are interpreted
#' correctly.
#' @param set_min FALSE or numeric. If numeric, classes in `clust_col` with less
#' than `set_min` cases will be filtered.
#' @param ... passed to [envModel::make_rf_good()].
#'
#' @return
#' @export
#'
#' @examples

make_rf_diagnostics_spatialcv <- function(env_df
                                          , clust_col = "cluster"
                                          , folds = 3L
                                          , reps = 5L
                                          , range_min = 20000
                                          , range_max = 100000
                                          , coords = c("long", "lat")
                                          , crs_df = 4283
                                          , crs_analysis = 3577
                                          , ...
                                          ) {

  range <- seq(range_min, range_max, length.out = reps)

  env_df_sf <- env_df %>%
    na.omit() %>%
    sf::st_as_sf(coords = coords
                 , crs = crs_df
                 ) %>%
    sf::st_transform(crs = crs_analysis)

  area <- terra::ext(env_df_sf)

  res <- tibble::tibble(fold = 1:folds) %>%
    dplyr::inner_join(tibble::tibble(rep = 1:reps
                                     , res = range
                                     ) %>%
                        dplyr::mutate(ras = purrr::map(range
                                                , ~ terra::rast(extent = area
                                                                , resolution = .
                                                                , crs = sp::proj4string(sf::as_Spatial(env_df_sf))
                                                                , vals = 1:folds
                                                                )
                                                )
                                      )
                      , by = character()
                      ) %>%
    dplyr::mutate(ind = purrr::map(ras
                            , terra::extract
                            , terra::vect(env_df_sf)
                            )
                  , ind = purrr::map2(fold
                               , ind
                               , ~list(analysis = as.integer(.y$ID[.y$lyr.1 != .x]) %>% na.omit()
                                       , assessment = as.integer(.y$ID[.y$lyr.1 == .x]) %>% na.omit()
                                       )
                               )
                  , split = purrr::map(ind
                                , rsample::make_splits
                                , env_df
                                )
                  ) %>%
    dplyr::select(-ras, -ind) %>%
    dplyr::mutate(rf_good = purrr::map(split
                                       , function(x
                                                  , ...
                                                  ) make_rf_good(rsample::analysis(x)
                                                                 , internal_metrics = rsample::assessment(x)
                                                                 , clust_col = "cluster"
                                                                 , ...
                                                                 )
                                       , ...
                                       )
                  ) %>%
    dplyr::mutate(rf_res = purrr::map(rf_good, "rf_res")
                  , metrics = purrr::map(rf_good, "metrics")
                  , seconds = purrr::map_dbl(rf_good, "seconds")
                  ) %>%
    dplyr::select(fold, rep, rf_res, metrics, seconds) %>%
    tidyr::unnest(cols = c(rf_res)) %>%
    dplyr::select(-rf) %>%
    dplyr::arrange(fold, rep)

    stuff <- ls() %>% grep("res", ., value = TRUE, invert = TRUE)

    rm(list = stuff)

    return(res)

  }

