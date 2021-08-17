
  random_forest <- function(df
                            , clustcol = "cluster"
                            , envcols = names(df[3:ncol(df)])
                            , trees = 999
                            , outfile
                            ) {

    x <- df[,which(names(df) %in% envcols)]
    y <- df %>% dplyr::pull(!!ensym(clustcol))

    # Assumes envData exists and is ready to go
    rf <- randomForest::randomForest(x = x
                                 , y = y
                                 , ntree = trees
                                 )

    conf <- caret::confusionMatrix(rf$predicted,df$cluster)

    rio::export(conf, outfile)

  }

  random_forest_good <- function(envdf, clustcol = "cluster"
                                 , sitecol = "cell", envnames
                                 , starttrees = 99, addtrees = 499
                                 , rfcores = 1, stabilise = TRUE
                                , outfile = fs::path("out","rfGood.rds")
                                 ) {

    rf_simple <- function(trees = starttrees, cores = rfcores, df = envdf) {

      # Parallel computation depends upon a parallel backend that must be registered before running foreach %dopar%
      # cores here is just used to help split up the task in foreach, it does not create the a parallel cluster

      x <- df[,which(names(df) %in% envnames)]
      y <- df[clustcol][[1]]

      # Assumes envData exists and is ready to go
      foreach(ntree=rep(ceiling(trees/cores), cores)
              , .combine=randomForest::combine
              , .packages = c("randomForest")
              , .export = c("useMtry")
      ) %dopar%
        randomForest::randomForest(x = x
                                   , y = y
                                   , ntree = ntree
                                   , importance = TRUE
                                   , mtry = useMtry
        )

    }

    # Iteratively add trees to a random forest with tibble output
    add_row_rf_simple <- function(resDf,rowGrow = addtrees) {

      prevRf <- resDf$rf[nrow(resDf)][[1]]

      nextRf <- rf_simple(rowGrow)

      newRf <- randomForest::combine(prevRf,nextRf)

      resDf %>%
        dplyr::bind_rows(tibble(start = Sys.time()
                                , run = max(resDf$run) + 1
                                ) %>%
          dplyr::mutate(trees = max(resDf$trees) + rowGrow
                        , rf = list(newRf)
                        #, rfProbCell = map(rf,rf_prob_cell)
                        #, meanVotesCell = map_dbl(rfProbCell,~mean(.$votes))
                        #, rfProbClass = map(rfProbCell,rf_prob_class)
                        #, meanVotesClass = map_dbl(rfProbClass,~mean(.$votes))
                        , deltaPrev = map_dbl(rf
                                              , ~tibble(last = .$predicted
                                                        , prev = prevRf$predicted
                                                        ) %>%
                                                dplyr::mutate(rows = nrow(.)
                                                              , same = last == prev
                                                              ) %>%
                                                dplyr::summarise(same = sum(same)/mean(rows)) %>%
                                                dplyr::pull(same)
                                              )
                        , kappaPrevRf = map_dbl(rf
                                                ,~caret::confusionMatrix(.$predicted
                                                                         ,prevRf$predicted
                                                                         )$overall[["Kappa"]]
                                                )
                        , end = Sys.time()
                        )
          ) %>%
        dplyr::mutate(seconds = lag(seconds, default = 0) + as.numeric(difftime(end,start, units = "secs")))

    }

    rfGood <- list()

    # Training control for caret implementation of machine learning methods
    ctrl <- caret::trainControl(method = "cv"
                                #, number = 3
                                #, repeats = 2
                                , savePredictions = TRUE
                                , verboseIter = TRUE
                                , allowParallel = TRUE
                                )

    # Tuning grid
    cTuneGrid <- expand.grid(.mtry = 1:floor(sqrt(length(envnames))))

    library(doParallel)
    cl <- makePSOCKcluster(rfcores)
    registerDoParallel(cl)

    rfGood$rfMtry <- caret::train(cluster ~ .
                           , data = envdf %>% dplyr::select(-!!ensym(sitecol))
                           , method = "rf"
                           , trControl = ctrl
                           , tuneGrid = cTuneGrid
                           , metric = "Kappa"
                           , trace = FALSE
                           )

    useMtry <- rfGood$rfMtry %>%
      `[[` ("finalModel") %>%
      `[[` ("mtry")

    rfGood$rfTrees <- tibble(start = Sys.time()
                      , run = 1
                      , trees = initialTrees
                      , rf = list(rf_simple(initialTrees))
                      #, rfProbCell = map(rf,rf_res)
                      #, meanVotesCell = map_dbl(rfProbCell,~mean(.$votes))
                      #, rfProbClass = map(rfProbCell,rf_prob_class)
                      #, meanVotesClass = map_dbl(rfProbClass,~mean(.$votes))
                      , end = Sys.time()
                      , seconds = as.numeric(difftime(end,start, units = "secs"))
                      ) %>%
      add_row_rf_simple(rowGrow = addtrees)

    if(stabilise) {

      while(as.logical((rfGood$rfTrees$kappaPrevRf[[nrow(rfGood$rfTrees)]] <= 0.995) *
                       (rfGood$rfTrees$deltaPrev[[nrow(rfGood$rfTrees)]] <= 0.995) *
                       (rfGood$rfTrees$rf[[nrow(rfGood$rfTrees)]]$ntree < 9999)
                       )
            ) {

        rfGood$rfTrees <- rfGood$rfTrees %>%
          add_row_rf_simple(rowGrow = addtrees)

        cat(
          paste0("ntree: ", rfGood$rfTrees$rf[[nrow(rfGood$rfTrees)]]$ntree
                 #, "\nmean votes cell: ",round(rfTrees$meanVotesCell[nrow(rfTrees)],4)
                 , "\n changed predictions: ",paste0(round(100-100*rfGood$rfTrees$deltaPrev[nrow(rfGood$rfTrees)],3),"%")
                 , "\n kappa based on confusion with last run: ", round(rfGood$rfTrees$kappaPrevRf[[nrow(rfGood$rfTrees)]],4)
                 , "\n time: ",round(rfGood$rfTrees$seconds[[nrow(rfGood$rfTrees)]],2)," seconds\n\n"
          )
        )

      }

    }

    stopCluster(cl)

    rio::export(rfGood,outfile)

    return(rfGood)

  }



