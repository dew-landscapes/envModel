
# env variables, presence and background points code from:
# https://rspatial.org/sdm/3_sdm_absence-background.html#absence-and-background-points

# setup
library(predicts)
f <- system.file("ex/bio.tif", package="predicts")

# env variables
predictors <- rast(f)
predictors

# presences
file <- paste(system.file(package="predicts"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file,  header=TRUE,  sep=',')
presvals <- extract(predictors, bradypus[,-1])[-1]
presvals$pa <- 1

# backgrounds
backgr <- spatSample(predictors, 500, "random", as.points=TRUE, na.rm=TRUE)
absvals <- values(backgr)
absvals$pa <- 0

# presences + background
pb <- rbind(presvals, absvals)

# find correlated variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , thresh_corr = 0.9
                     )

names(result)

result$remove_corr

result$remove_rf

result$keep

# find 'unimportant' variables
result <- reduce_env(pb
                     , env_cols = names(predictors)
                     , y_col = "pa"
                     , thresh_corr = 1
                     , quant_rf_imp = 0.5
                     )

names(result)

result$remove_corr

result$remove_rf

result$keep
