% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rf.R
\name{make_rf_good}
\alias{make_rf_good}
\title{Iteratively add trees to random forest until predictions stabilise}
\usage{
make_rf_good(
  env_df,
  clust_col = "cluster",
  env_names,
  trees_start = 499,
  trees_add = 249,
  trees_max = 9999,
  rf_cores = 1,
  use_mtry = NULL,
  set_min = FALSE,
  accept_delta = 0.995,
  accept_run = 3,
  internal_metrics = TRUE,
  do_imp = FALSE,
  keep_rf = FALSE,
  out_file = NULL,
  save_res = FALSE,
  do_gc = TRUE,
  ...
)
}
\arguments{
\item{env_df}{Dataframe with \code{clust_col}, \code{site_col} and columns \code{env_names}.}

\item{clust_col}{Character. Name of the columns with clusters.}

\item{env_names}{Character. Name of the environmental variables (e.g.
\code{names(stack_list)}).}

\item{trees_start}{Number of trees in first random forest run.}

\item{trees_add}{Number of trees to add in each subsequent run.}

\item{trees_max}{Maximum number of trees in the random forest.}

\item{rf_cores}{Number of cores to use for parallel processing.}

\item{use_mtry}{\code{mtry} value for \code{\link[randomForest:randomForest]{randomForest::randomForest()}} call. If
\code{NULL} it will be generated by a (lengthy) call to \code{\link[caret:train]{caret::train()}}
with a tune grid of \verb{.mtry = 1:floor(sqrt(length(env_names))}.}

\item{set_min}{FALSE or numeric. If numeric, classes in \code{clust_col} with less
than \code{set_min} rows will be filtered}

\item{accept_delta}{What proportion change between runs is acceptable?}

\item{accept_run}{How many forests (in a row) need to beat \code{accept_delta}?}

\item{internal_metrics}{TRUE or test data in same format as \code{env_df}.}

\item{do_imp}{Logical. Passed to \code{importance} argument of
\code{\link[randomForest:randomForest]{randomForest::randomForest()}}.}

\item{keep_rf}{Logical. If true, \code{randomForest} object will be included in
output. Defaults to \code{FALSE} to save memory.}

\item{out_file}{Optional name of file to save results.}

\item{save_res}{FALSE or folder path. If path is provided, any constant
columns in \code{env_df} will be used to generate a file name, with metrics saved
to folder path and file name.}

\item{do_gc}{Logical. Run \code{gc()} when results are available and all other
objects have been removed.}

\item{...}{Passed (via \code{make_rf_quick()}) to \code{randomForest::randomForest()}.}
}
\description{
Iteratively add trees to random forest until predictions stabilise
}
