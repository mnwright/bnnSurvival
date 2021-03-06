% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bnnSurvivalBaseLearner.R
\docType{methods}
\name{predict,bnnSurvivalBaseLearner-method}
\alias{predict,bnnSurvivalBaseLearner-method}
\title{Compute prediction for all samples.}
\usage{
\S4method{predict}{bnnSurvivalBaseLearner}(object, train_data, test_data,
  timepoints, metric, weighting_function, k)
}
\arguments{
\item{object}{bnnSurvivalBaseLearner object}

\item{train_data}{Training data (with response)}

\item{test_data}{Test data (without response)}

\item{timepoints}{Timepoint to predict at}

\item{metric}{Metric used}

\item{weighting_function}{Weighting function used}

\item{k}{Number of nearest neighbors}
}
\description{
Compute prediction for all samples.
}

