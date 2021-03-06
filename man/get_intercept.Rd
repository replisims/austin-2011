% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation_utils.R
\name{get_intercept}
\alias{get_intercept}
\title{Estimate intercept}
\usage{
get_intercept(
  n_iter,
  sample_size,
  n_covariates,
  n_normal,
  cov_mat,
  alpha,
  target_mean
)
}
\arguments{
\item{n_iter}{number of iterations}

\item{sample_size}{sample size}

\item{n_covariates}{number of covariates}

\item{n_normal}{number of normal covariates}

\item{cov_mat}{correlation matrix}

\item{alpha}{coefficient vector}

\item{target_mean}{intended population mean}
}
\value{
estimate for intercept
}
\description{
Estimate intercept
}
