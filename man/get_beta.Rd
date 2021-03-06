% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generation_utils.R
\name{get_beta}
\alias{get_beta}
\title{Estimate coefficient for treatment dummy}
\usage{
get_beta(
  n_iter,
  sample_size,
  n_covariates,
  n_normal,
  cov_mat,
  alpha,
  alpha_0_outcome,
  risk_diff
)
}
\arguments{
\item{n_iter}{number of iterations}

\item{sample_size}{sample size}

\item{n_covariates}{number of covariates}

\item{n_normal}{integer specifying the number of normal covariates (must be <= n_convariates)}

\item{cov_mat}{covariance matrix}

\item{alpha}{coeffcient vector (without intercept)}

\item{alpha_0_outcome}{intercept for outcome model}

\item{risk_diff}{intended risk difference}
}
\value{
estimate for beta (coefficient for treatment dummy)
}
\description{
Estimate coefficient for treatment dummy
}
