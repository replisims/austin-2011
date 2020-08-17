#' Generate predictor and outcome data
#'
#' @param sample_size integer specifying the sample size
#' @param n_covariates integer specifying the number of covariates
#' @param n_normal  integer specifying the number of normal covariates (must be <= \code{n_convariates})
#' @param pair_cor pairwise correlation of covariates
#' @param alpha covariate vector of length \code{n_covariate}
#' @param beta regression coefficient for treatment-dummy
#' @param sigma_squared error variance
#' @param outcome_type character value \code{"continuous"} or \code{"binary"}
#' @param n_iter integer indicating number of iterations
#' @param prop_treated numerical value of intended proportion of treated individuals
#' @param risk_diff numerical value of intended risk difference
#' @param margin_prev numerical value of intenden marginal prevalence
#'
#' @return a list with the following content
#' \itemize{
#' \item \code{sim_data} dataframe with simulated data}
#' \item \code{alpha_0_treat} numerical value indicating intercept of treatment model
#' \item \code{alpha_0_outcome} numerical value indicating intercept of outcome model
#' \item \code{beta} numerical value indicating regression coefficient of treatment dummy}
#' @export
#'
generate_data <- function(sample_size = 10000,
                          n_covariates = 10,
                          n_normal = n_covariates,
                          pair_cor = 0,
                          alpha,
                          beta = NULL,
                          sigma_squared = NULL,
                          outcome_type = NULL,
                          n_iter = 1000,
                          prop_treated = NULL,
                          risk_diff = NULL,
                          margin_prev = NULL){

  if(n_normal>0){
    cov_mat <- get_cov_matrix(n_normal = n_normal,
                                pair_cor = pair_cor)}

  alpha_0_treat <- get_intercept(n_iter,
                                 sample_size,
                                 n_covariates,
                                 n_normal,
                                 cov_mat,
                                 alpha,
                                 target_mean = prop_treated)

  alpha_0_outcome <- switch(outcome_type,
                            binary = get_intercept(n_iter,
                                   sample_size,
                                   n_covariates,
                                   n_normal,
                                   cov_mat,
                                   alpha,
                                   target_mean = margin_prev),
                            continuous = 0)

  beta <- switch(outcome_type,
                 binary = get_beta(n_iter,
                                   sample_size,
                                   n_covariates,
                                   n_normal,
                                   cov_mat = cov_mat,
                                   alpha,
                                   alpha_0_outcome,
                                   risk_diff),
                 continuous = beta)

  covariate_data <- sample_covariates(sample_size = sample_size,
                                        n_covariates = n_covariates,
                                        n_normal = n_normal,
                                        cov_mat = cov_mat)


  lin_pred_treat <- get_linear_predictor(alpha = c(alpha_0_treat, alpha),
                                         covariate_data = cbind(rep(1, sample_size),
                                                            covariate_data))


  lin_pred_outcome <- get_linear_predictor(alpha = c(alpha_0_outcome, alpha),
                                           covariate_data = cbind(rep(1, sample_size),
                                                                covariate_data))


  treatment_prob <- 1/(1 + exp(-(lin_pred_treat)))

  treatment_indicator <- sample_bernoulli(treatment_prob)

  outcome_prob <- 1/(1+exp(-(lin_pred_outcome + beta*treatment_indicator)))

  outcome <- switch(outcome_type,
                    binary = sample_bernoulli(outcome_prob),
                    continuous = lin_pred_outcome + beta * treatment_indicator + get_epsilon(sigma_squared = sigma_squared,
                                                                                             sample_size = sample_size))


  list(sim_data = tibble::tibble(covariate_data,
                         treatment_prob = treatment_prob,
                         treatment_indicator = treatment_indicator,
                         outcome = outcome),
       alpha_0_treat = alpha_0_treat,
       alpha_0_outcome = alpha_0_outcome,
       beta = beta)
}

