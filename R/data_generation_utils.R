# Helper functions for data generation ------------------------------------

# Function to assemble a covariance matrix --------------------------------

#' Assemble correlation matrix
#'
#' @param n_normal integer specifying the number of normal covariates
#' @param pair_cor pairwise correlation
#'
#' @return correlation matrix of size \code{n_normal}^2
#'
get_cov_matrix <- function(n_normal,
                           pair_cor){
  cov_mat <- matrix(rep(pair_cor, n_normal^2), nrow = n_normal)
  diag(cov_mat) <- 1
  return(cov_mat)
}


# Function for sampling the covariates data --------------------------------

#' Sample covariate data
#'
#' @param sample_size integer specifying the sample size
#' @param n_covariates integer specifying the number of covariates
#' @param n_normal integer specifying the number of normal covariates (must be <= n_convariates)
#' @param cov_mat correlation matrix
#'
#' @return a tibble data frame of size \code{sample_size} x \code{n_covariates}
#'
#' @importFrom magrittr "%>%"
sample_covariates <- function(sample_size,
                              n_covariates,
                              n_normal,
                              cov_mat = NULL){

#  covariate_data <- tibble(x_0 = rep(1, sample_size)) #intercept

  # Sampling the normal covariates

  if(n_normal > 0){
    covariate_data <- MASS::mvrnorm(n = sample_size,
                              mu = rep(0, n_normal),
                              Sigma = cov_mat) %>%
                        tibble::as_tibble() %>%
                          rlang::set_names(paste0("x_", 1:n_normal)) #%>%
                              #  cbind(covariate_data, .)
  }
  # Sample the Bernoulli covariates

  if(n_normal < n_covariates){
    covariate_data <- paste0("x_", (n_normal+1):n_covariates) %>%
                        purrr::imap_dfc(~{rbinom(sample_size, size = 1, prob = 0.5) %>%
                            tibble::as_tibble() %>%
                              rlang::set_names(.x)})# %>%
                              #  cbind(covariate_data, .)
  }
  return(covariate_data)
}


# Compute linear combination ----------------------------------------------

#' Obtain linear predictor
#'
#' @param alpha coefficient vector
#' @param covariate_data data frame with predictor data
#'
#' @return a vector
#'
#' @importFrom magrittr "%>%"
get_linear_predictor <- function(alpha, covariate_data){

  linear_combination <- t(alpha) %*% t(as.matrix(covariate_data))
  as.vector(linear_combination)
  }


# Estimate alpha_0 --------------------------------------------------------

#' Estimate intercept for given marginal probability
#'
#' @param candidate a candidate value for the target intercept
#' @param linear_predictor the linear predictor without the intercept
#'
#' @return marginal probability using the candidate intercept

estimate_intercept <- function(candidate,
                               linear_predictor){

  test_target <- 1/(1 + exp(-(linear_predictor + candidate)))
  mean(test_target)
}

# Binary search function --------------------------------------------------

#' Binary search
#'
#' @param fun a function to test the boundary values
#' @param target_value the target value
#' @param lower a lower boundary
#' @param upper an upper boundary
#' @param epsilon the minimum interval size of [lower,upper]
#' @param ...
#'
#' @return an approximation of the target value
#' @export

binary_search <- function(fun,
                          target_value,
                          lower, upper,
                          epsilon = 0.000001,
                          ...) {

  increasing <- fun(upper, ...) > fun(lower, ...)

  while(upper - lower > epsilon) {
    middle <- (lower + upper) / 2
    middle_function_value <- fun(middle, ...)

    if(middle_function_value < target_value && increasing)
      lower <- middle
    else
      upper <- middle
  }

  return(middle)
}


# Compute Treatment Indicator ---------------------------------------------


#' Sample treatment indicator
#'
#' @param probability
#'
#' @return
#' @export
#'
#' @importFrom magrittr "%>%"
sample_bernoulli <- function(probability){

  probability %>%
    purrr::map_dbl(~rbinom(n = 1, size = 1, prob = .x))
  }


# Estimate marginal risk difference ---------------------------------------

#' Estimate marginal risk difference
#'
#' @param candidate candidate value for marginal risk difference
#' @param linear_predictor linear predictor without intercept
#' @param marg_prob_untreated intended marginal probability untreated
#'
#' @return marginal risk difference obtained with candidate value
#'

estimate_marginal_risk_diff <- function(candidate,
                                        linear_predictor,
                                        marg_prob_untreated){

  marginal_prob_treated <- 1/(1 + exp(-(linear_predictor + candidate))) #p_bar_1

  marginal_risk_diff <-  mean(marginal_prob_treated) - mean(marg_prob_untreated) #p_bar_1-p_bar_0

}


# Function for estimating alpha_0 -----------------------------------------

#' Estimate intercept
#'
#' @param n_iter number of iterations
#' @param sample_size sample size
#' @param n_covariates number of covariates
#' @param n_normal number of normal covariates
#' @param cov_mat correlation matrix
#' @param alpha coefficient vector
#' @param target_mean intended population mean
#'
#' @return estimate for intercept
#'
#' @importFrom magrittr "%>%"
get_intercept <- function(n_iter,
                          sample_size,
                          n_covariates,
                          n_normal,
                          cov_mat,
                          alpha,
                          target_mean){
  1:n_iter %>% purrr::map_dbl(~{

  covariate_data <- sample_covariates(sample_size = sample_size,
                                      n_covariates = n_covariates,
                                      n_normal = n_normal,
                                      cov_mat = cov_mat)


  lin_pred <- get_linear_predictor(alpha = alpha,
                       covariate_data = covariate_data)


  alpha_0 <- binary_search(fun = estimate_intercept,
                           target_value = target_mean,
                           lower = -log((1 - target_mean)/target_mean) - mean(lin_pred) - var(lin_pred),
                           upper = -log((1 - target_mean)/target_mean) + mean(lin_pred) + var(lin_pred),
                           linear_predictor = lin_pred)}) %>%
    mean()
}



# Function for estimating beta --------------------------------------------

#' Estimate coefficient for treatment dummy
#'
#' @param n_iter number of iterations
#' @param sample_size sample size
#' @param n_covariates number of covariates
#' @param n_normal integer specifying the number of normal covariates (must be <= n_convariates)
#' @param cov_mat covariance matrix
#' @param alpha coefficient vector (without intercept)
#' @param alpha_0_outcome intercept for outcome model
#' @param risk_diff intended risk difference
#'
#' @return estimate for beta (coefficient for treatment dummy)
#' @importFrom magrittr "%>%"
#'
get_beta <- function(n_iter,
                     sample_size,
                     n_covariates,
                     n_normal,
                     cov_mat,
                     alpha,
                     alpha_0_outcome,
                     risk_diff){
  1:n_iter %>% purrr::map_dbl(~{

    covariate_data <- sample_covariates(sample_size = sample_size,
                                        n_covariates = n_covariates,
                                        n_normal = n_normal,
                                        cov_mat = cov_mat)


    lin_pred <- get_linear_predictor(alpha = c(alpha_0_outcome, alpha),
                                     covariate_data = cbind(rep(1, sample_size), covariate_data))


    marg_prob_untreated <- 1/(1 + exp(-(lin_pred))) # p_bar_0

    beta <- binary_search(fun = estimate_marginal_risk_diff,
                           target_value = risk_diff,
                           lower = -2 * sum(alpha),
                           upper = sum(alpha),
                           linear_predictor = lin_pred,
                           marg_prob_untreated = marg_prob_untreated)
  }) %>% mean()
}

#' Simulate random error for continuous outcome
#'
#' @param sigma_squared Error variance
#' @param sample_size sample size
#'
#' @return Numeric vector of length \code{sample_size}
#'
get_epsilon <- function(sigma_squared, sample_size){
  rnorm(n = sample_size, mean = 0, sd = sqrt(sigma_squared))
}
