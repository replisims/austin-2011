
# Quick and dirty function to check the Austin parameters -----------------


MC_sampling <- function(sample_size,
                        alpha,
                        alpha_0,
                        beta,
                        n_covariates){

  covariate_data <- 1:n_covariates %>%
    purrr::map(~rnorm(sample_size))

    names(covariate_data) <- paste0("x_", 1:n_covariates)
    covariate_data <- do.call(cbind, covariate_data)

    lin_pred <- get_linear_predictor(alpha = c(alpha_0, alpha),
                                     covariate_data = cbind(rep(1, sample_size), covariate_data))

    p_1 <- 1/(1 + exp(-(lin_pred + beta)))
    p_0 <- 1/(1 + exp(-(lin_pred)))

    gamma <- mean(p_1 - p_0)
    marginal_prob_treated <- mean(p_1)
    marginal_prob_untreated <- mean(p_0)

    return(list(covariate_data,
                gamma = gamma,
                marginal_prob_treated = marginal_prob_treated,
                marginal_prob_untreated = marginal_prob_untreated))
    }


# Parameters from paper ----------------------------------------------------

alpha_dich <- c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2))

alpha_0_paper <- log(0.29/0.71)
beta_paper <-  0.9077272

beta_paper_005 <- 0.7836084
beta_paper_01 <- 0.6086645
beta_paper_015 <- 0.4658031




# Marginals with parameters from paper ------------------------------------


test_austin <- MC_sampling(sample_size = 10000,
                           alpha = alpha_dich,
                           alpha_0 = -1.098537,
                           beta = 0.7836084,
                           n_covariates = 10)

test_austin$gamma #should be -0.05
test_austin$marginal_prob_treated # should be 0.29 - 0.05
test_austin$marginal_prob_untreated #should be 0.29



# Parameters from binary search -------------------------------------------

set.seed(1)
parameters_anna <- generate_data(sample_size = 10000,
                                        n_covariates = 10,
                                        n_normal = 10,
                                        alpha = alpha_dich,
                                        pair_cor = 0,
                                        prop_treated = 0.25,
                                        risk_diff =  -0.15,
                                        n_iter = 1000,
                                        outcome_type = "binary",
                                        margin_prev = 0.29)


get_contingency_matrix_unmatched(parameters_anna$sim_data) %>% print

# Marginals with binary search --------------------------------------------


test_anna <- MC_sampling(sample_size = 10000,
                         alpha = alpha_dich,
                         alpha_0 = parameters_anna$alpha_0_outcome,
                         beta = parameters_anna$beta,
                         n_covariates = 10)

test_anna$gamma #should be -0.02
test_anna$marginal_prob_treated # should be 0.29 - 0.02
test_anna$marginal_prob_untreated #should be 0.29


# Continuous data ---------------------------------------------------------



parameters_anna2 <- generate_data(sample_size = 10000,
                                 n_covariates = 10,
                                 n_normal = 10,
                                 alpha = alpha_dich,
                                 pair_cor = 0,
                                 prop_treated = 0.25,
                                 risk_diff =  -0.02,
                                 n_iter = 1000,
                                 outcome_type = "continuous",
                                 margin_prev = 0.29,
                                 sigma_squared = 127.6056)

mean(parameters_anna$sim_data$treatment_prob)
mean(parameters_anna$sim_data$treatment_indicator)


lin_pred_t <- get_linear_predictor(alpha = c(parameters_anna$alpha_0_outcome, alpha_dich),
                     covariate_data = cbind(rep(1,10000),parameters_anna$sim_data[1:10]))


# Marginal probability treated (if we treated everyone)
mean(1/(1 + exp(-(lin_pred_t + parameters_anna$beta)))) # 0.1401514

# Marginal probability untreated *if we didn't treat anyone)
mean(1/(1 + exp(-(lin_pred_t)))) #0.2901523

# ATE

# -0.1500009


#ATT
mean((1/(1 + exp(-(lin_pred_t + parameters_anna$beta)))) - (1/(1 + exp(-(lin_pred_t)))))

mean((1/(1 + exp(-(lin_pred_t + 0.4658031)))) - (1/(1 + exp(-(lin_pred_t)))))
# -0.1500009 (same thing)


sim_data_treated <- parameters_anna$sim_data %>% dplyr::filter(treatment_indicator == 1)

lin_pred_treated <- get_linear_predictor(alpha = c(parameters_anna$alpha_0_outcome, alpha_dich),
                                   covariate_data = cbind(rep(1, nrow(sim_data_treated)), sim_data_treated[1:10]))

mean((1/(1 + exp(-(lin_pred_treated + parameters_anna$beta)))) - (1/(1 + exp(-(lin_pred_treated)))))


mean((1/(1 + exp(-(lin_pred_treated + parameters_anna$beta))))) - mean(1/(1 + exp(-(lin_pred_treated))))


# Austin's betas have to be log'ed and induce the specified ATT
mean((1/(1 + exp(-(lin_pred_treated + log(0.4658031))))) - (1/(1 + exp(-(lin_pred_treated)))))


log(0.9077272)
log(0.7836084)
log(0.6086645)
log(0.4658031)


parameters_anna <- NULL


devtools::load_all()


run_sim(scenario = "indep_")
