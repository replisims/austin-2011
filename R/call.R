# n_covariates_test <- 10
# n_normal_test <- 5
# sample_size_test <- 12
# pair_cor_test <- 0.25
# alpha_dich <- c(log(0.29/0.71), rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2))
 alpha_dich <- c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2))
# prop_treated_test <- 0.25
# alpha_continuous <- c(rep(1.1, 3), rep(1.25, 3), rep(1.5, 3), 2)



outcome_austin_binary <- generate_data(sample_size = 10000,
                                        n_covariates = 10,
                                        n_normal = 10,
                                        alpha = alpha_dich,
                                        pair_cor = 0,
                                        prop_treated = 0.25,
                                        risk_diff =  -0.02,
                                        n_iter = 1000,
                                        outcome_type = "binary",
                                        margin_prev = 0.29)


outcome_austin_binary %>% group_by(treatment_indicator) %>% summarise(mean = mean(outcome),
                                                                      sum = sum(outcome),
                                                                      n = n())
outcome_austin_binary %>% summarise(mean = mean(outcome))


test_covariate_sample <- outcome_austin_binary %>% dplyr::select(x_1:x_10)


# Obtaining the empirical marginals for one data set ----------------------

lin_pred_test <- t(c(-1.098492, alpha_dich)) %*% t(as.matrix(cbind(rep(1,10000),test_covariate_sample)))

mean(1/(1+exp(-lin_pred_test))) #marginal untreated

mean(1/(1+exp(-(lin_pred_test-0.1196518)))) #marginal treated

2553/(7447 + 2553)


# Empirical marginals for Austins parameters ------------------------------

alpha_0_paper <- log(0.29/0.71)
beta_paper <- - 0.9077272

beta_paper_005 <- 0.7836084
beta_paper_01 <- 0.6086645
beta_paper_015 <- 0.4658031


lin_pred_austin <- t(c(alpha_0_paper, alpha_dich)) %*% t(as.matrix(cbind(rep(1,10000),test_covariate_sample)))

mean(1/(1+exp(-lin_pred_austin))) #marginal untreated

mean(1/(1+exp(-(lin_pred_austin + beta_paper)))) #marginal treated

mean(1/(1+exp(-(lin_pred_austin + beta_paper_005)))) #marginal treated

mean(1/(1+exp(-(lin_pred_austin + beta_paper_01)))) #marginal treated

mean(1/(1+exp(-(lin_pred_austin + beta_paper_015)))) #marginal treated



# beta equaled 0.9077272
# For risk differences of −0.02, −0.05,
# −0.10, and −0.15, the required value of
# beta equaled 0.8935633, 0.7493734, 0.5446253, and 0.3769236, respectively.
#alpha_0,outcome = log(0.29/0.71) = -0.895384

# To induce a risk difference of 0, beta
# was set to be 0. For the risk differences of 0.02, 0.05, 0.10,
# and 0.15, the required value of b equaled 0.9077272,
# 0.7836084, 0.6086645, and 0.4658031, respectively.

# outcome_test <- generate_data(sample_size = 1000,
#                               n_covariates = 10,
#                               n_normal = 10,
#                               alpha = alpha_dich,
#                               pair_cor = .25,
#                               prop_treated = 0.29,
#                               risk_diff = -0.02,
#                               n_iter = 10,
#                               outcome_type = "continuous",
#                               beta = 1.1)
#
# mean(outcome_test$outcome)
#
#
# mean(outcome_test)
#
#
#
#
# check_input <- function(sample_size,
#                         n_covariates,
#                         n_normal,
#                         pair_cor,
#                         alpha,
#                         outcome_type,
#                         n_iter,
#                         prop_treated){}
#
#
# sum(alpha_dich)
