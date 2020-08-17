
# Scenario definition -----------------------------------------------------

continuous_scenarios <- c("indep_normal_cont_0",
                          "indep_normal_cont_11",
                          "indep_normal_cont_125",
                          "indep_normal_cont_15",
                          "indep_normal_cont_2",
                          "cor_normal_cont_0",
                          "cor_normal_cont_11",
                          "cor_normal_cont_125",
                          "cor_normal_cont_15",
                          "cor_normal_cont_2",
                          )

# Continuous outcome -------------------------------------------------------

fixed_parameters <- list(sample_size = 10000,
                         n_covariates = 10,
                         prop_treated = 0.25
                         )

fixed_param_cont <- list(alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                              sigma_squared = 127.6056,
                              outcome_type = "continuous")

fixed_param_binary <- list(alpha = c(rep(log(1.1), 3),
                                     rep(log(1.25), 3),
                                     rep(log(1.5), 3),
                                     log(2)),
                          beta = NULL,
                          sigma_squared = NULL,
                          outcome_type = "binary",
                          n_iter = 1000,
                          prop_treated = 0.25,
                          margin_prev = 0.29)

fixed_param_indep_normal <- list(n_normal = 10,
                                 pair_cor = 0)

fixed_param_cor_normal <- list(n_normal = 10,
                               pair_cor = 0.25)

fixed_param_mix_1 <- list(n_normal = 5,
                          pair_cor = 0)

fixed_parameters_2 <- list(n_normal = 1,
                           pair_cor = 0)



# Independant normal covariates -------------------------------------------
indep_normal_cont <- list(fixed_parameters,
                          fixed_param_cont,
                          fixed_param_indep_normal)

indep_normal_cont_0 <- list(indep_normal_cont,
                            beta = 0)

indep_normal_cont_11 <- list(indep_normal_cont,
                             beta = 1.1)

indep_normal_cont_125 <- list(indep_normal_cont,
                              beta = 1.25)

indep_normal_cont_15 <- list(indep_normal_cont,
                             beta = 1.5)

indep_normal_cont_2 <- list(indep_normal_cont,
                            beta = 2)


# Correlated covariates ---------------------------------------------------

cor_normal_cont <- list(sample_size = 10000,
                          n_covariates = 10,
                          pair_cor = 0.25,
                          alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                          sigma_squared = 127.6056,
                          outcome_type = "continuous",
                          prop_treated = 0.25)

cor_normal_cont_0 <- c(cor_normal_cont,
                       beta = 0)

cor_normal_cont_11 <- c(cor_normal_cont,
                        beta = 1.1)

cor_normal_cont_125 <- c(cor_normal_cont,
                         beta = 1.25)

cor_normal_cont_2 <- c(cor_normal_cont,
                       beta = 2)


# Mixed covariates (50/50) ------------------------------------------------

mix_1_cont_0 <-

# Mixed covariates 90% binary----------------------------------------------

mix_2_cont_0 <-
# Binary outcome ----------------------------------------------------------


independent_binary_0 <- c(fixed_parameters,
                          fixed_param_binary,
                          fixed_param_indep_normal,
                          risk_diff = 0)

independent_binary_002 <- list(sample_size = 10000,
                           n_covariates = 10,
                           n_normal = 10,
                           pair_cor = 0,
                           alpha = c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2)),
                           beta = NULL,
                           sigma_squared = NULL,
                           outcome_type = "binary",
                           n_iter = 1000,
                           prop_treated = 0.25,
                           risk_diff = -0.02,
                           margin_prev = 0.29)

independent_binary_005 <- list(sample_size = 10000,
                           n_covariates = 10,
                           n_normal = 10,
                           pair_cor = 0,
                           alpha = c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2)),
                           beta = NULL,
                           sigma_squared = NULL,
                           outcome_type = "binary",
                           n_iter = 1000,
                           prop_treated = 0.25,
                           risk_diff = -0.05,
                           margin_prev = 0.29)

independent_binary_01 <- list(sample_size = 10000,
                           n_covariates = 10,
                           n_normal = 10,
                           pair_cor = 0,
                           alpha = c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2)),
                           beta = NULL,
                           sigma_squared = NULL,
                           outcome_type = "binary",
                           n_iter = 1000,
                           prop_treated = 0.25,
                           risk_diff = -0.1,
                           margin_prev = 0.29)

independent_binary_015 <- list(sample_size = 10000,
                           n_covariates = 10,
                           n_normal = 10,
                           pair_cor = 0,
                           alpha = c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2)),
                           beta = NULL,
                           sigma_squared = NULL,
                           outcome_type = "binary",
                           n_iter = 1000,
                           prop_treated = 0.25,
                           risk_diff = -0.15,
                           margin_prev = 0.29)

