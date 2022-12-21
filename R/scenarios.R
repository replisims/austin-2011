
# Scenario definition -----------------------------------------------------

# 5 scenario types * 5 effect sizes * 2 outcome types = 50 scenarios

# Continuous outcome -------------------------------------------------------

fixed_parameters <- list(sample_size = 10000,
                         n_covariates = 10,
                         prop_treated = 0.25)

fixed_param_cont <- list(alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                         sigma_squared = 127.6056,
                         outcome_type = "continuous")

fixed_param_bin <- list(alpha = c(rep(log(1.1), 3),
                                  rep(log(1.25), 3),
                                  rep(log(1.5), 3),
                                  log(2)),
                        beta = NULL,
                        sigma_squared = NULL,
                        outcome_type = "binary",
                        n_iter = 1000,
                        margin_prev = 0.29)

fixed_param_indep_normal <- list(n_normal = 10,
                                 pair_cor = 0)

fixed_param_indep_bin <- list(n_normal = 0,
                              pair_cor = 0)

fixed_param_cor_normal <- list(n_normal = 10,
                               pair_cor = 0.25)

fixed_param_mix_1 <- list(n_normal = 5,
                          pair_cor = 0)

fixed_param_mix_2 <- list(n_normal = 1,
                           pair_cor = 0)



# Independent normal covariates -------------------------------------------
indep_normal_cont <- c(fixed_parameters,
                       fixed_param_cont,
                       fixed_param_indep_normal)

indep_normal_cont_0 <- c(indep_normal_cont,
                         beta = 0)

indep_normal_cont_11 <- c(indep_normal_cont,
                          beta = 1.1)

indep_normal_cont_125 <- c(indep_normal_cont,
                              beta = 1.25)

indep_normal_cont_15 <- c(indep_normal_cont,
                             beta = 1.5)

indep_normal_cont_2 <- c(indep_normal_cont,
                            beta = 2)


# Correlated covariates ---------------------------------------------------

cor_normal_cont <- c(fixed_parameters,
                     fixed_param_cont,
                     pair_cor = 0.25)

cor_normal_cont_0 <- c(cor_normal_cont,
                       beta = 0)

cor_normal_cont_11 <- c(cor_normal_cont,
                        beta = 1.1)

cor_normal_cont_125 <- c(cor_normal_cont,
                         beta = 1.25)

cor_normal_cont_15 <- c(cor_normal_cont,
                         beta = 1.5)

cor_normal_cont_2 <- c(cor_normal_cont,
                       beta = 2)


# Mixed covariates (50/50) ------------------------------------------------

mix_1_cont <- c(fixed_parameters,
                  fixed_param_cont,
                  fixed_param_mix_1)

mix_1_cont_0 <- c(mix_1_cont,
                  beta = 0)

mix_1_cont_11 <- c(mix_1_cont,
                  beta = 1.1)

mix_1_cont_125 <- c(mix_1_cont,
                  beta = 1.25)

mix_1_cont_15 <- c(mix_1_cont,
                  beta = 1.5)

mix_1_cont_2 <- c(mix_1_cont,
                  beta = 2)

# Mixed covariates 90% binary----------------------------------------------

mix_2_cont <- c(fixed_parameters,
                  fixed_param_cont,
                  fixed_param_mix_2)

mix_2_cont_0 <- c(mix_2_cont,
                  beta = 0)

mix_2_cont_11 <- c(mix_2_cont,
                   beta = 1.1)

mix_2_cont_125 <- c(mix_2_cont,
                    beta = 1.25)

mix_2_cont_15 <- c(mix_2_cont,
                   beta = 1.5)

mix_2_cont_2 <- c(mix_2_cont,
                  beta = 2)

  #indep_bin


# Independent binary covariates (continuous outcome) ----------------------

indep_bin_cont <- c(fixed_parameters,
                       fixed_param_cont,
                       fixed_param_indep_bin)


indep_bin_cont_0 <- c(indep_bin_cont,
                      beta = 0)

indep_bin_cont_11 <- c(indep_bin_cont,
                      beta = 1.1)

indep_bin_cont_125 <- c(indep_bin_cont,
                      beta = 1.25)

indep_bin_cont_15 <- c(indep_bin_cont,
                      beta = 1.5)

indep_bin_cont_2 <- c(indep_bin_cont,
                      beta = 2)


# Binary outcome ----------------------------------------------------------


# Independent normal covariates (binary outcome) --------------------------

indep_norm_bin <- c(fixed_parameters,
                    fixed_param_bin,
                    fixed_param_indep_normal)

indep_norm_bin_0 <- c(indep_norm_bin,
                      risk_diff = 0)

indep_norm_bin_002 <- c(indep_norm_bin,
                        risk_diff = -0.02)

indep_norm_bin_005 <- c(indep_norm_bin,
                        risk_diff = -0.05)

indep_norm_bin_01 <- c(indep_norm_bin,
                       risk_diff = -0.1)

indep_norm_bin_015 <- c(indep_norm_bin,
                        risk_diff = -0.15)

# Correlated normal covariates (binary outcome) --------------------------

cor_norm_bin<-c(fixed_parameters,
                  fixed_param_bin,
                  fixed_param_cor_normal)

cor_norm_bin_0 <- c(cor_norm_bin,
                      risk_diff = 0)

cor_norm_bin_002 <- c(cor_norm_bin,
                        risk_diff = -0.02)

cor_norm_bin_005 <- c(cor_norm_bin,
                      risk_diff = -0.05)

cor_norm_bin_01 <- c(cor_norm_bin,
                      risk_diff = -0.1)

cor_norm_bin_015 <- c(cor_norm_bin,
                      risk_diff = -0.15)


# Mixed covariates (50/50) ------------------------------------------------

mix_1_bin <- c(fixed_parameters,
               fixed_param_bin,
               fixed_param_mix_1)

mix_1_bin_0 <- c(mix_1_bin,
                 risk_diff = 0)

mix_1_bin_002 <- c(mix_1_bin,
                   risk_diff = -0.02)

mix_1_bin_005 <- c(mix_1_bin,
                   risk_diff = -0.05)

mix_1_bin_01 <- c(mix_1_bin,
                   risk_diff = -0.1)

mix_1_bin_015 <- c(mix_1_bin,
                  risk_diff = -0.15)

# Mixed covariates (90% binary) binary outcome ----------------------------------

mix_2_bin <- c(fixed_parameters,
               fixed_param_bin,
               fixed_param_mix_2)

mix_2_bin_0 <- c(mix_2_bin,
                 risk_diff = 0)

mix_2_bin_002 <- c(mix_2_bin,
                   risk_diff = -0.02)

mix_2_bin_005 <- c(mix_2_bin,
                   risk_diff = -0.05)

mix_2_bin_01 <- c(mix_2_bin,
                  risk_diff = -0.1)

mix_2_bin_015 <- c(mix_2_bin,
                   risk_diff = -0.15)

# Independent binary covariates (binary outcome) ----------------------

indep_bin_bin <- c(fixed_parameters,
                   fixed_param_bin,
                   fixed_param_indep_bin)


indep_bin_bin_0 <- c(indep_bin_bin,
                     risk_diff = 0)

indep_bin_bin_002 <- c(indep_bin_bin,
                       risk_diff = -0.02)

indep_bin_bin_005 <- c(indep_bin_bin,
                       risk_diff = -0.05)

indep_bin_bin_01 <- c(indep_bin_bin,
                      risk_diff = -0.1)

indep_bin_bin_015 <- c(indep_bin_bin,
                       risk_diff = -0.15)


# Scenario lists ----------------------------------------------------------

continuous_scenarios <- list(indep_normal_cont_0 = indep_normal_cont_0,
                             indep_normal_cont_11 = indep_normal_cont_11,
                             indep_normal_cont_125 = indep_normal_cont_125,
                             indep_normal_cont_15 = indep_normal_cont_15,
                             indep_normal_cont_2 = indep_normal_cont_2,
                             cor_normal_cont_0 = cor_normal_cont_0,
                             cor_normal_cont_11 = cor_normal_cont_11,
                             cor_normal_cont_125 = cor_normal_cont_125,
                             cor_normal_cont_15 = cor_normal_cont_15,
                             cor_normal_cont_2 = cor_normal_cont_2,
                             mix_1_cont_0 = mix_1_cont_0,
                             mix_1_cont_11 = mix_1_cont_11,
                             mix_1_cont_125 = mix_1_cont_125,
                             mix_1_cont_15 = mix_1_cont_15,
                             mix_1_cont_2 = mix_1_cont_2,
                             mix_2_cont_0 = mix_2_cont_0,
                             mix_2_cont_11 = mix_2_cont_11,
                             mix_2_cont_125 = mix_2_cont_125,
                             mix_2_cont_15 = mix_2_cont_15,
                             mix_2_cont_2 = mix_2_cont_2,
                             indep_bin_cont_0 = indep_bin_cont_0,
                             indep_bin_cont_11 = indep_bin_cont_11,
                             indep_bin_cont_125 = indep_bin_cont_125,
                             indep_bin_cont_15 = indep_bin_cont_15,
                             indep_bin_cont_2 = indep_bin_cont_2)

binary_scenarios <- list(indep_norm_bin_0 = indep_norm_bin_0,
                         indep_norm_bin_002 = indep_norm_bin_002,
                         indep_norm_bin_005 = indep_norm_bin_005,
                         indep_norm_bin_01 = indep_norm_bin_01,
                         indep_norm_bin_015 = indep_norm_bin_015,
                         cor_norm_bin_0 = cor_norm_bin_0,
                         cor_norm_bin_002 = cor_norm_bin_002,
                         cor_norm_bin_005 = cor_norm_bin_005,
                         cor_norm_bin_01 = cor_norm_bin_01,
                         cor_norm_bin_015 = cor_norm_bin_015,
                         mix_1_bin_0 = mix_1_bin_0,
                         mix_1_bin_002 = mix_1_bin_002,
                         mix_1_bin_005 = mix_1_bin_005,
                         mix_1_bin_01 = mix_1_bin_01,
                         mix_1_bin_015 = mix_1_bin_015,
                         mix_2_bin_0 = mix_2_bin_0,
                         mix_2_bin_002 = mix_2_bin_002,
                         mix_2_bin_005 = mix_2_bin_005,
                         mix_2_bin_01 = mix_2_bin_01,
                         mix_2_bin_015 = mix_2_bin_015,
                         indep_bin_bin_0 = indep_bin_bin_0,
                         indep_bin_bin_002 = indep_bin_bin_002,
                         indep_bin_bin_005 = indep_bin_bin_005,
                         indep_bin_bin_01 = indep_bin_bin_01,
                         indep_bin_bin_015 = indep_bin_bin_015)

