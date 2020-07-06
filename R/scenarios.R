
# Scenario definition -----------------------------------------------------

scenarios_binary <- data.frame(sample_size = 10000,
                          n_covariates = 10,
                          n_normal = 10,
                          pair_cor = 0,
                          alpha = c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2)),
                          beta = NULL,
                          sigma_squared = NULL,
                          outcome_type = "binary",
                          n_iter = 1000,
                          prop_treated = 0.25,
                          risk_diff = c(0, -0.02, -0.05, -0.1, -0.15),
                          margin_prev = 0.29)


scenarios_continuous <- data.frame(sample_size = 1000,
                                        n_covariates = 10,
                                        pair_cor = 0,
                                        alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                                        beta = c(0, 1.1, 1.25, 1.5, 2),
                                        sigma_squared = 127.6056,
                                        outcome_type = "continuous",
                                        n_iter = 100,
                                        prop_treated = 0.25)

scenarios_continuous <- data.frame(sample_size = 1000,
                                        n_covariates = 10,
                                        pair_cor = 0.25,
                                        alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                                        beta = c(0, 1.1, 1.25, 1.5, 2), #Has to be modified ???
                                        sigma_squared = 127.6056,
                                        outcome_type = "continuous",
                                        n_iter = 100,
                                        prop_treated = 0.25)

scenarios_continuous <- data.frame(sample_size = 1000,
                                        n_covariates = 10,
                                        n_normal = 5,
                                        pair_cor = 0,
                                        alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                                        beta = c(0, 1.1, 1.25, 1.5, 2), #Has to be modified ???
                                        sigma_squared = 127.6056,
                                        outcome_type = "continuous",
                                        n_iter = 100,
                                        prop_treated = 0.25)

scenarios_continuous <- data.frame(sample_size = 1000,
                                        n_covariates = 10,
                                        n_normal = 1,
                                        pair_cor = 0,
                                        alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                                        beta = c(0, 1.1, 1.25, 1.5, 2), #Has to be modified ???
                                        sigma_squared = 127.6056,
                                        outcome_type = "continuous",
                                        n_iter = 100,
                                        prop_treated = 0.25)

scenarios_continuous <- data.frame(sample_size = 1000,
                                        n_covariates = 10,
                                        n_normal = 0,
                                        pair_cor = 0,
                                        alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                                        beta = c(0, 1.1, 1.25, 1.5, 2), #Has to be modified ???
                                        sigma_squared = 127.6056,
                                        outcome_type = "continuous",
                                        n_iter = 100,
                                        prop_treated = 0.25)
