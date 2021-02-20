test_continuous <- generate_data(sample_size = 1000,
                          n_covariates = 10,
                          pair_cor = 0,
                          alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
                          beta = 0,
                          sigma_squared = 127.6056,
                          outcome_type = "continuous",
                          n_iter = 100,
                          prop_treated = 0.25)

glimpse(test_continuous)

fit <- lm(outcome ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = test_continuous$sim_data)
summary(fit)
