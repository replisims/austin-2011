true_rd <- c(0, -0.02, -0.05, -0.10, -0.1)
gamma <- seq(0.05, 2.5, 0.05)

# trial run binary --------------------------------------------------------

alpha_dich <- c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2))


test_binary <- generate_data(sample_size = 10000,
                             n_covariates = 10,
                                  n_normal = 10,
                                  alpha = alpha_dich,
                                  pair_cor = 0,
                                  prop_treated = 0.25,
                                  risk_diff =  -0.02,
                                  n_iter = 1000,
                                  outcome_type = "binary",
                                  margin_prev = 0.29)


propensity_score <- get_propensity(treatment_indicator = test_binary$sim_data$treatment_indicator,
                                   predictors = test_binary$sim_data[,1:10])

logit_propensity <- compute_logit(propensity_score)

matched_df <- get_matched_df(gamma = 0.3,
                             treatment_indicator = test_binary$sim_data$treatment_indicator,
                             logit_propensity = logit_propensity,
                             seed = 42)


sim_data <- data.frame(id = 1:nrow(test_binary$sim_data),
                       test_binary$sim_data)

matched_data <- right_join(sim_data, matched_df)

delta <- compute_delta(matched_data)

average_delta <- mean(delta)

t_test <- t.test(delta,
                 mu = 0,
                 alternative = "two.sided",
                 var.equal = TRUE)


std_error <- t_test$stderr
ci_95 <- t_test$conf.int


bias_crude(sim_data, 0)


bias_ps(average_delta, true_treatment_effect = 0)


reduction_bias <- bias_reduction(
  crude_bias = bias_crude(sim_data = sim_data,
                          true_treatment_effect = 0),
  ps_bias = bias_ps(average_delta = average_delta,
                    true_treatment_effect = 0))


difference_proportions(matched_data)

contingency_matrix <- get_contingency_matrix(matched_data)

mc_nemar <- mcnemar.test(contingency_matrix, correct = FALSE)

get_h_0(contingency_matrix)
