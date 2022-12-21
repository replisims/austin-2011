gamma <- seq(0.05, 2.5, 0.05)

gamma_short <- c(0.05, 1, 2.5)

# trial run binary --------------------------------------------------------

alpha_dich <- c(rep(log(1.1), 3), rep(log(1.25), 3), rep(log(1.5), 3), log(2))


test_binary <- generate_data(sample_size = 10000, #
                             n_covariates = 10, #
                                  n_normal = 0, #
                                  alpha = alpha_dich, #
                                  pair_cor = 0, #
                                  prop_treated = 0.25, #
                                  risk_diff =  -0.15,
                                  n_iter = 1000, #
                                  outcome_type = "binary", #
                                  margin_prev = 0.29) #


propensity_score <- get_propensity(treatment_indicator = test_binary$sim_data$treatment_indicator,
                                   predictors = test_binary$sim_data[, 2:11])

summary(propensity_score)

sim_data <- data.frame(id = 1:nrow(test_binary$sim_data),
                       test_binary$sim_data)

logit_propensity <- get_propensity_logit(treatment_indicator = sim_data$treatment_indicator,
                                         predictors = sim_data[, 2:11])

summary(logit_propensity)


matched_df <- get_matched_df(gamma = 0.3,
                             treatment_indicator = sim_data$treatment_indicator,
                             logit_propensity = logit_propensity,
                             seed = 42)



# Code to test matching step by step --------------------------------------


# matching_data <- data.frame(id = 1:length(sim_data$treatment_indicator),
#                             treatment_indicator = sim_data$treatment_indicator,
#                             logit_propensity = logit_propensity)
#
#
# matched_data <- NULL
#
# caliper <- get_caliper_width(gamma = 0.3,
#                                var_treated = get_var_logit_prop_score(group = 1,
#                                                                       logit_propensity = logit_propensity,
#                                                                       treatment_indicator = sim_data$treatment_indicator),
#                                var_untreated = get_var_logit_prop_score(group = 0,
#                                                                         logit_propensity = logit_propensity,
#                                                                         treatment_indicator = sim_data$treatment_indicator))
#
# candidate_id <- sample_matching_candidate(matching_data = matching_data)
#
# distance <- compute_distance(candidate_id = candidate_id, matching_data = matching_data)


matched_data <- dplyr::left_join(matched_df, sim_data,
                                  by = c("id", "treatment_indicator"))

diff_prop <- difference_proportions(matched_data = matched_data)

match_id <- compute_id_min_dist(candidate_id, matching_data)




#######

sim_data <- data.frame(id = 1:nrow(test_binary$sim_data),
                       test_binary$sim_data)

matched_data <- dplyr::right_join(sim_data, matched_df)

delta <- compute_delta(matched_data)

average_delta <- mean(delta)

t_test <- t.test(delta,
                 mu = 0,
                 alternative = "two.sided",
                 var.equal = TRUE)


std_error <- t_test$stderr
ci_95 <- t_test$conf.int


bias_crude(sim_data, 0)

diff_prop <- difference_proportions(matched_data)

bias_ps_test <- bias_ps(estimated_effect = diff_prop,
        true_treatment_effect = -0.02)


reduction_bias <- bias_reduction(
  crude_bias = bias_crude(sim_data = sim_data,
                          true_treatment_effect = 0),
  ps_bias = bias_ps(estimated_effect = diff_prop,
                    true_treatment_effect = 0))




contingency_matrix <- get_contingency_matrix(matched_data)

mc_nemar <- mcnemar.test(contingency_matrix, correct = FALSE)

get_h_0(contingency_matrix)
