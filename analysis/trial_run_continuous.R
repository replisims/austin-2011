gamma <- seq(0.05, 2.5, 0.05)

gamma_short <- c(0.05, 1, 2.5)

# trial run continuous --------------------------------------------------------


alpha_cont <- c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2)


test_cont <- generate_data(sample_size = 10000,
                             n_covariates = 10,
                             n_normal = 10,
                             alpha = alpha_cont,
                             pair_cor = 0,
                             prop_treated = 0.25,
                             beta =  2,
                             n_iter = 1000,
                             outcome_type = "continuous",
                             sigma_squared = 127.6056)

sim_data <- data.frame(id = 1:nrow(test_cont$sim_data),
                       test_cont$sim_data)

logit_propensity <- get_propensity_logit(treatment_indicator = sim_data$treatment_indicator,
                                         predictors = sim_data[, 2:11])

matched_df <- get_matched_df(gamma = 0.3,
                             treatment_indicator = sim_data$treatment_indicator,
                             logit_propensity = logit_propensity,
                             seed = 42)



propensity_score <- get_propensity(treatment_indicator = test_binary$sim_data$treatment_indicator,
                                   predictors = test_binary$sim_data[, 1:10])

hist(propensity_score)

n_matched <- nrow(matched_df)/2

prop_treated <- mean(sim_data$treatment_indicator)

matched_data <- dplyr::left_join(matched_df, sim_data,
                                 by = c("id", "treatment_indicator"))

delta <- compute_delta(matched_data)
hist(delta)

mean(delta)


sim_data <- data.frame(id = 1:nrow(test_binary$sim_data),
                       test_binary$sim_data)

logit_propensity <- compute_logit(propensity_score)

logit_propensity <- get_propensity_logit(treatment_indicator = sim_data$treatment_indicator,
                                         predictors = sim_data[, 1:10])

summary(logit_propensity)


matched_df <- get_matched_df(gamma = 0.3,
                             treatment_indicator = sim_data$treatment_indicator,
                             logit_propensity = logit_propensity,
                             seed = 42)

# matching_df <- data.frame(id = 1:length(test_binary$sim_data$treatment_indicator),
#                             treatment_indicator = test_binary$sim_data$treatment_indicator,
#                             logit_propensity = logit_propensity)

matched_data <- dplyr::left_join(matched_df, sim_data,
                                  by = c("id", "treatment_indicator"))

diff_prop <- difference_proportions(matched_data = matched_data)

# cand <- sample_matching_candidate(matching_df)
# # matched_df <- get_matched_df(gamma = 0.3,
# #                              treatment_indicator = test_binary$sim_data$treatment_indicator,
# #                              logit_propensity = logit_propensity,
# #                              seed = 42)
#
#
# matched_data <- NULL
#
# calipher <- get_calipher_width(gamma = 0.3,
#                                var_treated = get_var_logit_prop_score(group = 1,
#                                                                       logit_propensity = logit_propensity,
#                                                                       treatment_indicator = test_binary$sim_data$treatment_indicator),
#                                var_untreated = get_var_logit_prop_score(group = 0,
#                                                                         logit_propensity = logit_propensity,
#                                                                         treatment_indicator = test_binary$sim_data$treatment_indicator))
#
# distance <- compute_distance(candidate_id = cand, matching_data = matching_df)
#
# match_id <- compute_id_min_dist(cand, matching_df)
#
# min(distance$distance) < calipher
#
# matched_data <- as.data.frame(dplyr::bind_rows(matched_data, matching_df %>% dplyr::filter(id == cand)) %>%
#                                 dplyr::bind_rows(matching_df %>% dplyr::filter(id == match_id$id)))
#
# matching_df <- matching_df %>% dplyr::filter(!id %in% c(cand, match_id$id))


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
