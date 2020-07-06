
# trial_run.R -------------------------------------------------------------


test_continuous <- generate_data(sample_size = 1000,
  n_covariates = 10,
  pair_cor = 0,
  alpha = c(1.1, 1.1, 1.1, 1.25, 1.25, 1.25, 1.5, 1.5, 1.5, 2),
  beta = 0,
  sigma_squared = 127.6056,
  outcome_type = "continuous",
  n_iter = 100,
  prop_treated = 0.25)


propensity_score <- get_propensity(treatment_indicator = test_continuous$sim_data$treatment_indicator,
                                   predictors = test_continuous$sim_data[,1:10])

logit_propensity <- get_logit_propensity(propensity_score)

matched_df <- get_matched_df(gamma = 0.3,
                             treatment_indicator = test_continuous$sim_data$treatment_indicator,
                             logit_propensity = logit_propensity,
                             seed = 42)


sim_data <- data.frame(id = 1:nrow(test_continuous$sim_data),
                       test_continuous$sim_data)

matched_data <- right_join(sim_data, matched_df)

delta <- compute_delta(matched_data)

average_delta <- mean(delta)

t_test <- t.test(delta,
                 mu = 0,
                 alternative = "two.sided",
                 var.equal = TRUE)


std_error <- t_test$stderr
ci_95 <- t_test$conf.int

bias_crude <- function(sim_data, true_treatment_effect){
  mean_treated <- sim_data %>%
    filter(treatment_indicator == 1) %>%
    select(outcome) %>%
    summarize(mean = mean(outcome))
  mean_untreated <- sim_data %>%
    filter(treatment_indicator == 0) %>%
    select(outcome) %>%
    summarize(mean = mean(outcome))
  (mean_treated - mean_untreated) - true_treatment_effect
}


bias_crude(sim_data, 0)

bias_ps <- function(average_delta, true_treatment_effect){
  average_delta - true_treatment_effect
}

bias_ps(average_delta, true_treatment_effect = 0)

bias_reduction <- function(crude_bias, ps_bias){
  100 * ((crude_bias - ps_bias) / crude_bias)
}

reduction_bias <- bias_reduction(
  crude_bias = bias_crude(sim_data = sim_data,
                          true_treatment_effect = 0),
  ps_bias = bias_ps(average_delta = average_delta,
                    true_treatment_effect = 0))





