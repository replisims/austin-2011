true_rd <- c(0, -0.02, -0.05, -0.10, -0.1)
gamma_seq <- seq(0.05, 2.5, 0.05)

# trial_run.R -----------------------------------------------------------

run_sim <- function(sim_parameters, gamma_seq, seed){

  data <- rlang::exec("generate_data", !!!sim_parameters)

  sim_data <- data.frame(id = 1:nrow(data$sim_data),
                         data$sim_data)

  propensity_score <- get_propensity(treatment_indicator = sim_data$treatment_indicator,
                                     predictors = sim_data[, 1:sim_parameters$n_covariates])

  logit_propensity <- get_logit_propensity(propensity_score)

  # Map over all gamma values
  matched_data <- gamma_seq %>%
    purrr::map_dfr(~{matched_df <- get_matched_df(gamma = .x,
                                                  treatment_indicator = sim_data$treatment_indicator,
                                                  logit_propensity = logit_propensity,
                                                  seed = seed)

      matched_data <- dplyr::right_join(sim_data, matched_df,
                                        by = c("id", "treatment_indicator"))

      delta <- compute_delta(matched_data)

      average_delta <- mean(delta)

      t_test <- t.test(delta,
                       mu = 0,
                       alternative = "two.sided",
                       var.equal = TRUE)

      significance <- t_test$p.value < 0.05

      std_error <- t_test$stderr
      ci_95 <- t_test$conf.int

      crude_bias <- bias_crude(sim_data, sim_parameters$beta)

      ps_bias <- bias_ps(average_delta, true_treatment_effect = sim_parameters$beta)

      reduction_bias <- bias_reduction(
        crude_bias = bias_crude(sim_data = sim_data,
                                true_treatment_effect = sim_parameters$beta),
        ps_bias = bias_ps(average_delta = average_delta,
                          true_treatment_effect = sim_parameters$beta))

      performance <- tibble::tibble(
        repetition = seed,
        true_effect = sim_parameters$beta,
        estimated_effect = average_delta,
        ps_bias = ps_bias,
        crude_bias = crude_bias,
        gamma = .x,
        reduction_bias = reduction_bias,
        significance = significance,
        ci_95_low = ci_95[[1]],
        ci_95_up = ci_95[[2]]
      )}
      )
}

gamma_seq <- seq(0.05, 2.5, 0.05)
repetition <- 1:50

test_run <- purrr::map_df(repetition, ~{run_sim(sim_parameters = independent_normal_0,
        gamma_seq = gamma_seq,
        seed = .x)}
)

saveRDS(test_run, file = "analysis/data/test_run.rds")

test_run11 <- purrr::map_df(repetition, ~{run_sim(sim_parameters = independent_normal_11,
                                                gamma_seq = gamma_seq,
                                                seed = .x)}
)

saveRDS(test_run11, file = "analysis/data/test_run11.rds")
