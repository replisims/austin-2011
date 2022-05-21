#' Run simulation
#'
#' @param scenario string indicating the name of the current parameter constellation
#' @param sim_parameters list of simulation parameters (i.e. input for the \code{generate_data()} function)
#' @param gamma_seq vector of gammas (i.e. calipher widths) for the propensity score matching.
#' Defaults to the sequence from the manuscript.
#' @param seed seed (defaults to 42)
#'
#' @return Returns a tibble with the following columns:
#' \scenario
#' \item seed
#' \item true_effect
#' \item estimated_effect
#' \item ps_bias
#' \item crude_bias
#' \item gamma
#' \item reduction_bias
#' \item significance
#' \item coverage
#'
#' @export

run_sim <- function(scenario = "unnamed",
                    sim_parameters,
                    gamma_seq = seq(from = 0.05,
                                    to = 2.5,
                                    by = 0.05),
                    seed = 42){
  start_time <- Sys.time()

  # Generate data according to simulation parameters
  data <- rlang::exec(generate_data, !!!sim_parameters)

  end_time <- Sys.time()
  timediff <- end_time - start_time
  print(timediff)
   sim_data <- data.frame(id = 1:nrow(data$sim_data),
                         data$sim_data)

  # Get propensity scores for each case
  logit_propensity <- get_propensity_logit(treatment_indicator = sim_data$treatment_indicator,
                                           predictors = sim_data[, 1:sim_parameters$n_covariates])

  # Map over the sequence of gamma values (calipher widths)
  purrr::map_dfr(gamma_seq, ~{
    matched_df <- get_matched_df(gamma = .x,
                                 treatment_indicator = sim_data$treatment_indicator,
                                 logit_propensity = logit_propensity,
                                 seed = seed)

    # join the matched cases to the original data
    matched_data <- dplyr::right_join(sim_data, matched_df,
                                      by = c("id", "treatment_indicator"))

    # Performance measures for continuous outcomes
    if(sim_parameters$outcome_type == "continuous") {

      # Vector of differences in means
      delta <- compute_delta(matched_data)

      # Mean of differences in means
      average_delta <- mean(delta)

      # Perform t-test to test whether effect is sign different from zero
      t_test <- t.test(delta,
                       mu = 0,
                       alternative = "two.sided",
                       var.equal = TRUE)

      # Logical value indicating whether the t-test was significant
      significance <- t_test$p.value < 0.05

      # TODO PL  where is this used?
      std_error <- t_test$stderr

      ci_95_low <- t_test$conf.int[[1]]

      ci_95_up <- t_test$conf.int[[2]]

      crude_bias <- bias_crude(sim_data = sim_data,
                               true_treatment_effect = - sim_parameters$beta)

      ps_bias <- bias_ps(estimated_effect = average_delta,
                         true_treatment_effect = - sim_parameters$beta)

      reduction_bias <- bias_reduction(crude_bias = crude_bias,
                                       ps_bias = ps_bias)

      # Logical value indicating whether the true effect is within the 95% CI
      coverage <- (ci_95_low < - sim_parameters$beta) & (sim_parameters$beta < ci_95_up)

      return(tibble::tibble(scenario = scenario,
                            outcome_type = "continuous",
                            seed = seed,
                            true_effect = - sim_parameters$beta,
                            estimated_effect = average_delta,
                            ps_bias = ps_bias,
                            crude_bias = crude_bias,
                            gamma = .x,
                            reduction_bias = reduction_bias,
                            significance = significance,
                            coverage = coverage,
                            squared_error = (- sim_parameters$beta - estimated_effect)^2
      )
      )
    }

    # Performance measures for binary outcomes
    if(sim_parameters$outcome_type == "binary"){

      diff_prop <- difference_proportions(matched_data = matched_data)

      var_diff_prop <- var_difference_proportions(matched_data = matched_data)

      # Logical value indicating whether the true effect is within the 95% CI
      coverage <- (diff_prop - 1.96 * sqrt(var_diff_prop) < sim_parameters$risk_diff) &
                  ((diff_prop + 1.96 * sqrt(var_diff_prop) > sim_parameters$risk_diff) )

      crude_bias <- bias_crude(sim_data = sim_data,
                               true_treatment_effect = sim_parameters$risk_diff)

      ps_bias <- bias_ps(estimated_effect = diff_prop,
                         true_treatment_effect = sim_parameters$risk_diff)

      reduction_bias <- bias_reduction(crude_bias = crude_bias,
                                       ps_bias = ps_bias)

      contingency_matrix <- get_contingency_matrix(matched_data)

      significance <- get_h_0(contingency_matrix)

      return(tibble::tibble(scenario = scenario,
                            outcome_type = "binary",
                            seed = seed,
                            true_effect = sim_parameters$risk_diff,
                            estimated_effect = diff_prop,
                            ps_bias = ps_bias,
                            crude_bias = crude_bias,
                            gamma = .x,
                            reduction_bias = reduction_bias,
                            significance = significance,
                            coverage = coverage,
                            squared_error = (sim_parameters$risk_diff - diff_prop)^2
      )
      )
    }
  })
}
