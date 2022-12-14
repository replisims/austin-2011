
# Performance utils -------------------------------------------------------

#' Obtain within-matched pair difference d
#'
#' @param matched_df matched data frame with uneven rows corresponding to exposed
#' subjects and even rows to unexposed
#'
#' @return vector of within-matched pair differences

compute_delta <- function(matched_df){
  seq(from = 2, to = nrow(matched_df), by = 2) %>%
        purrr::map_dbl(~{matched_df$outcome[.x] - matched_df$outcome[.x - 1]})
}

#' Compute crude bias from simulated data
#'
#' @param sim_data simulated data set
#' @param true_treatment_effect true treatment effect
#'
#' @return numeric value indicating the crude bias
#' @export

bias_crude <- function(sim_data, true_treatment_effect){
  mean_treated <- sim_data %>%
    dplyr::filter(treatment_indicator == 1) %>%
    dplyr::select(outcome) %>%
    dplyr::summarize(mean = mean(outcome))

  mean_untreated <- sim_data %>%
    dplyr::filter(treatment_indicator == 0) %>%
    dplyr::select(outcome) %>%
    dplyr::summarize(mean = mean(outcome))
  (mean_treated[1, 1] - mean_untreated[1, 1]) - true_treatment_effect %>% unname()
}


#' Compute bias reduction
#'
#' @param crude_bias crude bias
#' @param ps_bias bias based on matched sample
#'
#' @return bias reduction
#' @export

bias_reduction <- function(crude_bias, ps_bias){
  100 * ((crude_bias - ps_bias) / crude_bias)
}

#' Bias based on matched pairs
#'
#' @param estimated_effect average within-matched pair difference (continuous outcomes)
#' or difference in proportions (binary outcome)
#' @param true_treatment_effect true treatment effect
#'
#' @return bias based on matched pairs
#' @export

bias_ps <- function(estimated_effect, true_treatment_effect){
  estimated_effect - true_treatment_effect
}


#' Compute difference in proportions based on matched pairs
#'
#' @param matched_data matched dataframe with uneven rows corresponding to exposed
#' subjects and even rows to unexposed
#'
#' @return
#' @export
#'
#' @examples
difference_proportions <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_data), by = 2)
  n_pairs <- nrow(matched_data)/2
  # b = pairs in which the treated subject experienced the event
  # while the untreated subject did not

  # c = pairs in which the untreated subject experienced the event while the treated subject does not

  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  (b-c)/n_pairs
}


#' Estimated variance of difference in proportions from matched pairs
#'
#' @param matched_data matched data frame with uneven rows corresponding to exposed
#' subjects and even rows to unexposed
#'
#' @return estimated variance
#' @export

var_difference_proportions <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_data), by = 2)
  n <- nrow(matched_data)/2
  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  ((b + c) - (c - b)^2 / n) / n^2

}

#' Obtain contingency matrix from matched data
#'
#' @param matched_data data frame with matched pairs uneven rows corresponding to exposed
#' subjects and even rows to unexposed
#'
#' @return
#' @export

get_contingency_matrix <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_data), by = 2)
  a <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 1)
  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  d <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 0)
  data <- matrix(c(a, c, b, d),
                 nrow = 2,
                 dimnames = list("Treated" = c("Event", "Non-event"),
                                "Untreated" = c("Event", "Non-event")))
}

#' Test Null hypothesis of equal proportions
#'
#' @param contingency_matrix
#'
#' @return
#' @export

get_h_0 <- function(contingency_matrix){
  mcnemar.test(contingency_matrix, correct = FALSE)$p.value < 0.05
}

