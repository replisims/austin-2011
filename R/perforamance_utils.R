
# Performance utils -------------------------------------------------------

compute_delta <- function(matched_df){
  seq(from = 2, to = nrow(matched_df), by = 2) %>%
        map_dbl(~{matched_df$outcome[.x] - matched_df$outcome[.x - 1]})
}

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

bias_reduction <- function(crude_bias, ps_bias){
  100 * ((crude_bias - ps_bias) / crude_bias)
}

bias_ps <- function(average_delta, true_treatment_effect){
  average_delta - true_treatment_effect
}

# b = pairs in which the treated subject experienced the event
# while the untreated subject did not

# c = pairs in which the untreated subjext experienced the event while the treated subject does not

difference_proportions <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_df), by = 2)
  n_pairs <- nrow(matched_df)/2

  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  (b-c)/n_pairs
}


var_difference_proportions <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_df), by = 2)
  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  ((b + c) - (c - b)^2 / n) / n^2
}

get_contingency_matrix <- function(matched_data){
  even_index <- seq(from = 2, to = nrow(matched_df), by = 2)
  a <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 1)
  b <- sum(matched_data$outcome[even_index] == 1 & matched_data$outcome[even_index - 1] == 0)
  c <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 1)
  d <- sum(matched_data$outcome[even_index] == 0 & matched_data$outcome[even_index - 1] == 0)
  data <- matrix(c(a, c, b, d),
                 nrow = 2,
                 dimnames = list("Treated" = c("Event", "Non-event"),
                                "Untreated" = c("Event", "Non-event")))
}

get_h_0 <- function(contingency_matrix){
  mcnemar.test(contingency_matrix, correct = FALSE)$p.value < 0.05
}
