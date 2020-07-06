
# Matching utils ----------------------------------------------------------


#fitted values (p-hats) from a logistic regression of T on the subjects’ baseline (pre-treatment) characteristics.
get_propensity <- function(treatment_indicator, predictors){
  fit_logistic <- glm(treatment_indicator ~ .,
                    data = cbind(treatment_indicator, predictors),
                    family = "binomial")

  predict(fit_logistic, type = "response")
}

get_logit_propensity <- function(propensity_score){
  log(propensity_score/(1 - propensity_score))
}


get_var_logit_prop_score <- function(group, logit_propensity, treatment_indicator){
  var(logit_propensity[which(treatment_indicator == group)])
}

get_calipher_width <- function(gamma, var_treated, var_untreated){
  gamma*sqrt((var_treated + var_untreated)/2)
}

sample_matching_candidate <- function(data){
  candidate_id <- data %>%
    dplyr::filter(treatment_indicator == 1) %>%
    .$id
  candidate_id[sample(x = length(candidate_id), size = 1, replace = FALSE)]
}

get_case_logit_propensity <- function(case_id, data){
  data %>%
    filter(id == case_id) %>%
    select(logit_propensity) %>% as.numeric()
}

compute_distance <- function(candidate_id, data){

  candidate_logit_ps <- get_case_logit_propensity(case_id = candidate_id, data)
  data %>%
  dplyr::filter(treatment_indicator == 0) %>%
  transmute(distance = abs(candidate_logit_ps - logit_propensity))
}

compute_id_min_dist <- function(candidate_id, data){
  distance <-  compute_distance(candidate_id, data)
  control_data <- data %>% filter(treatment_indicator == 0)
  data <- bind_cols(control_data, distance)
  data %>%
    filter(distance == min(data$distance)) %>%
    select(id)
}

