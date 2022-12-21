
# Matching utils ----------------------------------------------------------


#fitted values (p-hats) from a logistic regression of T on the subjects’ baseline (pre-treatment) characteristics.
#' Compute propensity scores
#'
#' @param treatment_indicator numeric variable indicating whether a case belongs to the treatment group or not
#' @param predictors
#'
#' @return vector with propensity scores
#' @export
#'
get_propensity <- function(treatment_indicator, predictors){
  fit_logistic <- glm(treatment_indicator ~ .,
                      data = cbind(treatment_indicator, predictors),
                      family = "binomial")

  predict(fit_logistic, type = "response")
}

#' Compute logit
#'
#' @param propensity_score
#'
#' @return logit
#' @export

compute_logit <- function(propensity_score){
  log(propensity_score/(1 - propensity_score))
}

# TODO PL  Better with "dynamic" parameters
get_propensity_logit <- function(...){
  compute_logit(get_propensity(...))
}

#' Compute variance of logit propensity score
#'
#' @param group
#' @param logit_propensity
#' @param treatment_indicator
#'
#' @return
#' @export

get_var_logit_prop_score <- function(group, logit_propensity, treatment_indicator){
  var(logit_propensity[which(treatment_indicator == group)])
}

#' Compute the caliper width
#'
#' @param gamma
#' @param var_treated
#' @param var_untreated
#'
#' @return
#' @export

get_calipher_width <- function(gamma, var_treated, var_untreated){
  gamma * sqrt((var_treated + var_untreated)/2)
}

#' get id's for matched data
#'
#' @param matching_data
#'
#' @return
#' @export

sample_matching_candidate <- function(matching_data){
  candidate_id <- matching_data %>%
    dplyr::filter(treatment_indicator == 1) %>%
    .$id
  candidate_id[sample(x = length(candidate_id), size = 1, replace = FALSE)]
}

#' get the logit propensity of a given id
#'
#' @param case_id
#' @param matching_data
#'
#' @return
#' @export
#'
#' @examples
get_case_logit_propensity <- function(case_id, matching_data){
  matching_data %>%
    dplyr::filter(id == case_id) %>%
    dplyr::select(logit_propensity) %>% as.numeric()
}

#' Compute distance of propensity score to candidate propensity score
#'
#' @param candidate_id
#' @param matching_data
#'
#' @return
#' @export

compute_distance <- function(candidate_id, matching_data){

  candidate_logit_ps <- get_case_logit_propensity(case_id = candidate_id,
                                                  matching_data = matching_data)
  matching_data %>%
  dplyr::filter(treatment_indicator == 0) %>%
    dplyr::transmute(distance = abs(candidate_logit_ps - logit_propensity))
}

#' Get the case with the smallest distance to the candidate case
#'
#' @param candidate_id
#' @param matching_data
#'
#' @return
#' @export

compute_id_min_dist <- function(candidate_id, matching_data){
  distance <-  compute_distance(candidate_id, matching_data)
  control_data <- matching_data %>%
    dplyr::filter(treatment_indicator == 0)
  matching_data <- dplyr::bind_cols(control_data, distance)
  min_dist <- matching_data %>%
    dplyr::filter(distance == min(matching_data$distance)) %>%
    dplyr::pull(id)

  if(length(min_dist > 1)){
    min_dist <- sample(x = min_dist, size = 1)
  }

  return(min_dist)
}

