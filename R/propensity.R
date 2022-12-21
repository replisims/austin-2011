
# Propensity score matching -----------------------------------------------
# install.packages("MatchIt")
# library(MatchIt)
#gamma <- seq(0.05, 2.5, 0.05)
#n_matched_samples <- length(gamma)

# Matching Utils ----------------------------------------------------------

#' Create a data frame with matched data
#'
#' @param gamma caliper width
#' @param treatment_indicator vector indicating treatment or control per case
#' @param logit_propensity vector with logit of propensity score per case
#' @param seed
#'
#' @return a data frame with id's of matched cases
#' @export
get_matched_df <- function(gamma, treatment_indicator, logit_propensity, seed){

  set.seed(seed)
  # add an id column
  matching_data <- data.frame(id = 1:length(treatment_indicator),
                     treatment_indicator = treatment_indicator,
                     logit_propensity = logit_propensity)

  matched_data <- NULL

  caliper <- get_caliper_width(gamma = gamma,
                               var_treated = get_var_logit_prop_score(group = 1,
                                                                      logit_propensity = logit_propensity,
                                                                      treatment_indicator = treatment_indicator),
                               var_untreated = get_var_logit_prop_score(group = 0,
                                                                        logit_propensity = logit_propensity,
                                                                        treatment_indicator = treatment_indicator))


  #while there are still participants in the control group as well as in the treatment group

  while(sum(matching_data$treatment_indicator) > 0 & (sum(matching_data$treatment_indicator) < length(matching_data$treatment_indicator))){



  # sample one candidate case for matching---------------------------------------------------------

  candidate_id <- sample_matching_candidate(matching_data = matching_data)



  # Get distance to candidate_value -----------------------------------------

  distance_dat <- matching_data %>%
      dplyr::filter(treatment_indicator == 0) %>%
      dplyr::mutate(candidate_logit_ps = get_case_logit_propensity(case_id = candidate_id,
                                                                   matching_data = matching_data),
                    distance = abs(candidate_logit_ps - logit_propensity))

  min_dist <- min(distance_dat$distance)

  # Get index of minimal distance -------------------------------------------

  match_id <- distance_dat %>% dplyr::filter(distance == min_dist) %>% dplyr::pull(id)

  if(length(match_id) > 1){
    match_id <- sample(x = match_id, size = 1)
  }


  # Check whether minimal distance lower than caliper ----------------------

  if(min_dist < caliper){

    #add matched pair to matched data and remove from data

    matched_data <- as.data.frame(dplyr::bind_rows(matched_data, matching_data %>% dplyr::filter(id == candidate_id))) %>%
                                    dplyr::bind_rows(matching_data %>% dplyr::filter(id == match_id))

    matching_data <- matching_data %>% dplyr::filter(!id %in% c(candidate_id, match_id))
  }else{

    #kick out candidate
    matching_data <- matching_data %>% dplyr::filter(id != candidate_id)
   }

  }

return(matched_data)
}






# within matched pair difference in outcome between treated and un --------
