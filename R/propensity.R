
# Propensity score matching -----------------------------------------------
# install.packages("MatchIt")
# library(MatchIt)
#gamma <- seq(0.05, 2.5, 0.05)
#n_matched_samples <- length(gamma)

# Matching Utils ----------------------------------------------------------

get_matched_df <- function(gamma, treatment_indicator, logit_propensity, seed){

  data <- data.frame(id = 1:length(treatment_indicator),
                     treatment_indicator = treatment_indicator,
                     logit_propensity = logit_propensity)

  matched_data <- NULL

  calipher <- get_calipher_width(gamma = gamma,
                     var_treated = get_var_logit_prop_score(group = 1,
                                                            logit_propensity = data$logit_propensity,
                                                            treatment_indicator = data$treatment_indicator),
                     var_untreated = get_var_logit_prop_score(group = 0,
                                                            logit_propensity = data$logit_propensity,
                                                            treatment_indicator = data$treatment_indicator))


  #while there are still participants in the controlgroup as well as in the treatment group

  while(sum(data$treatment_indicator) > 0 & (sum(data$treatment_indicator) < length(data$treatment_indicator))){

  set.seed(seed)

  # sample one candidate case for matching---------------------------------------------------------

  candidate_id <- sample_matching_candidate(data)



# Get distance to candidate_value -----------------------------------------

  distance <- compute_distance(candidate_id, data)


  # Get index of minimal distance -------------------------------------------

  match_id <- compute_id_min_dist(candidate_id, data)




  # Check whether minimal distance lower than calipher ----------------------

  if(min(distance$distance) < calipher){

    #add matched pair to matched data and remove from data

    matched_data <- as.data.frame(bind_rows(matched_data, data %>% filter(id %in% c(candidate_id, match_id))))

    data <- data %>% filter(!id %in% c(candidate_id, match_id))
  }else{

    #kick out candidate
    data <- data %>% filter(id != candidate_id)
   }

  }

return(matched_data)
}






# within matched pair difference in outcome between treated and un --------







# Obtain estimand ---------------------------------------------------------


# Obtain performance measures ---------------------------------------------



# treatment effect --------------------------------------------------------

mean(delta)

# One-sample t-test -------------------------------------------------------

t.test(delta, mu = 0, alternative = "two.sided")


# Standard error of the estimated difference in means ----------------------



# 95% confidence interval of mean treatment effect ------------------------


# Reduction in bias -------------------------------------------------------


# MSE of estimated difference in means ------------------------------------




