# dump








# Obtain estimand ---------------------------------------------------------


# Obtain performance measures ---------------------------------------------



# treatment effect --------------------------------------------------------

treatment_effect <- mean(delta)

# Continuous outcome
# One-sample t-test -------------------------------------------------------

signifficant_effect <- t.test(delta, mu = 0,
                              alternative = "two.sided",
                              var.equal = TRUE)$p.value < 0.05


# Standard error of the estimated difference in means ----------------------

std_error_delta <- t.test(delta, mu = 0, alternative = "two.sided", var.equal = TRUE)$stderr

# 95% confidence interval of mean treatment effect ------------------------

ci_delta <- t.test(delta, mu = 0, alternative = "two.sided", var.equal = TRUE)$conf.int

# Reduction in bias -------------------------------------------------------


# MSE of estimated difference in means ------------------------------------

mse <- function(observed_effect, true_effect){
  mean((observed_effect - true_effect)^2)
}




