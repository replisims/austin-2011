
# Independent normal covariates MSE -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_norm_bin_0", "indep_norm_bin_002", "indep_norm_bin_005",
                                "indep_norm_bin_01", "indep_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012),
                     limits = c(0, 0.012)) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")



# Correlated normal covariates MSE -------------------------------------------



result_data %>%
  dplyr::filter(scenario %in% c("cor_norm_bin_0",
                                "cor_norm_bin_002", "cor_norm_bin_005", "cor_norm_bin_01", "cor_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012),
                     limits = c(0, 0.012)) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Correlated normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")


# Mixture (Scenario 1) covariates -----------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("mix_1_bin_0", "mix_1_bin_002", "mix_1_bin_005", "mix_1_bin_01",
                                "mix_1_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012),
                     limits = c(0, 0.012)) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Mixture (Scenario 1) covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")

# Mixture (Scenario 2) covariates -----------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("mix_2_bin_0", "mix_2_bin_002", "mix_2_bin_005", "mix_2_bin_01",
                                "mix_2_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012),
                     limits = c(0, 0.012)) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Mixture (Scenario 2) covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")

# Independent binary covariates -----------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_bin_bin_0", "indep_bin_bin_002",
                                "indep_bin_bin_005", "indep_bin_bin_01", "indep_bin_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     # breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.010, 0.012),
                     # limits = c(0, 0.012)
                     ) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent binary covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")


# Independent normal covariates MSE continuous -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_normal_cont_0", "indep_normal_cont_11",
                                "indep_normal_cont_125", "indep_normal_cont_15", "indep_normal_cont_2")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mse = mean(squared_error)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mse,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "MSE",
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                     limits = c(0, 10)) +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True delta",
       linetype = "True delta")
