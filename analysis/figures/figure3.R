result_data <- result_ext3

# Independent normal covariates coverage -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_norm_bin_0", "indep_norm_bin_002", "indep_norm_bin_005",
                                "indep_norm_bin_01", "indep_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")

# Correlated normal covariates coverage -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("cor_norm_bin_0",
                                "cor_norm_bin_002", "cor_norm_bin_005", "cor_norm_bin_01", "cor_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Correlated normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")


# Mixture (Scenario 1) covariates coverage-----------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("mix_1_bin_0", "mix_1_bin_002", "mix_1_bin_005", "mix_1_bin_01",
                                "mix_1_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Mixture (Scenario 1) covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")

# Mixture (Scenario 2) covariates coverage -----------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("mix_2_bin_0", "mix_2_bin_002", "mix_2_bin_005", "mix_2_bin_01",
                                "mix_2_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
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
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent binary covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")


# Independent normal covariates coverage continuous -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_normal_cont_0", "indep_normal_cont_11",
                                "indep_normal_cont_125", "indep_normal_cont_15", "indep_normal_cont_2")) %>%
  group_by(true_effect, gamma) %>%
  summarize(coverage = mean(coverage)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = coverage,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  scale_y_continuous(name = "Coverage of 95% CIs",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  scale_x_continuous(name = "Caliper width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True delta",
       linetype = "True delta")


