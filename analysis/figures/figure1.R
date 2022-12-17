# Independent normal covariates -------------------------------------------

result_data <- result_ext3

result_data %>%
  dplyr::filter(scenario %in% c("indep_norm_bin_0", "indep_norm_bin_002", "indep_norm_bin_005",
                                "indep_norm_bin_01", "indep_norm_bin_015")) %>%
  mutate(bias_red_abs = 100 *((abs(crude_bias) - abs(ps_bias))/abs(crude_bias))) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(bias_red_abs)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mean_bias_red,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Reduction in bias") +
  xlab("Calipher width: SD of logit of PS")



result_data %>%
  dplyr::filter(scenario %in% c("indep_norm_bin_0", "indep_norm_bin_002", "indep_norm_bin_005",
                                "indep_norm_bin_01", "indep_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(reduction_bias),
            sd_bias_red = sd(reduction_bias)) %>% View()


# Correlated normal covariates --------------------------------------------



result_data %>%
  dplyr::filter(scenario %in% c("cor_norm_bin_0",
                                "cor_norm_bin_002", "cor_norm_bin_005", "cor_norm_bin_01", "cor_norm_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(reduction_bias)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mean_bias_red,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  theme_classic() +
  labs(title = "Correlated normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Reduction in bias") +
  xlab("Calipher width: SD of logit of PS")


# Mixture (Scenario 1) covariates -----------------------------------------



result_data %>%
  dplyr::filter(scenario %in% c("mix_1_bin_0", "mix_1_bin_002", "mix_1_bin_005", "mix_1_bin_01",
                                "mix_1_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(reduction_bias)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mean_bias_red,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  theme_classic() +
  labs(title = "Mixture (Scenario 1) covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Reduction in bias") +
  xlab("Calipher width: SD of logit of PS")


# Mixture (Scenario 2) covariates -----------------------------------------



result_data %>%
  dplyr::filter(scenario %in% c("mix_2_bin_0", "mix_2_bin_002", "mix_2_bin_005", "mix_2_bin_01",
                                "mix_2_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(reduction_bias)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mean_bias_red,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  theme_classic() +
  labs(title = "Mixture (Scenario 2) covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Reduction in bias") +
  xlab("Calipher width: SD of logit of PS")



# Independent binary covariates -----------------------------------------



result_data %>%
  dplyr::filter(scenario %in% c( "indep_bin_bin_0", "indep_bin_bin_002",
                                 "indep_bin_bin_005", "indep_bin_bin_01", "indep_bin_bin_015")) %>%
  group_by(true_effect, gamma) %>%
  summarize(mean_bias_red = mean(reduction_bias)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = mean_bias_red,
                      group = factor(true_effect))) +
  geom_point(aes(color = factor(true_effect)),
             size = 2) +
  geom_line(aes(linetype = factor(true_effect),
                color = factor(true_effect)),
            size = 1) +
  theme_classic() +
  labs(title = "Independent binary covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Reduction in bias") +
  xlab("Calipher width: SD of logit of PS")
