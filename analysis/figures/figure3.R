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
  theme_classic() +
  labs(title = "Independent normal covariates",
       color = "True Risk Difference",
       linetype = "True Risk Difference")+
  ylab("Coverage of 95% CIs") +
  xlab("Calipher width: SD of logit of PS")
