result_data <- result_ext3

# Binary outcomes type 1 error -------------------------------------------

result_data %>%
  dplyr::filter(scenario %in% c("indep_norm_bin_0",
                                "cor_norm_bin_0",
                                "mix_1_bin_0",
                                "mix_2_bin_0",
                                "indep_bin_bin_0")) %>%
  group_by(scenario, gamma) %>%
  summarize(type1error = mean(significance)) %>%
  ggplot2::ggplot(aes(x = gamma,
                      y = type1error,
                      group = factor(scenario))) +
  geom_point(aes(color = factor(scenario)),
             size = 2) +
  geom_line(aes(linetype = factor(scenario),
                color = factor(scenario)),
            size = 1.3) +
  geom_hline(yintercept = 0.05,
             color = "black") +
  scale_x_continuous(name = "Calipher width: SD of logit of PS",
                     breaks = c(0.05, seq(0.25, 2.5, 0.25))) +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = 'bold'),
    axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.line = element_line(size = 1.3)) +
  labs(title = "Binary outcomes: risk differences",
       color = "Scenario",
       linetype = "Scenario",
        y = "Empirical Type 1 Error rate")


c("indep_norm_bin_0", "indep_norm_bin_002", "indep_norm_bin_005",
  "indep_norm_bin_01", "indep_norm_bin_015", "cor_norm_bin_0",
  "cor_norm_bin_002", "cor_norm_bin_005", "cor_norm_bin_01", "cor_norm_bin_015",
  "mix_1_bin_0", "mix_1_bin_002", "mix_1_bin_005", "mix_1_bin_01",
  "mix_1_bin_015", "mix_2_bin_0", "mix_2_bin_002", "mix_2_bin_005",
  "mix_2_bin_01", "mix_2_bin_015", "indep_bin_bin_0", "indep_bin_bin_002",
  "indep_bin_bin_005", "indep_bin_bin_01", "indep_bin_bin_015")
