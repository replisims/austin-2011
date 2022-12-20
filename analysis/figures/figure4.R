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
  scale_y_continuous(name = "Empirical Type 1 Error rate",
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     limits = c(0, 1)) +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = 'bold'),
    axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.line = element_line(size = 1.3)) +
  labs(title = "Binary outcomes: risk differences",
       color = "Scenario",
       linetype = "Scenario")

