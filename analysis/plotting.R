library(ggplot2)
plot1 <- test_run11 %>%
  dplyr::group_by(gamma) %>%
  dplyr::summarize(mean_bias_reduction = mean(reduction_bias)) %>%
  ggplot2::ggplot(aes(x = gamma,
                     y = mean_bias_reduction)) +#,
                     #group = factor(!!facet2),
                     #color = factor(!!facet2),
                     #shape = factor(!!facet2))) +
  #facet_grid(cols = vars(!!facet3),
  #           rows = vars(!!facet4)) +
  geom_point(size = 3) +
  geom_line(size = 1) #+
  #geom_hline(yintercept = 0.1,
  #           linetype = "dashed",
  #           color = "black",
  #           size = 1) +
  # labs(title = "Bias Reduction") #+
  #ylab(yaxis)+
  #ggtitle(paste(titel,.$test_type))
