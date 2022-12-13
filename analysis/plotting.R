library(ggplot2)
library(tidyverse)

performance_plot <- function(plot_data, performance_measure, y_axis, title, outcome){

  legend_label <- switch(outcome,
                         "binary" = "True Risk Difference",
                         "continuous" = "True delta")

  performance_measure <- dplyr::enquo(performance_measure)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = gamma,
                                          y = !!performance_measure,
                                          group = factor(true_effect)),
                  color = true_effect,
                  ) +
            geom_point(aes(color = factor(true_effect)),
                       size = 2) +
            geom_line(aes(linetype = factor(true_effect),
                          color = factor(true_effect)),
                      size = 1) +
            theme_classic() +
            labs(title = title) +
            ylab(y_axis) +
            xlab("Calipher width: SD of logit of PS") +
            scale_color_manual(values = c("#00AFBB"), # "#E7B800", "#FC4E07"),
                              name = legend_label) + #,
                              #breaks = c("0.5", "1", "2"),
                              #labels = c("D0.5", "D1", "D2")
            scale_linetype_manual(values = c("solid"),
                                  name = legend_label)

          }
# Plot reduction in Bias --------------------------------------------------

plot_bias_reduction <- function(df, title, outcome, ...){

  # Summarizing plot data
  plot_data <- df %>%
    dplyr::group_by(scenario, gamma, true_effect) %>%
    dplyr::summarize(mean_bias_reduction = mean(reduction_bias))

  performance_plot(plot_data = plot_data,
                   performance_measure = mean_bias_reduction,
                   title = title,
                   y_axis = "Reduction in bias",
                   outcome = outcome
                   )
}
# Plot mean squared error (MSE) -------------------------------------------

plot_mse <- function(df, title, outcome, ...){

  # Summarizing plot data
  plot_data <- df %>%
    dplyr::group_by(scenario, gamma, true_effect) %>%
    dplyr::summarize(mse = mean(squared_error))

  performance_plot(plot_data = plot_data,
                   performance_measure = mse,
                   title = title,
                   y_axis = "MSE",
                   outcome = outcome)
}


