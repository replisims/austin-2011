# trial_run.R -----------------------------------------------------------
sim_range <- 1:2

test_run2 <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_normal_cont_2",
          gamma_seq = seq(0.05, 2.5, 0.05),
          sim_parameters = indep_normal_cont_2,
          seed = .x)
})

saveRDS(test_run2, file = "analysis/data/test_run2.rds")



test_run_bin <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_norm_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[1:3],
          sim_parameters = indep_norm_bin_0,
          seed = .x)
})

saveRDS(test_run_bin, file = "analysis/data/test_run_bin.rds")

indep_norm_bin_0


test_run_bin2_long <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_norm_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05),
          sim_parameters = indep_norm_bin_01,
          seed = .x)
})

saveRDS(test_run_bin2_long, file = "analysis/data/test_run_bin2_long.rds")


test_run_bin2_long11 <- purrr::map_df(11:20, ~{
  run_sim(scenario = "indep_norm_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05),
          sim_parameters = indep_norm_bin_01,
          seed = .x)
})

saveRDS(test_run_bin2_long11, file = "analysis/data/test_run_bin2_long11.rds")



indep_norm_bin_01


plot_bias_reduction(df = test_run_bin2_long,
                    title = "Independant normal covariates - Binary Outcome")

plot_mse(df = test_run_bin2_long,
                    title = "Independant normal covariates - Binary Outcome")



test_run_binx <- purrr::map_df(sim_range, ~{
  run_sim(scenario = names(binary_scenarios)[13],
          gamma_seq = seq(0.05, 2.5, 0.05)[1:2],
          sim_parameters = binary_scenarios[[13]],
          seed = .x)
})


test_run_binz <- purrr::map_df(sim_range, ~{
  run_sim(scenario = names(continous_scenarios)[13],
          gamma_seq = seq(0.05, 2.5, 0.05)[1:2],
          sim_parameters = continous_scenarios[[13]],
          seed = .x)
})
