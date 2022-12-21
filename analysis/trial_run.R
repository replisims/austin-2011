# trial_run.R -----------------------------------------------------------
sim_range <- 3

test_run2a <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_normal_cont_2",
          gamma_seq = seq(0.05, 2.5, 0.05),
          sim_parameters = indep_normal_cont_2,
          seed = .x)
})

saveRDS(test_run2, file = "analysis/data/test_run2a.rds")


# Testing all binary scenarios with rd = 0 --------------------------------------------

start_time <- Sys.time()

test_run_bin <- purrr::map(sim_range, ~{   #ran
  run_sim(scenario = "indep_norm_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_norm_bin_0,
          seed = .x)
})

test_run_bin2 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "cor_norm_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[c(3, 48)],
          sim_parameters = cor_norm_bin_0,
          seed = .x)
})

test_run_bin3 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "mix_1_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[c(3, 48)],
          sim_parameters = mix_1_bin_0,
          seed = .x)
})

test_run_bin4 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "mix_2_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[c(3, 48)],
          sim_parameters = mix_2_bin_0,
          seed = .x)
})

test_run_bin5 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "indep_bin_bin_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[c(3, 48)],
          sim_parameters = indep_bin_bin_0,
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time
print(diff_time)


# Testing all binary scenarios with rd = 002 --------------------------------------------

test_run_bin002 <- purrr::map_df(sim_range, ~{ #ran
  run_sim_quiet(scenario = "indep_norm_bin_002",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_norm_bin_002,
          seed = .x)
})


test_run_bin2002 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "cor_norm_bin_002",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = cor_norm_bin_002,
          seed = .x)
})

test_run_bin3002 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_1_bin_002",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_1_bin_002,
          seed = .x)
})

test_run_bin4002 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_2_bin_002",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_2_bin_002,
          seed = .x)
})

test_run_bin5002 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "indep_bin_bin_002",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_bin_bin_002,
          seed = .x)
})


# Testing all binary scenarios with rd = 002 --------------------------------------------

start_time <- Sys.time()

test_run_bin005 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "indep_norm_bin_005",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
          sim_parameters = indep_norm_bin_005,
          seed = .x)
})

View(test_run_bin005[[1]]$result)

test_run_bin2005 <- purrr::map(sim_range, ~{ #ran
  run_sim_quiet(scenario = "cor_norm_bin_005",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = cor_norm_bin_005,
          seed = .x)
})

test_run_bin3005 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_1_bin_005",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_1_bin_005,
          seed = .x)
})

test_run_bin4005 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_2_bin_005",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_2_bin_005,
          seed = .x)
})

test_run_bin5005 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "indep_bin_bin_005",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_bin_bin_005,
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time
print(diff_time)


# Testing all binary scenarios with rd = 01 --------------------------------------------

start_time <- Sys.time()

test_run_bin01 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "indep_norm_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_norm_bin_01,
          seed = .x)
})


test_run_bin201 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "cor_norm_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = cor_norm_bin_01,
          seed = .x)
})

test_run_bin301 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_1_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_1_bin_01,
          seed = .x)
})

test_run_bin401 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "mix_2_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_2_bin_01,
          seed = .x)
})

test_run_bin501 <- purrr::map_df(sim_range, ~{ #ran
  run_sim(scenario = "indep_bin_bin_01",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_bin_bin_01,
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time
print(diff_time)

# Testing all binary scenarios with rd = 015 --------------------------------------------

start_time <- Sys.time()

test_run_bin015 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_norm_bin_015",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_norm_bin_015,
          seed = .x)
})


test_run_bin2015 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "cor_norm_bin_015",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = cor_norm_bin_015,
          seed = .x)
})

test_run_bin3015 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_1_bin_015",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_1_bin_015,
          seed = .x)
})

test_run_bin4015 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_2_bin_015",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_2_bin_015,
          seed = .x)
})

test_run_bin5015 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_bin_bin_015",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_bin_bin_015,
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time
print(diff_time)



# Testing the plotting ----------------------------------------------------

plot_bias_reduction(df = test_run2a,
                    title = "Independant normal covariates - Continuous Outcome",
                    outcome = "continuous")

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
          gamma_seq = seq(0.05, 2.5, 0.05)[1],
          sim_parameters = continuous_scenarios[[13]],
          seed = .x)
})

test_run_binb

start_time <- Sys.time()

test_run_binxz <- purrr::map_df(c(1), ~{
  run_sim(scenario = names(binary_scenarios)[3],
          gamma_seq = seq(0.05, 2.5, 0.05)[25],
          sim_parameters = binary_scenarios[[3]],
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time


start_timebc <- Sys.time()
test_run_binb <- purrr::map_df(c(4), ~{
  run_sim(scenario = names(continuous_scenarios)[22],
          gamma_seq = seq(0.05, 2.5, 0.05)[25],
          sim_parameters = continuous_scenarios[[22]],
          seed = .x)
})

end_timeb <- Sys.time()
diff_timeb <- end_timeb - start_timebc



# Testing all continuous scenarios ----------------------------------------


test_run_cont <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_normal_cont_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_normal_cont_0,
          seed = .x)
})


test_run_cont2 <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "cor_normal_cont_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = cor_normal_cont_0,
          seed = .x)
})

test_run_cont3 <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "mix_1_cont_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_1_cont_0,
          seed = .x)
})

test_run_cont4 <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "mix_2_cont_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = mix_2_cont_0,
          seed = .x)
})

test_run_cont5 <- purrr::map_df(sim_range, ~{
  run_sim(scenario = "indep_bin_cont_0",
          gamma_seq = seq(0.05, 2.5, 0.05)[48:50],
          sim_parameters = indep_bin_cont_0,
          seed = .x)
})

# Testing all continuous scenarios 11 ----------------------------------------

start_time <- Sys.time()

test_run_cont11 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_normal_cont_11",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = indep_normal_cont_11,
          seed = .x)
})


test_run_cont211 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "cor_normal_cont_11",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = cor_normal_cont_11,
          seed = .x)
})

test_run_cont311 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_1_cont_11",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = mix_1_cont_11,
          seed = .x)
})

test_run_cont411 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_2_cont_11",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = mix_2_cont_11,
          seed = .x)
})

test_run_cont511 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_bin_cont_11",
          gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
          sim_parameters = indep_bin_cont_11,
          seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time


# Testing all continuous scenarios 125 ----------------------------------------

start_time <- Sys.time()

test_run_cont125 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_normal_cont_125",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
                sim_parameters = indep_normal_cont_125,
                seed = .x)
})


test_run_cont2125 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "cor_normal_cont_125",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
                sim_parameters = cor_normal_cont_125,
                seed = .x)
})

test_run_cont3125 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_1_cont_125",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
                sim_parameters = mix_1_cont_125,
                seed = .x)
})

test_run_cont4125 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_2_cont_125",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
                sim_parameters = mix_2_cont_125,
                seed = .x)
})

test_run_cont5125 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_bin_cont_125",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25)),
                sim_parameters = indep_bin_cont_125,
                seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time

# Testing all continuous scenarios 15 ----------------------------------------


start_time <- Sys.time()

test_run_cont15 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_normal_cont_15",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = indep_normal_cont_15,
                seed = .x)
})


test_run_cont215 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "cor_normal_cont_15",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = cor_normal_cont_15,
                seed = .x)
})

test_run_cont315 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_1_cont_15",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = mix_1_cont_15,
                seed = .x)
})

test_run_cont415 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_2_cont_15",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = mix_2_cont_15,
                seed = .x)
})

test_run_cont515 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_bin_cont_15",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = indep_bin_cont_15,
                seed = .x)8
})

end_time <- Sys.time()
diff_time <- end_time - start_time


# Testing all continuous scenarios 2 ----------------------------------------


start_time <- Sys.time()

test_run_cont2 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_normal_cont_2",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = indep_normal_cont_2,
                seed = .x)
})


test_run_cont22 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "cor_normal_cont_2",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = cor_normal_cont_2,
                seed = .x)
})

test_run_cont32 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_1_cont_2",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = mix_1_cont_2,
                seed = .x)
})

test_run_cont42 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "mix_2_cont_2",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = mix_2_cont_2,
                seed = .x)
})

test_run_cont52 <- purrr::map(sim_range, ~{
  run_sim_quiet(scenario = "indep_bin_cont_2",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = indep_bin_cont_2,
                seed = .x)
})

end_time <- Sys.time()
diff_time <- end_time - start_time

test_bias <- purrr::map(1:5, ~{
run_sim_quiet(scenario = "indep_norm_bin_015",
              gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1, 5, 11)],
              sim_parameters = indep_norm_bin_015,
              seed = .x)
})


test_bias2 <- purrr::map(6:20, ~{
  run_sim_quiet(scenario = "indep_norm_bin_015",
                gamma_seq = c(0.05, seq(0.25, 2.5, 0.25))[c(1,5,11)],
                sim_parameters = indep_norm_bin_015,
                seed = .x)
})

test_bias_df <- test_bias %>% purrr::map_df("result")
test_bias_df2 <- test_bias2 %>% purrr::map_df("result")

test_bias_df_all <- bind_rows(test_bias_df, test_bias_df2)

test_bias_df2 <- test_bias_df %>% mutate(risk_diff_unadj = contingency_matrix_unmatched %>% map_dbl(~risk_diff(.x)))


devtools::load_all()

result_data <- results_20221219_113225_v01 %>% purrr::map_df("result")

results_20221219_113225_v01 %>% head
