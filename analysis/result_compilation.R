
# Analysis ----------------------------------------------------------------
library(tidyverse)

results_20221220_072445_v02 <- readRDS(here::here("./analysis/data/raw_data/results_20221220_072445_v02.rds"))

result_data <- results_20221220_072445_v02 %>% purrr::map_df("result")



check_data <- result_data %>% dplyr::filter(scenario == "indep_bin_bin_015")


result_warnings <- results_20221219_113225_v01 %>% purrr::map("warnings")

check_data <- results_20221219_113225_v01 %>% purrr::map_df("result") %>% dplyr::filter(outcome_type == "continuous")
