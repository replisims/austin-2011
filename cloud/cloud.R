library(reticulate)
boto3 <- import('boto3')

library(tidyverse)
library(futile.logger)

source("R/data_generation_utils.R")
source("R/generate_data.R")
source("R/matching_utils.R")
source("R/performance_utils.R")
source("R/propensity.R")
source("R/run_sim.R")
source("R/scenarios.R")


##### Get configuration
cache_dir <- Sys.getenv('CACHE_DIR')
results_bucket <- Sys.getenv('RESULTS_BUCKET')
results_dir <- Sys.getenv('RESULTS_DIR')

scenario_id <- strtoi(Sys.getenv('SCENARIO_ID'))
repetition_id <- strtoi(Sys.getenv('FIRST_REPETITION_ID')) + strtoi(Sys.getenv('AWS_BATCH_JOB_ARRAY_INDEX', unset="0"))
start_time <- Sys.getenv('START_TIME')

scenario_name <- names(scenarios)[scenario_id]
scenario_sim_parameters <- scenarios[[scenario_id]]


##### Do the magic
job_description <- sprintf('Scenario %s (ID %d), Repetition %d, Started %s', scenario_name, scenario_id, repetition_id, start_time)
flog.info('Computing: %s ...', job_description)

job_start_time = Sys.time()

gamma_seq <- c(0.05, seq(0.25, 2.5, 0.25))

job_results <- run_sim_quiet(
  scenario = scenario_name,
  gamma_seq = gamma_seq,
  sim_parameters = scenario_sim_parameters,
  seed = repetition_id
)

flog.info('Computed: %s', job_description)
# flog.info('Computed scenarios: %d', length(job_results))
flog.info('Computation duration: %f sec', as.numeric(Sys.time() - job_start_time, units='secs'))

##### Save results
job_filename <- sprintf('scenario_%d-repetition_%d-time_%s.rds', scenario_id, repetition_id, start_time)
results_filepath <- sprintf('%s/%s', cache_dir, job_filename)
saveRDS(job_results, file = results_filepath)


##### Upload results
s3_client <- boto3$client('s3')

results_s3_key <- sprintf('%s/results/%s', results_dir, job_filename)
flog.info('Uploading %s to s3://%s/%s', results_filepath, results_bucket, results_s3_key)
s3_client$upload_file(
  Filename = results_filepath,
  Bucket = results_bucket,
  Key = results_s3_key
)

file.remove(results_filepath)


# ##### Upload resource usage
# resources_filepath <- sprintf('%s/profile.txt', cache_dir)
# resources_s3_key <- sprintf('%s/resources/%s.txt', results_dir, job_filename)
# flog.info('Uploading %s to s3://%s/%s', resources_filepath, results_bucket, resources_s3_key)
# s3_client$upload_file(
#   Filename = resources_filepath,
#   Bucket = results_bucket,
#   Key = resources_s3_key
# )


flog.info('All done')




# # flog.info('Start')

# # test_run <- run_sim(
# #   scenario = "indep_normal_cont_2",
# #   gamma_seq = c(1.0), #seq(0.05, 2.5, 0.05),
# #   sim_parameters = indep_normal_cont_2,
# #   seed = repetition_id
# # )

# # print(test_run)

# # saveRDS(test_run, file = "/tmp/cache/test_run.rds")

# # flog.info('All done')

