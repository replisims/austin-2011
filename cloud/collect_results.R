library(reticulate)
boto3 <- import('boto3')

library(tidyverse)
library(futile.logger)

library(magrittr)


##### Get configuration
base_dir <- sprintf('%s/%s', Sys.getenv('CACHE_DIR'), Sys.getenv('RESULTS_DIR'))
start_times <- strsplit(Sys.getenv('START_TIMES'), split='\\s+')[[1]]

results_dir <- sprintf('%s/results', base_dir)

results <- list()
last_prefix <- ''
warns <- list()
errors <- list()
for (filename in sort(list.files(results_dir))) {
  filepath <- sprintf('%s/%s', results_dir, filename)
  prefix <- sub('(scenario_\\d+-repetition_\\d+).*', '\\1', filename)
  if (prefix == last_prefix) {
    flog.warn('Ignored because of duplicate prefix: %s', filepath)
    # errors %<>% c(filepath)
  } else if (! any(map_lgl(start_times, ~ grepl(., filename, fixed = TRUE)))) {
    flog.warn('Ignored because of unmatched start_time: %s', filepath)
    # warns %<>% c(filepath)
  }
  else {
    flog.info('Loading %s', filepath)
    results <- c(results, list(readRDS(filepath)))
    # results <- append(results, readRDS(filepath))
    last_prefix <- prefix
  }
}
# for (error in errors)
# for (warn in warns)

# ordered_results <- results[order(unlist(map(results, ~ .$scenario_id)), unlist(map(results, ~ .$job_id)))]

print(results)

# result <- bind_rows(results)
# result <- do.call(bind_rows, results)

# print(result)

joined_results_filepath <- sprintf('%s/results.rds', base_dir)
flog.info('Saving joined file %s', joined_results_filepath)
saveRDS(results, file = joined_results_filepath)


# View(results)


# resources_dir <- sprintf('%s/resources', base_dir)

# resources <- list()
# for (filename in list.files(resources_dir)) {
#   print(filename)
#   resources[[filename]] <- readRDS(sprintf('%s/%s', resources_dir, filename))
# }
# glimpse(resources)
