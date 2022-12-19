#!/bin/bash

set -eux

PROJECT="austin-2011"
IMAGE="$PROJECT-app-local"

docker build -t $IMAGE .

docker run --rm -it \
  -v ~/.aws:/root/.aws \
  -v "$(pwd)/cache:/tmp/cache" \
  -e AWS_PROFILE="default" \
  -e CACHE_DIR="/tmp/cache" \
  -e RESULTS_BUCKET="annloh-$PROJECT-staging-cache" \
  -e RESULTS_DIR="local/v4" \
  -e SCENARIO_ID=30 \
  -e FIRST_REPETITION_ID=1 \
  -e AWS_BATCH_JOB_ARRAY_INDEX=0 \
  -e START_TIME="20221215_000000" \
  $IMAGE cloud/wrap_cloud.sh
