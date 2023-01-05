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
  -e RESULTS_DIR="batch/v9" \
  -e START_TIMES="20221222_153847 20221222_154427" \
  $IMAGE cloud/wrap_collect_results.sh
