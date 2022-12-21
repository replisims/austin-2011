#!/bin/bash

set -ux

mkdir -pv "$CACHE_DIR/$RESULTS_DIR/results"

aws s3 sync "s3://$RESULTS_BUCKET/$RESULTS_DIR/results" "$CACHE_DIR/$RESULTS_DIR/results"

Rscript cloud/collect_results.R
