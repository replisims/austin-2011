#!/bin/bash

set -ux

mkdir -pv "$CACHE_DIR"

Rscript cloud/cloud.R

# R_PID=$!

# psrecord $R_PID --interval 1 --include-children --log "$CACHE_DIR/profile.txt"

# wait $R_PID

# R_STATUS=$?

# sleep 2

# if [[ $R_STATUS == 0 ]]; then
#   echo "SUCCESS"
# else
#   echo "ERROR"
#   cat "$CACHE_DIR/profile.txt"
#   exit 1
# fi
