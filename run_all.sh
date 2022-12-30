#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

# sbt commands for regression (long running) tests
sbt test

