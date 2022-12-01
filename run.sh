#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

# sbt commands for regression test
sbt testOnly Chainsaw.* -- -l *Isca2023Test *

