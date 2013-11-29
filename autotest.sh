#!/bin/sh
R --vanilla -e "library(devtools); library(testthat); dev_mode(); load_all(); auto_test(\"./R\", \"./inst/tests/\")"
