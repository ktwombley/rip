#!/bin/sh
R --vanilla -e "library(devtools); library(testthat); dev_mode(); load_all(); auto_test(\".\", \"tests\")"
