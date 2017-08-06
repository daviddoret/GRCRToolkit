# Development Environment Setup

install.packages("devtools")

install.packages("testthat")
install.packages("roxygen2")
install.packages("rriskDistributions")

library(testthat)

#devtools::load_all(pkg=".")

# Run tests:
# Shortcut: Ctrl/Cmd + Shift + T
# or:
devtools::test()

install.packages("R6")
