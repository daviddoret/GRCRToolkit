# Development Environment Setup

install.packages("devtools")

install.packages("testthat")

library(testthat)

#devtools::load_all(pkg=".")

# Run tests:
# Shortcut: Ctrl/Cmd + Shift + T
# or:
devtools::test()
