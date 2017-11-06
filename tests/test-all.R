if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, grctoolkit)

# References:
# - https://github.com/nathanvan/minimalbugexample/blob/e6d62a6da7127fc165332935db04aa88b4575ae6/tests/test-all.R
# - help(test_check)

test_check("grctoolkit")
