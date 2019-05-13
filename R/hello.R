# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

# devtools::build_manual()

# install.packages('processx', repos = 'https://mran.microsoft.com/snapshot/2019-03-22/', dependencies=TRUE)
# install.packages('dplyr', repos = 'https://mran.microsoft.com/snapshot/2018-08-01/', dependencies=TRUE)
