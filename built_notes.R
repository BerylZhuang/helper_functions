library("devtools")
library("roxygen2")

## notes: make sure tab is 4 spaces (Tools -> global options -> code editting)
#Build -> Configure build tools -> Code editing: tab width =4
#Build -> Configure build tools -> Build Tools -> check the box: Generate documention with roxygen -> Configure -> check all 6 boxes


setwd("~/git/bin/helper_functions/")
getwd()
devtools::load_all(".") ## get the description file = load all (Ctrl + shift + l)

roxygen2::roxygenise() #converts roxygen comments to .Rd files

# Build and reload (ctrl + shift + b)

#' notes:
#' Functions are in folder R
#' 