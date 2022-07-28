## Install and load packages

# rm(list=ls())

# install.packages("pacman") # install this package first if not installed

## Installing required packages
pacman::p_load(
  "reshape2", 
  "ggplot2",
  "ggpubr",
  "dplyr",
  "tidyverse",
  "kableExtra",
  "gridExtra",
  "ggpubr",
  "RColorBrewer",
  "cowplot",
  "reshape2",
  # "DataExplorer",
  "likert",
  "janitor",
  "ggdist",
  "table1",
  "gtsummary",
  "Hmisc"
)
  # compareGroups"

# package_list <- c()
# if(F){
#   install.packages(package_list)
# }
# 
# lapply(package_list, function(x) library(x, character.only = T))
# 
# rm(package_list)
