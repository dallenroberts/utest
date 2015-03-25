################################################################################
## Author: Allen Roberts
## Date Created: March 2015
## Description: Read in raw UTEST data and format for analysis
################################################################################

################################################################################
## SET UP
################################################################################
library(plyr)

version <- "UTESTData_DATA_2015-03-25_1545"
data <- read.csv(paste0("data/raw/", version, ".csv"), stringsAsFactors = FALSE)