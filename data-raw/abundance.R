# Source utils
source("R/utils.R")

# Load packages
library(magrittr)
library(readr)

# Define path
path <- "~/github/sablefish-data/data/abundance.rda"

# Assign value
abundance <- read_from_path(path = path)

# Write to data/
write_data(abundance)
