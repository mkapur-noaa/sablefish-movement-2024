# Source utils
source("R/utils.R")

# Define path
path <- "~/github/sablefish-data/data/tags_recovered.rda"

# Assign value
tags_recovered <- read_from_path(path = path)

# Write to data/
write_data(tags_recovered)
