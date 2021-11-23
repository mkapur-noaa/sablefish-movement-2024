# Source utils
source("R/utils.R")

# Define path
path <- "~/github/sablefish-data/data/numbers_at_length.rda"

# Assign value
numbers_at_length <- read_from_path(path = path)

# Write to data/
write_data(numbers_at_length)
