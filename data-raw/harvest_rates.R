# Source utils
source("R/utils.R")

# Define path
path <- "~/github/sablefish-data/data/harvest_rates.rda"

# Assign value
harvest_rates <- read_from_path(path = path)

# Write to data/
write_data(harvest_rates)
