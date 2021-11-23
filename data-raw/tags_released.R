# Source utils
source("R/utils.R")

# Define path
path <- "~/github/sablefish-data/data/tags_released.rda"

# Assign value
tags_released <- read_from_path(path = path)

# Write to data/
write_data(tags_released)
