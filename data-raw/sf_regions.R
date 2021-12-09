# Source utils
source("R/utils.R")

# Define path
path <- "~/github/sablefish-shapefiles/data/sf_regions.rda"

# Assign value
sf_regions <- read_from_path(path = path)

# Write to data/
write_data(sf_regions)
