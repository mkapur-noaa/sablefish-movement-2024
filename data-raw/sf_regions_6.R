# Packages
library(sf)

# Source utils
source("R/utils.R")
library(magrittr)

# Define path
path <- "~/github/sablefish-shapefiles/data/sf_regions_6.rda"

# Assign value
sf_regions_6 <- read_from_path(path = path)

# Write to data/
write_data(sf_regions_6)
