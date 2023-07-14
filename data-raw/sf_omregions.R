# Source utils
source("R/utils.R")
library(magrittr)

# Define path
path <- "~/github/sablefish-shapefiles/data/sf_omregions.rda"

# Assign value
sf_omregions <- read_from_path(path = path) %>%
  dplyr::mutate(region_short = omregion_short)

# Write to data/
write_data(sf_omregions)
