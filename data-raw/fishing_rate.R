# Source utils
source("R/utils.R")
# Load packages
library(magrittr)

# Define path
path <- "~/github/sablefish-data/data/fishing_rates.rda"

# Assign value
fishing_rate <- read_from_path(path = path) %>%
  dplyr::select(
    spatial,
    name,
    short,
    number,
    year,
    fishing_rate
  )

# Write to data/
write_data(fishing_rate)
