# Source utils
source("R/utils.R")
# Load packages
library(magrittr)

# Define path
path <- "~/github/sablefish-data/data/harvest_rates.rda"

# Assign value
fishing_rate <- read_from_path(path = path) %>%
  dplyr::mutate(
    fishing_rate = -log(1 - .data$harvest_rate)
  ) %>%
  dplyr::select(
    .data$spatial,
    .data$name,
    .data$short,
    .data$number,
    .data$year,
    .data$fishing_rate
  )

# Write to data/
write_data(fishing_rate)
