# Source utils
source("R/utils.R")
# Load packages
library(magrittr)

# Define paths
path_released <- "~/github/sablefish-data/data/tags_released.rda"
path_recovered <- "~/github/sablefish-data/data/tags_recovered.rda"

# Assign value
tags_released <- read_from_path(path = path_released)
tags_recovered <- read_from_path(path = path_recovered)

# Tag data
tag_data <- tags_released %>%
  dplyr::left_join(
    tags_recovered,
    by = c(
      "tag_id",
      "released_date",
      "released_length",
      "released_region",
      "released_omregion",
      "source"
    )
  ) %>%
  dplyr::rename(
    date_released = .data$released_date,
    date_recovered = .data$recovered_date,
    region_released = .data$released_region,
    region_recovered = .data$recovered_region,
    omregion_released = .data$released_omregion,
    omregion_recovered = .data$recovered_omregion,
    size_released = .data$released_length,
    size_recovered = .data$recovered_length
  )

# Write to data/
write_data(tag_data)
