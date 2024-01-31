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
      "date_released",
      "size_released",
      "region_released_3",
      "region_released_6",
      "region_released_8",
      "source"
    )
  )

# Write to data/
write_data(tag_data)

