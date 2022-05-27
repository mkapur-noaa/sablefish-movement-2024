# Source utils
source("R/utils.R")

# Load packages
library(magrittr)
library(readr)

# Define path
path <- "~/github/sablefish-data/data/numbers_at_length.rda"

# Assign value
abundance <- read_from_path(path = path) %>%
  # dplyr::filter(.data$length %in% c(41:71)) %>%
  dplyr::group_by(.data$region_name, .data$year) %>%
  dplyr::mutate(total = sum(.data$number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    .data$region_name,
    .data$region_short,
    .data$region_number,
    .data$year,
    .data$total
  ) %>%
  dplyr::distinct(.keep_all = TRUE)


# Write to data/
write_data(abundance)
