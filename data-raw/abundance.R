# Source utils
source("R/utils.R")

# Load packages
library(magrittr)
library(readr)

# Define path
path <- "~/github/sablefish-data/data/abundance.rda"

# # Assign value
# abundance <- read_from_path(path = path) %>%
#   dplyr::group_by(.data$region_name, .data$year) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(
#     .data$region_name,
#     .data$region_short,
#     .data$region_number,
#     .data$year,
#     .data$total,
#     .data$sd
#   ) %>%
#   dplyr::distinct(.keep_all = TRUE)


# Placeholder to troubleshoot figures
# TODO: Replace with empirical data

abundance_ak <- tibble::tibble(
  region_name = "Alaska",
  region_short = "AK",
  region_number = 1,
  year = 1979:2018,
  total = 100000000,
  sd = 10000000
)

abundance_bc <- tibble::tibble(
  region_name = "British Columbia",
  region_short = "BC",
  region_number = 2,
  year = 1979:2018,
  total = 10000000,
  sd = 1000000
)

abundance_cc <- tibble::tibble(
  region_name = "California Current",
  region_short = "CC",
  region_number = 3,
  year = 1979:2018,
  total = 100000000,
  sd = 10000000
)

abundance <- dplyr::bind_rows(
  abundance_ak,
  abundance_bc,
  abundance_cc
)


# Write to data/
write_data(abundance)
