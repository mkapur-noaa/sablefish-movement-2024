library(targets)

# Set options
options(tidyverse.quiet = TRUE)
tar_option_set(packages = "sablefishmovement", imports = "sablefishmovement")

# List target objects
list(
  # Watch data
  list(
    tar_target(
      watch_harvest_rates,
      "data/harvest_rates.rda",
      format = "file"
    ),
    tar_target(
      watch_numbers_at_length,
      "data/numbers_at_length.rda",
      format = "file"
    ),
    tar_target(
      watch_tags_recovered,
      "data/tags_recovered.rda",
      format = "file"
    ),
    tar_target(
      watch_tags_released,
      "data/tags_released.rda",
      format = "file"
    )
  ),
  # Read data
  list(
    tar_target(harvest_rates, read_from_path(watch_harvest_rates)),
    tar_target(numbers_at_length, read_from_path(watch_numbers_at_length)),
    tar_target(tags_recovered, read_from_path(watch_tags_recovered)),
    tar_target(tags_released, read_from_path(watch_tags_released))
  )
)
