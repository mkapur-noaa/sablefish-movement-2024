#' Sablefish Movement Model Fit Parameter Summaries
#'
#' @name fits
#' @keywords datasets
NULL

#' @rdname fits
"fit_region_average_pooled"

#' @rdname fits
"fit_region_average_length"

#' @rdname fits
"fit_region_season_pooled"

#' @rdname fits
"fit_region_season_length"

#' @rdname fits
"fit_region_year_pooled"

#' @rdname fits
"fit_region_year_length"

#' @rdname fits
"fit_omregion_average_pooled"

#' @rdname fits
"fit_omregion_average_length"

#' @rdname fits
"fit_subregion_average_pooled"

#' @rdname fits
"fit_subregion_average_length"

#' @rdname fits
"fit_subregion_season_pooled"

#' @rdname fits
"fit_subregion_season_length"


#' @rdname fits
"fit_subregion_year_pooled"

#' @rdname fits
"fit_subregion_year_length"

#' @rdname fits
"fit_region_average_pooled_h_prior_sd_001"

#' @rdname fits
"fit_region_average_pooled_h_prior_sd_003"

#' @rdname fits
"fit_region_average_pooled_h_prior_sd_030"

#' @rdname fits
"fit_region_average_pooled_h_prior_sd_100"

#' @rdname fits
"fit_region_average_pooled_w_decr_ak_33_pct"

#' @rdname fits
"fit_region_average_pooled_w_decr_bc_33_pct"

#' @rdname fits
"fit_region_average_pooled_w_decr_cc_33_pct"

#' @rdname fits
"fit_region_average_pooled_w_incr_ak_50_pct"

#' @rdname fits
"fit_region_average_pooled_w_incr_bc_50_pct"

#' @rdname fits
"fit_region_average_pooled_w_incr_cc_50_pct"


#' Sablefish Harvest Rates
#'
#' A dataset containing annual sablefish harvest rates for spatial strata
#' in the northeast Pacific Ocean.
#'
#' @format [data.frame()] [tibble()] with 760 rows and 6 variables:
#' \describe{
#'   \item{spatial}{spatial scale, one of \code{region},
#'     \code{subregion}, or \code{omregion}}
#'   \item{name}{spatial strata name}
#'   \item{short}{short version of spatial strata name}
#'   \item{number}{spatial strata number}
#'   \item{year}{year}
#'   \item{harvest_rate}{sablefish harvest rate}
#' }
#'
#' @source Harvest rate data come from:
#' \describe{
#'   \item{regions}{
#'     \itemize{
#'       \item{Alaska: sablefish stock assessment annual fishing mortality
#'         rate F, converted to a harvest rate by \code{H = 1 - exp(-F)}}
#'       \item{British Columbia: sablefish stock assessment annual legal
#'         harvest rate}
#'       \item{California Current: sablefish stock assessment annual fishing
#'         mortality rate F, converted to a harvest rate by
#'         \code{H = 1 - exp(-F)}}
#'     }
#'   }
#'   \item{subregions}{
#'     \itemize{
#'       \item{Southeast Alaska: Chatham Strait sablefish statistical
#'         catch-at-age model annual fishing mortality rate F, converted to a
#'         harvest rate by \code{H = 1 - exp(-F)}}
#'       \item{Other Alaska subregions: sablefish stock assessment annual catch
#'         (including discards) by subregion divided by annual age 4+ biomass
#'         by subregion}
#'       \item{British Columbia subregions: same as British Columbia}
#'       \item{California Current subregions: same as California Current}
#'     }
#'   }
#'   \item{omregions}{
#'     \itemize{
#'       \item{Western Alaska: same as Alaska}
#'       \item{Eastern Alaska: same as Southeast Alaska}
#'       \item{British Columbia operating model strata:
#'         same as British Columbia}
#'       \item{California Current operating model strata: same as
#'         California Current}
#'     }
#'   }
#' }
#'
#' @source see https://github.com/luke-a-rogers/sablefish-data
#'
"harvest_rates"

#' Sablefish Numbers At Length
#'
#' @source see https://github.com/luke-a-rogers/sablefish-data
#'
"numbers_at_length"

#' Sablefish Tags Recovered
#'
#' A dataset containing sablefish tags recovered by date, released length,
#' and spatial stratum in the northeast Pacific Ocean.
#'
#' \describe{
#'   \item{tag_id}{}
#'   \item{released_date}{}
#'   \item{released_length}{}
#'   \item{released_region}{}
#'   \item{released_subregion}{}
#'   \item{released_omregion}{}
#'   \item{recovered_date}{}
#'   \item{recovered_region}{}
#'   \item{recovered_subregion}{}
#'   \item{recovered_omregion}{}
#'   \item{source}{}
#' }
#'
#' @source see https://github.com/luke-a-rogers/sablefish-data
#'
"tags_recovered"

#' Sablefish Tags Released
#'
#' A dataset containing sablefish tags released by date, released length,
#' and spatial stratum in the northeast Pacific Ocean.
#'
#' \describe{
#'   \item{tag_id}{}
#'   \item{released_date}{}
#'   \item{released_length}{}
#'   \item{released_region}{}
#'   \item{released_subregion}{}
#'   \item{released_omregion}{}
#'   \item{source}{}
#' }
#'
#' @source see https://github.com/luke-a-rogers/sablefish-data
#'
"tags_released"
