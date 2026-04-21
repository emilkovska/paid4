#' A list of available related cost correction methods by country
#'
#' If a country conducts a comprehensive cost-of-illness study and it has been
#' included in PAID, then the default cost method is "dscosts", else "totcosts".
#' The data shows which method is available per country. This dataset shows the
#' possible options by country for the `cost.method` option under the `paid`
#' function.
#'
#' @source PAID 4.0 tool
#' @format list
"available.costmethods"

#' A list of available providers by country
#'
#' Each country has its own set of healthcare providers that comprise per-capita
#' healthcare spending. This file gives a list of available providers by country
#' to guide what can be placed in the `providers` option of the `paid` function.
#'
#' @source PAID 4.0 tool
#' @format list
"available.providers"

#' A list of diseases that can be excluded from the estimates
#'
#' Only important for countries that have a cost of illness study - Netherlands,
#' Germany, and France. `paid4::mapping[[country]][[coi.year]]` displays a data frame
#' listing all diseases that comprise the cost estimates and show what their
#' corresponding groups are. To be used as guidance for `related.diseases`
#' option of the `paid` function.
#'
#' @source PAID 4.0 tool
#' @format list of data frames for each country
"mapping"

#' A template for users to provide own related costs - no end-of-life correction
#'
#' This file serves as a template for users to fill in (if needed) and pass
#' onto `related.costs` option of the `paid` function. This template should be
#' used if the related costs you wish to upload are simple per-capita averages
#' and do not differentiate between last year of life spending and spending in
#' other years. The first 101 elements should contain per-capita related
#' spending that needs to be excluded from PAID for men for ages 0 to 100.
#' Elements 102:202 should contain the same for women.
#'
#' @source PAID 4.0 tool
#' @format a numeric vector of length 202
"related.costs.ac"

#' A template for users to provide own related costs - with end-of-life
#' correction
#'
#' This file serves as a template for users to fill in (if needed) and pass onto
#' `related.costs` option of the `paid` function. This template should be used
#' if the related costs you wish to upload have separate estimates for the last
#' year of life ("dc") and other years ("sc"). The first 101 elements within
#' each list should contain per-capita related spending that needs to be
#' excluded from PAID for men for ages 0 to 100. Elements 102:202 should contain
#' the same for women.
#'
#' @source PAID 4.0 tool
#' @format list of length 2. The two objects are called "sc" and "dc" and
#'   contain a numeric vector of length 202.
"related.costs.scdc"
