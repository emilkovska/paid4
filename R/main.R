

#' Produce unrelated medical spending
#'
#' Cost-effectiveness analysis (CEA) is used in many countries to evaluate
#' whether new drugs and technologies provide sufficient value for money as
#' compared to existing ones. In CEA, costs and benefits are estimated over a
#' patient’s lifetime in various scenarios. While lifetime medical spending
#' associated with the treated disease(s) is routinely included in CEA, the
#' inclusion of spending related to other diseases in life years gained is less
#' common practice (e.g. costs of treating dementia in life-years gained after a
#' successful heart transplant). These costs are referred to as future unrelated
#' medical costs or indirect medical costs (IMC). The current function produces
#' estimates of IMC for different sets of healthcare providers and comprising
#' diseases.
#'
#' @param country Current options are "Netherlands", "Germany","France",
#'   "Greece", "Spain", and "United Kingdom".
#' @param providers By default all providers within a country are taken. Review
#'   available providers by country `paid4::available.providers`. For example,
#'   setting this to c("HC","LTC") will only show the sum of costs from
#'   inpatient hospital and long-term care spending.
#' @param discount_perc either a single numerical value (in percent, e.g. 3) or
#'   a numerical named vector (e.g., c('1' = 2.5, '30' = 1.5)). This will be the
#'   percentage used to discount lifetime healthcare costs. The default is
#'   assigned as per the country-specific HTA guidelines.
#' @param related.diseases an integer vector or `NULL`. Applicable for the
#'   Netherlands, Germany, and France only. The default, `NULL`, marks all
#'   diseases as unrelated. To mark a disease as related and exclude its costs,
#'   provide here its corresponding "Group in PAID" number from
#'   `paid4::mapping`. Example: `related.diseases = c(14,25)` for the
#'   Netherlands would mark "breast cancer" and "other cancers" as related
#'   diseases and exclude their costs.
#' @param cost.method `NULL` (default) or a character vector. Defines how costs
#'   of related diseases are excluded - from cost of illness (option "dscosts")
#'   or from own data "totcosts". The default for Netherlands, Germany, and
#'   France is "dscosts", "totcosts" for the rest. Review options
#'   `paid4::available.costmethods`.
#' @param related.costs `NULL` (default), numeric vector (see
#'   `paid4::related.costs.ac`) or list (see `paid4::related.costs.scdc`). This
#'   excludes any (additional) user-provided related spending by age, as needed.
#'   This can be relevant for countries without cost of illness studies, for
#'   example, to exclude per-capita costs of cancer from the total per-capita
#'   survivor and decedent costs. Fill in the provided templates under
#'   `related.costs.ac` or `related.costs.scdc` and pass it on as an argument
#'   here retaining the same structure.
#' @param survdata     By default `NULL`. Else takes a data.frame or matrix with
#'   two columns. The first column must give overall survival of the
#'   intervention group, and the second - of the control.
#' @param pmen Only relevant if `survdata` is provided. Proportion of men in the
#'   cohort. Default is 0.5.
#' @param cycle_length Only relevant if `survdata` is provided. A numeric value
#'   indicating the cycle length in years. The default is 1, meaning 1 year. For
#'   example, a cycle length of 3 weeks would be 3/52 = 0.05769231.
#' @param start_age    Only relevant if `survdata` is provided. Age at the start
#'   of the simulation. Default is 0.
#' @param coi.year numerical value, denoting which Cost of Illness study year is
#'   to be used for calculating lifetime healthcare spending. The default
#'   differs by country, but it is always the latest COI year. Currently, the
#'   default for the Netherlands is 2019, Germany - 2020, and France - 2019.
#' @param PSA logical. If `TRUE` ratios will be drawn 1000 times and the resulting
#'   lifetime medical spending will be presented as a distribution. 
#' @param psa.N integer. Defines how many draws in case of PSA. Default is 1000.
#' @param psa.seed integer. If PSA, by default, a random seed is generated and 
#'   reported back in the end results. User can also provide their own seed here.
#'
#' @return If no `survdata` is provided, the function returns a list containing:
#'   1) lhce: full age matrices of discounted but not half-cycle corrected
#'   lifetime unrelated HCE, for men and women separately. The result comes back
#'   as a named array of size `[202,101,3]`. The first 101 rows are for men, and
#'   102:202 are for women. Rows denote start age, while columns are the age at
#'   death. The third dimension of the array is also named and returns either
#'   the mean, lower of upper range of the estimate. For example, sub-setting
#'   the `lhce` array as `[1,,]` will return a 101x3 matrix of lifetime medical
#'   spending since age 0 for men with its mean, lower and upper range. 
#'   2) sc: non-discounted survivor per-capita costs by age (TTD>4). 
#'   3) dc: non-discounted decedent costs (TTD<=4) where TTD=0 means death at 
#'   the same age as start. 
#'   4) Source year used for costs and presenting prices.
#'
#'   If `survdata` is provided, the function returns a list of 
#'   1) output: Survival times discounted, half-cycle corrected, sex-weighted 
#'      unrelated costs;
#'   2) wcosts:  Discounted, half-cycle corrected, sex-weighted unrelated costs;
#'   3) uwcosts: Discounted, half-cycle corrected unrelated costs for men &
#'   women separately in the pre-specified cycle length. 
#'   4) Source year used for costs and presenting prices.
#'
#' @importFrom mgcv gam
#' @importFrom mgcv s
#' @export
paid <- function(country = c("Netherlands","Germany","France","Greece","Spain","United Kingdom"),
                 providers = "ALL", discount_perc = NULL, related.diseases = NULL, 
                 cost.method = NULL, related.costs = NULL, survdata = NULL,
                 pmen = 0.5, cycle_length = 1, start_age = 0, coi.year = NULL, PSA = FALSE, psa.N = NULL, psa.seed = NULL) {

#### Set-up defaults and on user options ####

  ##### Validate Country choice ####
  country <- match.arg(country)
  
  ##### Validate COI year choice and set defaults ####
  options <- list(
    Netherlands = c("2019", "2017", "2015"),
    Germany     = c("2020", "2015"),
    France      = "2019",
    Greece      = "2014",
    Spain       = "2008",
    `United Kingdom` = "2011"
  )
  
  defaults <- c(
    Netherlands = "2019",
    Germany     = "2020",
    France      = "2019",
    Greece      = "2014",
    Spain       = "2008",
    `United Kingdom` = "2011"
  )
  
  # assign default if missing
  if (is.null(coi.year)) {
    coi.year <- defaults[[country]]
  }
  
  # validate - for use across functions
  coi.year <- match.arg(as.character(coi.year), options[[country]])
  
  # show end user info
  coi.year.out <- rbind(coi.year, ifelse(country %in% c("Netherlands","Germany","France"),coi.year,"2020"))
  rownames(coi.year.out) <- c("Source spending from:", "Prices indexed to:")
  
  ##### Validate discount percentage choice and set defaults ####
  defaults <- list(
    "Netherlands" = 3,
    "Germany"     = 3,
    "France"      = c("1" = 2.5, "30" = 1.5),
    "Greece"      = 3.5,
    "Spain"       = 3.5
  )
  
  # assign default if missing
  if (is.null(discount_perc)) {
    discount_perc <- defaults[[country]]
  }
  
  ##### Validate Provider choice ####
  if (any(toupper(providers) == "ALL")) {
    user.providers <- available.providers[[country]]
  } else {
    user.providers <- match.arg(toupper(providers), toupper(available.providers[[country]]), several.ok = TRUE)
    user.providers[user.providers %in% "OTHER"] <- "Other"
  }

  ##### Validate cost.method choice ####
  # Related disease correction (if any) - COI & no user.uploaded related costs
  cost.method   <- match.arg(cost.method, available.costmethods[[country]]) # Will error out if user chooses dscosts for a non-COI country, which is the intention.
                                                                            # Returns "dscosts"  if cost.method=NULL & COI country
                                                                            # Returns "totcosts" if cost.method=NULL & non-COI country
                                                                            # Unless specified the default is from COI for NL, DE & FR
  
  ##### Validate related.diseases choice ####
  all.diseases        <- mapping[[country]][[paste(coi.year)]][,c(2,4)]
  names(all.diseases) <- c("Group", "Description")
  all.diseases        <- all.diseases[!(all.diseases$Group %in% c("Header", "Total")),]
  all.diseases$bool   <- TRUE
  if (!is.null(related.diseases)) {
    all.diseases$bool[all.diseases$Group %in% related.diseases] <- FALSE
    }
  all.diseases <- switch(cost.method,
                         "dscosts"  = all.diseases,
                         "totcosts" = data.frame(Group = 1:5, bool = TRUE))

  # Validate user-provided related costs
  if (!is.null(related.costs) & !(class(related.costs) %in% c("list","numeric"))) {
    error.msg <- "related.costs must either be NULL or class list/numeric. Fill in one of the available templates under paid4::related.costs.scdc or paid4::related.costs.ac"
    stop(error.msg)
  }
  
  # Validate PSA options
  if (PSA & is.null(psa.N)) {
    psa.N <- 1000
  } else if (PSA & !is.null(psa.N)) {
    psa.N <- round(psa.N)
  }
  
  if (is.null(psa.seed)) {psa.seed <- round(runif(1)*10^9)}

#### Generate LHCE ####
  if (PSA) {set.seed(psa.seed)}
  list.costs <- renderLHCE(country         = country,
                           user.providers  = user.providers,
                           user.diseases   = all.diseases,
                           disc_percentage = discount_perc,
                           user.uploaded   = related.costs,
                           coi.year        = coi.year,
                           PSA             = PSA,
                           psa.N           = psa.N)

#### Adjust to cycle length and uploaded survival ####
  # Validate user-provided survival data
  if (exists("survdata") & !is.null(survdata)) {
    if (!inherits(survdata,"data.frame")) {
      error.msg <- "'survdata' must be class data.frame with two columns: survival of Intervention group in the first, and survival of Comparator group in the second."
      stop(error.msg)
    }
    data           <- renderCohort(survdata, costlist = list.costs[["lhce"]],
                                   pmen = pmen, cycle_length = cycle_length, start_age = start_age,
                                   PSA = PSA, psa.N = psa.N)
    data[[4]]      <- coi.year.out
    names(data)[4] <- "Output info"
    res.out        <- data 
  } else {
    list.costs[[4]]      <- coi.year.out
    names(list.costs)[4] <- "Output info"
    res.out              <- list.costs
  }
  
  res.out[[4]]      <- coi.year.out
  names(res.out)[4] <- "Output info"
  
  if (PSA) {
    res.out[[5]]      <- psa.seed
    names(res.out)[5] <- "Seed for PSA"
  }
  
  gc()
  
  return(res.out)

}

