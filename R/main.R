

#' Produce unrelated medical spending
#'
#'
#' @param country Current options are
#'   c("Netherlands", "Germany", "Greece", "Spain", "United Kingdom")
#' @param providers By default all providers within a country are taken. Review
#'   available providers by country `paid4::available.providers`. For example,
#'   setting this to c("HC","LTC") will only show the sum of costs from
#'   inpatient hospital and long-term care spending.
#' @param discount_perc The percentage for discounting costs (in percent). The
#'   default is 3.
#' @param related.diseases an integer vector or `NULL`. Applicable for the
#'   Netherlands and Germany only. The default, `NULL`, marks all diseases as
#'   unrelated. To mark a disease as related and exclude its costs, provide here
#'   its corresponding "Group in PAID" from `paid4::mapping`. Example:
#'   `related.diseases = c(16,25)` for the Netherlands would mark "breast
#'   cancer" and "other cancers" as related diseases and exclude their costs.
#' @param cost.method `NULL` (default) or a character vector. Defines how costs
#'   of related diseases are excluded - from cost of illness (option "dscosts")
#'   or from own data "totcosts". The default for Netherlands and Germany is
#'   dscosts, totcosts for the rest. Review options
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
#'   two columns. The first column gives overall survival of the intervention
#'   group, and the second - of the control.
#' @param pmen         Only relevant if `survdata` is provided. Proportion of
#'   men in the cohort. Default is 0.5.
#' @param cycle_length Only relevant if `survdata` is provided. A numeric value
#'   indicating the cycle length in years. The default is 1, meaning 1 year. For
#'   example, a cycle length of 3 weeks would be 3/52 = 0.05769231.
#' @param start_age    Only relevant if `survdata` is provided. Age at the start
#'   of the simulation. Default is 0.
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
#'   3) dc: non-discounted decedent costs (TTD<=4) where TTD=0 means
#'   death at the same age as start.
#'
#'   If `survdata` is provided, the function returns a list of
#'   1) output:  Survival * discounted, half-cycle corrected, sex-weighted unrelated costs;
#'   2) wcosts:  Discounted, half-cycle corrected, sex-weighted unrelated costs;
#'   3) uwcosts: Discounted, half-cycle corrected unrelated costs for men &
#'   women separately in the pre-specified cycle length.
#'
#' @importFrom mgcv gam
#' @importFrom mgcv s
#' @export
paid <- function(country = c("Netherlands","Germany","Greece","Spain","United Kingdom"),
                 providers = "ALL", discount_perc = 3, related.diseases = NULL, cost.method = NULL, related.costs = NULL, survdata = NULL,
                 pmen = 0.5,
                 cycle_length = 1,
                 start_age = 0
                 ) {


#### Set-up defaults & based on user options ####

  # Country
  country         <- match.arg(country)

  # Providers
  if (toupper(providers) == "ALL") {
    user.providers <- available.providers[[country]]
  } else {
    user.providers <- match.arg(toupper(providers), toupper(available.providers[[country]]), several.ok = TRUE)
    user.providers[user.providers %in% "OTHER"] <- "Other"
  }

  # Related disease correction (if any) - COI & no user.uploaded related costs
  cost.method   <- match.arg(cost.method, available.costmethods[[country]]) # Will error out if user chooses dscosts for a non-COI country, which is the intention.
                                                                            # Returns "dscosts"  if cost.method=NULL & COI country
                                                                            # Returns "totcosts" if cost.method=NULL & non-COI country
                                                                            # Unless specified the default is from COI for NL & DE
  all.diseases  <- mapping[[country]][,c(2,4)]
  names(all.diseases) <- c("Group", "Description")
  all.diseases  <- all.diseases[!(all.diseases$Group %in% c("Header", "Total")),]
  all.diseases$bool <- TRUE
  if (!is.null(related.diseases)) {
    all.diseases$bool[all.diseases$Group %in% related.diseases] <- FALSE
    }
  all.diseases <- switch(cost.method,
                         "dscosts"  = all.diseases,
                         "totcosts" = data.frame(Group = 1:5, bool = TRUE))

  if (!is.null(related.costs) & !(class(related.costs) %in% c("lits","numeric"))) {
    error.msg <- "related.costs must either be class NULL, list or numeric. Fill in one of the available templates under paid4::related.costs.scdc or paid4::related.costs.ac"
    return(print(error.msg))
  }

#### Generate LHCE ####
  list.costs <- renderLHCE(country = country,
                           user.providers = user.providers,
                           user.diseases = all.diseases,
                           disc_percentage = discount_perc,
                           user.uploaded = related.costs)

#### Adjust to cycle length and uploaded survival ####
  if (exists("survdata") & !is.null(survdata)) {
    if (!inherits(survdata,"data.frame")) {
      error.msg <- "'survdata' must be class data.frame with two columns: survival of Intervention group in the first, and survival of Comparator group in the second."
     return(print(error.msg))
    }
    data <- renderCohort(survdata, costlist = list.costs[["lhce"]],
                         pmen = pmen, cycle_length = cycle_length, start_age = start_age )
    return(data)
  } else {
    return(list.costs)
  }

}
