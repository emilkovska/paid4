## code to prepare `DATASET` dataset goes here
source("data-raw/data-raw-helper.R")
library(dplyr)

all.country <- c("Netherlands", "Germany", "France" ,"Greece","Spain","United Kingdom")
coi.country <- c("Netherlands", "Germany","France")
available.providers  <- list("Netherlands" = c("Hospitals" = "HC",
                                               "Nursing and residential care facilities"  = "LTC",
                                               "Retail sale and other providers of medical goods"  = "MED",
                                               "Providers of ambulatory health care"  = "GP",
                                               "Other health care providers"  = "Other"),
                             "Germany"     = c("Hospitals" = "HC",
                                               "Residential care and prevention/rehabilitation facilities"  = "LTC",
                                               "Retail sale of medical goods"  = "MED",
                                               "Providers of ambulatory health care"  = "GP",
                                               "Other health care providers"  = "Other"),
                             "France"      = c("Hospitals" = "HC",
                                               "Retail sale and other providers of medical goods"  = "MED",
                                               "Providers of ambulatory health care"  = "GP",
                                               "Cash benefits"  = "Other"),
                             "Greece"      = c("Inpatient hospital" = "HC"),
                             "Spain"       = c("Inpatient, specialized outpatient, prescription drugs, transport, and other healthcare categories" = "ALL"),
                             "United Kingdom" = c("Inpatient and outpatient care, and GP and pharmaceutical spending" = "ALL"))

available.costmethods <- list("Netherlands"  = c("From cost of illness" = "dscosts",
                                                 "From own data"        = "totcosts"),
                              "Germany"      = c("From cost of illness" = "dscosts",
                                                 "From own data"        = "totcosts"),
                              "France"       = c("From cost of illness" = "dscosts",
                                                 "From own data"        = "totcosts"),
                              "Greece"       = c("From own data"        = "totcosts"),
                              "Spain"        = c("From own data"        = "totcosts"),
                              "United Kingdom" = c("From own data"      = "totcosts")
)

coi.options <- list(
  Netherlands = c("2019", "2017", "2015"),
  Germany     = c("2020", "2015"),
  France      = "2019",
  Greece      = "2014",
  Spain       = "2008",
  `United Kingdom` = "2011"
)

coi.defaults <- c(Netherlands = "2019",
                  Germany     = "2020",
                  France      = "2019",
                  Greece      = "2014",
                  Spain       = "2008",
                  `United Kingdom` = "2011")


##### GENERATE INITIAL MAPPING DIAG FILE ####

# Every country must have a mapping > diag file even if empty, as it allows for easier update is the country gains any COI
# The NL mapping for diagnoses can be dynamically updated depending on COI year, but an initial file is needed 
# Mapping Excel files will be deleted prior to package launch, but will remain available through the Shiny App Repo in GitHub
mapping <- vector("list",length(all.country))
names(mapping) <- all.country
for (cntry in all.country) {
  mapping[[cntry]]        <- vector("list",length = length(coi.options[[cntry]]))
  names(mapping[[cntry]]) <- paste(coi.options[[cntry]])
}

# For the NL diag mapping varies by COI year, for Germany, they are exactly the same (providers info also)
for (ctry in all.country) {
  foruse <- sub(" ","",ctry)
  mapping[[ctry]][[coi.defaults[[ctry]]]] <- readxl::read_excel(paste("data-raw/",sub(" ","",foruse),"_mapping.xlsx",sep=""),
                                             sheet = "diag",.name_repair = "unique_quiet")
}
mapping[["Germany"]][["2015"]] <- readxl::read_excel(paste("data-raw/Germany_mapping.xlsx",sep=""),
                                                     sheet = "diag",.name_repair = "unique_quiet")
usethis::use_data(mapping, overwrite = TRUE)
rm(mapping)
##### GENERATE TC files ######

# To add newer COI studies: 
# 1) Add the desired year under the year argument in function callKvZ()
# 2) Update the coi list within callKvZ() accordingly. 
    # The API info is taken from https://www.vzinfo.nl/kosten-van-ziekten > click on the "Maak uw eigen tabellen en grafieken YYYY" 
    # Go to the Download button above the table > "Meer opties via Dataportaal" 
    # Copy and paste everything after clicking on "API (voor Apps)" and surround it by single quotes to make text ---> ''
# 3) The callKvZ has a built-in functionality to update the mapping for the NL after/if one pulls a new COI year. 

# Get Total Costs (not yet per-capita)
tc_NL_2019 <- cleanKvZ(data = callKvZ(year = "2019"))
tc_NL_2017 <- cleanKvZ(data = callKvZ(year = "2017"))
tc_NL_2015 <- cleanKvZ(data = callKvZ(year = "2015"))
tc_DE_2020 <- cleanCOIDE(coi.DE = 2020)
tc_DE_2015 <- cleanCOIDE(coi.DE = 2015)
tc_FR      <- cleanCOIFR()
tc         <- list("Netherlands" = list("2019" = tc_NL_2019,
                                        "2017" = tc_NL_2017,
                                        "2015" = tc_NL_2015), 
                   "Germany" = list("2020" = tc_DE_2020,
                                    "2015" = tc_DE_2015),
                   "France"  = list("2019" = tc_FR))
rm(list = c("tc_DE_2020","tc_DE_2015","tc_FR","tc_NL_2019","tc_NL_2017","tc_NL_2015"))
devtools::load_all()
##### MORTALITY  ######

# Raw mortality data is obtained from the Human Mortality Database - mortality.org
# The HMD does not have an API service set up, and is accessible only thought a free account.
# To get new raw files, get Age-Specific Death Rates 1x1.
# Instead, the cleanMortRate() function will clean up the files as needed and combine them to be used internally. 
probDeath <- mort  <- vector(mode = "list", length = length(all.country))
names(mort) <- names(probDeath) <- all.country
for (i in all.country) {
  mort[[i]] <- cleanMortRate(country = i)
  mort <<- mort
  probDeath[[i]] <- getProbDeath(i)
}

# Calculate p(x,d=1|a,s) - Onlt for NL & DE (COI countries)

probDeathCause <- probDeathChapters <- vector("list", length(coi.country))
names(probDeathCause) <- names(probDeathChapters) <- coi.country

# Causes of death, 2019 https://www-genesis.destatis.de/ 
# Code: 23211-0002 Deaths: Germany, years, causes of death, sex, age groups.
# Mapping for COD to Disease codes was done manually.

x <- getDeathCauseNL()
probDeathCause[["Netherlands"]]    <- x[["probDeathCause"]]
probDeathChapters[["Netherlands"]] <- x[["probDeathChapters"]]

x <- getDeathCauseDE()
probDeathCause[["Germany"]]    <- x[["probDeathCause"]]
probDeathChapters[["Germany"]] <- x[["probDeathChapters"]]

x <- getDeathCauseFR()
probDeathCause[["France"]]    <- x[["probDeathCause"]]
probDeathChapters[["France"]] <- x[["probDeathChapters"]]
rm(x)

##### POPULATION  ######
pop            <- cleanPopCounts()

##### RATIOS  ######
RatiosDTA <- haven::read_dta("data-raw/Ratios.dta")
ratios    <- cleanRatios()

##### NOT RATIOS - for COI studies only (DE & NL & FR)  ######

###### N.B.! Once the new COI comes out with COVID, update mapping & rerun this with also chapter 22 ###### 
notratios <- vector("list",length(coi.country))
names(notratios) <- coi.country
for (cntry in coi.country) {
  notratios[[cntry]]        <- vector("list",length = length(coi.options[[cntry]]))
  names(notratios[[cntry]]) <- paste(coi.options[[cntry]])
}


notratios[["Netherlands"]][["2019"]] <- cleanRatiosNOTChapter("Netherlands", coi.year = 2019)
notratios[["Netherlands"]][["2017"]] <- cleanRatiosNOTChapter("Netherlands", coi.year = 2017)
notratios[["Netherlands"]][["2015"]] <- cleanRatiosNOTChapter("Netherlands", coi.year = 2015)
notratios[["Germany"]][["2020"]]     <- cleanRatiosNOTChapter("Germany"    , coi.year = 2020)
notratios[["Germany"]][["2015"]]     <- cleanRatiosNOTChapter("Germany"    , coi.year = 2015)
notratios[["France"]][["2019"]]      <- cleanRatiosNOTChapter("France"     , coi.year = 2019)

##### GENERATE AC files #########

group.pop.age  <- list("Netherlands" =  c(0,1,seq(5,101,by = 5)),
                       "Germany"     =  c(0,15,30,45,65,85,101),
                       "France"      =  c(0,15,35,55,65,75,101))

ac <- vector("list",length(all.country))
names(ac) <- all.country
for (cntry in all.country) {
  ac[[cntry]]        <- vector("list",length = length(coi.options[[cntry]]))
  names(ac[[cntry]]) <- paste(coi.options[[cntry]])
}

for (cntry in coi.country) {
  for (y in coi.options[[cntry]]) {
      ac[[cntry]][[y]] <- getAC(country = cntry, 
                                cut.pop.age = group.pop.age[[cntry]], 
                                providers = available.providers[[cntry]],
                                coi.year  = y)
  }
}


# The ones from Hamraz'  paper where I have their dc/sc but for TTD=0 only. Need to convert them to AC -> DC(ttd) & SC.
for (cntry in c("Greece","Spain","United Kingdom")) {

  foruse <- sub(" ","",cntry)
  # Calculate AC
  data <- readxl::read_excel("data-raw/Mokriestimates.xlsx", sheet = foruse,.name_repair = "unique_quiet")
  mx   <- mort[[cntry]]
  qx   <- 1-exp(-mx$mort)
  AC   <- as.data.frame(data$sc * (1-qx) + data$dc*qx)
  names(AC) <- "Total"

  # Smooth out ac results & save
  nk             <<- 12
  v.age          <<- 0:100
  AC$Total       <- f.smoothAC(AC$Total, inc=0, lastsexage = 101)
  ac[[cntry]][[1]]    <- list(as.matrix(AC))
  names(ac[[cntry]][[1]]) <- available.providers[[cntry]]
}
rm(AC)
rm(mx)
rm(data)

##### GENERATE SC DC files #####
scdc <- vector("list",length(all.country))
names(scdc) <- all.country
for (cntry in all.country) {
  scdc[[cntry]]        <- vector("list",length = length(coi.options[[cntry]]))
  names(scdc[[cntry]]) <- paste(coi.options[[cntry]])
}

for (cntry in all.country) {
  for (y in coi.options[[cntry]]) {
    scdc[[cntry]][[y]] <- getSCDC(cntry,providers = available.providers[[cntry]], coi.year = y)
  }
}


# Make some available to users
related.costs.scdc <- list(sc = rep(0,202),dc = rep(0,202))
names(related.costs.scdc$sc) <- names(related.costs.scdc$dc) <- paste(rep(c("Men","Women"), each=101),0:100,sep = "_")
related.costs.ac <- related.costs.scdc$sc

usethis::use_data(mort, probDeath, probDeathCause, probDeathChapters, 
                  notratios, ac, scdc,
                  internal = TRUE, overwrite = TRUE)

usethis::use_data(mapping, 
                  available.costmethods, 
                  available.providers, 
                  related.costs.scdc, 
                  related.costs.ac, overwrite = TRUE)

devtools::load_all()