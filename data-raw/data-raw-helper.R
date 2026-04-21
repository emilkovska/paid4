#' Text to JSON query
#' 
#'  Helper function to clean API KvZ or Cause of death files. 
#' 
#' @param x a string vector of Keys which to filter for
#' @param varname a string of the variable name which to filter on
#' @param and logical, if TRUE the produced string ends with "and"
#'
#' @return a string in the URL query format to truncate JSON API calls. 
toFilter  <- function(x, varname, and = TRUE) {
  n   <- length(x)
  
  if (n>1) {
    l_x <- list()
    for (i in 1:n) {
      l_x[[i]] <- paste(varname," eq '",x[i],"'", sep="")
    }
    res <- paste(l_x,collapse = " or ")
    
    if (and) {
      return(paste("(",res,") and ", sep=""))
    } else {
      return(paste("(",res,")", sep=""))
    }
    
    
  } else {
    
    if (and) {
      res <- paste(varname," eq '",x,"' and ",sep="")
    } else {
      res <- paste(varname," eq '",x,"'",sep="")
    }
    
    return(res)
    
  }
  
}

#' API call for Cost of Illness data
#' 
#'  It calls the COI data for the Netherlands and updates the mapping for the NL automatically. 
#'
#' @param year a string. There are three possible cost of illness years to pull from: `"2019"`, `"2017"`, or `"2015"`
#'
#' @return a data frame
callKvZ    <- function(year = c(2019,2017,2015)) {
  
  year <- match.arg(year)
  
  
  coi  <- list(
    "2019" = '{
  "odata.metadata":"https://dataderden.cbs.nl/ODataApi/OData/50091NED/$metadata","value":[
    {
      "name":"TableInfos","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/TableInfos"
    },{
      "name":"UntypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/UntypedDataSet"
    },{
      "name":"TypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/TypedDataSet"
    },{
      "name":"DataProperties","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/DataProperties"
    },{
      "name":"CategoryGroups","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/CategoryGroups"
    },{
      "name":"Geslacht","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Geslacht"
    },{
      "name":"Leeftijd","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Leeftijd"
    },{
      "name":"Diagnose","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Diagnose"
    },{
      "name":"Zorgsector","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Zorgsector"
    },{
      "name":"Zorgfunctie","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Zorgfunctie"
    },{
      "name":"Financieringsvorm","url":"https://dataderden.cbs.nl/ODataApi/odata/50091NED/Financieringsvorm"
    }
  ]
}',
    "2017" = '{
  "odata.metadata":"https://dataderden.cbs.nl/ODataApi/OData/50050NED/$metadata","value":[
    {
      "name":"TableInfos","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/TableInfos"
    },{
      "name":"UntypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/UntypedDataSet"
    },{
      "name":"TypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/TypedDataSet"
    },{
      "name":"DataProperties","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/DataProperties"
    },{
      "name":"CategoryGroups","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/CategoryGroups"
    },{
      "name":"Geslacht","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Geslacht"
    },{
      "name":"Leeftijd","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Leeftijd"
    },{
      "name":"Diagnose","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Diagnose"
    },{
      "name":"Zorgsector","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Zorgsector"
    },{
      "name":"Zorgfunctie","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Zorgfunctie"
    },{
      "name":"Financieringsvorm","url":"https://dataderden.cbs.nl/ODataApi/odata/50050NED/Financieringsvorm"
    }
  ]
}',
    "2015" = '{
  "odata.metadata":"https://dataderden.cbs.nl/ODataApi/OData/50040NED/$metadata","value":[
    {
      "name":"TableInfos","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/TableInfos"
    },{
      "name":"UntypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/UntypedDataSet"
    },{
      "name":"TypedDataSet","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/TypedDataSet"
    },{
      "name":"DataProperties","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/DataProperties"
    },{
      "name":"CategoryGroups","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/CategoryGroups"
    },{
      "name":"Geslacht","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Geslacht"
    },{
      "name":"Leeftijd","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Leeftijd"
    },{
      "name":"Diagnose","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Diagnose"
    },{
      "name":"Zorgsector","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Zorgsector"
    },{
      "name":"Zorgfunctie","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Zorgfunctie"
    },{
      "name":"Financieringsvorm","url":"https://dataderden.cbs.nl/ODataApi/odata/50040NED/Financieringsvorm"
    }
  ]
}'
    
  )
  
  coi <- coi[[paste(year)]]
  # Retrieve data from API ----
  metadata  <- jsonlite::fromJSON(coi)
  mainurl   <- metadata$value$url[metadata$value$name =="TypedDataSet"]
  
  # Deal with disease mapping ----
  mapgroup <- metadata$value$url[metadata$value$name =="CategoryGroups"]
  mapgroup <- jsonlite::fromJSON(mapgroup)$value
  mapgroup <- mapgroup[mapgroup$DimensionKey=="Diagnose",]
  mapgroup <- mapgroup[,c("ID", "Title" )]
  names(mapgroup)  <- c("CategoryGroupID","Group")
  mapgroup$Chapter <- 100
  mapgroup$Chapter[mapgroup$Group == "Nieuwvormingen"]         <- 2
  mapgroup$Chapter[mapgroup$Group == "Psychische stoornissen"] <- 5
  mapgroup$Chapter[mapgroup$Group == "Hartvaatstelsel"]        <- 9
  mapgroup$Chapter[mapgroup$Group == "Ademhalingswegen"]       <- 10
  
  map       <- metadata$value$url[metadata$value$name =="Diagnose"]
  map       <- jsonlite::fromJSON(map)$value
  map$order <- as.numeric(rownames(map))
  map       <- merge(map,mapgroup,all.x = TRUE, by = "CategoryGroupID")
  map       <- map[order(map$order),]
  map$header[map$Group=="Totalen hoofdstuk"] <- map$Key[map$Group=="Totalen hoofdstuk"]
  # Fill in header code
  for (i in 2:length(map$header)) {
    if (is.na(map$header[i])) {
      map$header[i] <- map$header[i-1]
    }
  }
  map$header[map$Group=="Totalen hoofdstuk"]  <- "Header"
  map$header[map$Title=="Totaal"]            <- "Total"
  paidgr <- map %>% 
    filter(!(header %in% c("Total","Header"))) %>%
    mutate("Group in PAID" = row_number())
  paidgr <- paidgr[,c("Key","Group in PAID")]
  map    <- merge(map,paidgr, all.x = TRUE)
  map    <- map[order(map$order),]  
  map$Description <- paste(map$Title, map$Description)
  map <- map[,c("Key","Group in PAID","Chapter","Description","header")]
  rownames(map) <- NULL 
  map$Chapter[map$header=="Header"] <- map$`Group in PAID`[map$header=="Header"] <- "Header"
  map$Chapter[map$header=="Total"] <- map$`Group in PAID`[map$header=="Total"] <- "Total"
  names(map)[1] <- "KvZ codes" 
  
  # Update mapping (diag) for NL
  if (file.exists("data/mapping.rda")) {
    load("data/mapping.rda")
    mapping[["Netherlands"]][[paste(year)]] <- map
    usethis::use_data(mapping, overwrite = TRUE)
  }
  
  
  agegroups <- c("0 jaar","1 tot 5 jaar","5 tot 10 jaar","10 tot 15 jaar","15 tot 20 jaar",
                 "20 tot 25 jaar","25 tot 30 jaar","30 tot 35 jaar","35 tot 40 jaar",
                 "40 tot 45 jaar","45 tot 50 jaar","50 tot 55 jaar","55 tot 60 jaar",
                 "60 tot 65 jaar","65 tot 70 jaar","70 tot 75 jaar","75 tot 80 jaar",
                 "80 tot 85 jaar","85 tot 90 jaar","90 tot 95 jaar","95 jaar of ouder")
  names(agegroups) <- c(0.5, 2.5,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97)
  
  
  ##### Set up filters ------------
  # Pre-filter the data (500 Error if you try to pull all the data). Must be pre-filtered.
  # All diagnoses, all zorgsector, but:
  # RIVM > Total Costs (mln euro)
  onderwerpen <- 'TotaleKostenRIVM_3'    
  # Sex filter by men and women separately
  sex         <- metadata$value$url[metadata$value$name =="Geslacht"]
  sex         <- jsonlite::fromJSON(sex)$value
  ismale      <- sex$Key[sex$Title=="Mannen"]
  sex         <- sex$Key[sex$Title %in% c("Mannen","Vrouwen")]
  # Zorgfunctie - Total only
  zorgfunctie <- metadata$value$url[metadata$value$name =="Zorgfunctie"]
  zorgfunctie <- jsonlite::fromJSON(zorgfunctie)$value
  zorgfunctie <- zorgfunctie$Key[zorgfunctie$Title=="Totaal"]
  # Financieringsvorm - Total only
  finform     <- metadata$value$url[metadata$value$name =="Financieringsvorm"]
  finform     <- jsonlite::fromJSON(finform)$value
  finform     <- finform$Key[finform$Title=="Totaal"]
  # By 5-year age groups
  age         <- metadata$value$url[metadata$value$name =="Leeftijd"]
  age         <- jsonlite::fromJSON(age)$value
  
  ##### Get the data ------------
  # Looping to get data one by one of the age groups, otherwise even with 2 age groups, data set becomes too big and errors out:
  for (a in 1:length(agegroups)) {
    
    agegr <- age$Key[age$Title %in% agegroups[a]]
    
    filter <- paste(
      toFilter(x = onderwerpen, varname = "DataProperties"),
      toFilter(x = sex, varname = "Geslacht"),
      toFilter(x = agegr, varname = "Leeftijd"),
      toFilter(x = zorgfunctie, varname = "Zorgfunctie"),
      toFilter(x = finform, varname = "Financieringsvorm", and = FALSE), 
      sep=""
    )
    
    url <- paste0(
      mainurl,"?",
      "$filter=", utils::URLencode(filter, reserved = FALSE),
      "&$select=Geslacht,Leeftijd,Diagnose,Zorgsector,TotaleKostenRIVM_3"
    )
    
    if (a==1) {
      data     <- jsonlite::fromJSON(url)$value
      data$age <- names(agegroups)[a]
    } else {
      temp     <- jsonlite::fromJSON(url)$value
      temp$age <- names(agegroups)[a]
      data <- rbind(data,temp)
    }
  }
  data$sex <- ifelse(data$Geslacht==ismale,"Men","Women")
  
  return(data)
  
}


#' Cleans up the NL COI data
#' 
#' Prepares the COI data for PAID use.
#'
#' @param data a data frame produced by the `callKvZ()` function.
#'
#' @return a list
cleanKvZ   <- function(data) {
  
  sectors <- c("B000545", # 1. Hospitals (HC) = Ziekenhuis-,medisch specialistische zorg
               "B000560", # 2. Nursing and residential care facilities (LTC) = Ouderenzorg
               "B000565", # 3. Providers of ambulatory healthcare (GP)
               "B000566", # 4. Retail sale and other providers of medical goods (Med)
               "B000564","B000553","B000556",
               "B000567","B000568","B000558","B000569") # 5. Other
  names.sectors  <- c("HC","LTC","GP","MED",rep("Other",7))
  names(sectors) <- names.sectors
  
  l.tc <- vector("list", 5)
  names(l.tc) <- names.sectors[1:5]
  
  # Separate by sector
  for (i in sectors[1:4]) {
    df <- data[data$Zorgsector == i, c("sex","age","Diagnose","TotaleKostenRIVM_3")]
    names(df) <- c("sex","age","diag","totalcosts")
    df <- tidyr::pivot_wider(df,names_from = diag, values_from = totalcosts)
    l.tc[[names(sectors)[sectors == i]]] <- df
  }
  
  df <- data %>%
    filter(Zorgsector %in% sectors[5:11]) %>%
    group_by(sex,age,Diagnose) %>%
    summarize(totalcosts = sum(TotaleKostenRIVM_3,na.rm = TRUE))
  names(df) <- c("sex","age","diag","totalcosts")
  df <- tidyr::pivot_wider(df,names_from = diag, values_from = totalcosts)
  l.tc[[5]] <- df
  l.tc
}

#' Produce per-capita spending
#' 
#'  Takes as input the total costs as seen in COI studies and converts it to per-capita. 
#' 
#' @param country a string
#' @param providers a string vector
#' @param cut.pop.age age groups as defined in the COI study
#'
#' @return a list 
getAC <- function(country, providers, cut.pop.age,coi.year) {
  map  <- mapping[[country]][[paste(coi.year)]]
  ac        <- vector("list", length(providers))
  names(ac) <- providers
  pop     <- pop[pop$country == country,c("sex","age","pop")]
  mx      <- mort[[country]]
  mx      <- mx[,c("sex","age","mort")]
  pop$age[pop$age==100] <- 99.5
  pop$agecat <- cut(as.numeric(pop$age),cut.pop.age, right = FALSE)
  pop <- pop %>%
    group_by(agecat,sex) %>%
    dplyr::summarize(popcat = sum(pop))
  pop <- pop[order(pop$sex),]
  mx  <- mx[order(mx$sex),]
  
  for (p in providers) {
    # Load data
    totcost     <- tc[[country]][[paste(coi.year)]][[p]]
    totcost     <- totcost[order(totcost$sex),]
    
    # For NL, I checked and rowSums(totcost[,dis.code], na.rm=TRUE) == totcost[,total.code] (almost exactly)
    # For FR, they'll match by definition - Total was already created as a sum of diseases, and diseases are non-overlapping
    # For Germany, however, that is not the case :
        # headers (Neoplasms), disease groups (Malignant neoplasms of digestive organs), and diseases (Malignant neoplasm of stomach)
        # are included in the ISHMT and the "Group in PAID" does not have "Headers". Must look at the tree instead.
    dis.code  <- unlist(map[!(map$`Group in PAID` %in% c("Header","Total")),1])
    
    if (country=="Germany") {
      head.code  <- map$`KvZ codes`[map$tree %in% paste(1:20,".",sep = "")]
    }
    
    total.code  <- unlist(map[map$`Group in PAID` == "Total",1])
    totcost     <- totcost[,c("sex","age",total.code,dis.code)]

    # Get average costs (in euros)
    ac0    <- ifelse(country=="France",10^9,10^6) * totcost[,-c(1,2)] / pop$popcat
    v.age  <<- as.numeric(unique(totcost$age))
    nk     <<- ifelse(country=="Netherlands",20,5)
    # Step 1 Smooth average costs into 0-100 ages
    ac1 <- apply(as.matrix(ac0),2, function(x) {
      f.smoothAC(dep = x, lastsexage = ifelse(country=="Netherlands",21,6))
    } )
    # Step 2 Ensure average across all diseases equals total average
    if (country=="Germany") {
      sums <- rowSums(ac1[,head.code], na.rm=TRUE)
    } else {
      sums <- rowSums(ac1[,dis.code], na.rm=TRUE)
    }
    ac0  <- apply(ac1[,dis.code],2,function(x) {x/sums})
    ac1[,2:ncol(ac1)] <- apply(ac0,2,function(x) {x * ac1[,1]})
    # Note: The first 101 rows are for men & the first column is always Total
    ac[[p]] <- ac1
  }
  
  ac
}


#' Disaggregate per-capita spending
#'
#' Attributes costs to survivors and decedents. 
#' 
#' @param country a string
#' @param providers a string vector
#'
#' @return a list
getSCDC <- function(country, providers, coi.year) {
  
  DC <- SC <- vector("list", length(providers) )
  names(DC) <- names(SC) <- providers
  
  for (j in providers) {
    probs   <- probDeath[[country]]
    avrc    <- ac[[country]][[paste(coi.year)]][[j]]
    ratio   <- ratios[["Deterministic"]][[j]]
    res     <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
    avrc    <- avrc[,1] # The first column is always the total (= all diseases)
    r       <- ratio[,3:ncol(ratio)] # The first two columns are "sex" and "age"
    probs   <- probs[,rep(1:5,3)]
    denom   <- (r - 1) * probs
    denom   <- cbind(mean = rowSums(denom[,1:5], na.rm = TRUE), # 1:5 = 5 years of TTD
                     lower = rowSums(denom[,6:10], na.rm = TRUE),
                     upper = rowSums(denom[,11:15], na.rm = TRUE))
    denom  <- 1 + denom
    sc     <- avrc / denom
    sc     <- sc[,c("mean", "lower", "upper")]
    
    dc     <- r * sc[,rep(1:3, each = 5)]
#    dc     <- r * sc[,rep(1, each = 15)]
    
    SC[[j]] <- sc
    DC[[j]] <- dc
  }
  
  return(list(SC = SC,DC = DC))
}


#' Clean COI for DE
#'
#' @return a list of total spending by diseases and age
cleanCOIDE <- function(coi.DE = 2020) {
  
  # Step 1. Get the ICD-10 and sex-specific probability for each provider
  # Load data
  prov.data <- readxl::read_excel("data-raw/Germany_23631-0003_COI_disease_year_providers.xlsx", 
                                  sheet = paste(coi.DE),.name_repair = "unique_quiet")
  prov.data <- prov.data[6:2216,]
  names(prov.data) <- c("Provider_code","Provider_name","ICD_code","ICD_label","Men","Women","Total","header")
  
  # Fill in missing provider codes/labels at all rows
  for (c in 1:2) {
    for (r in 1:dim(prov.data)[1]){
      prov.data[r,c] <- ifelse(is.na(prov.data[r,c]),prov.data[r-1,c],prov.data[r,c])
    }
  }
  # Group providers as per mapping
    # Define mapping
  map <- data.frame("KvZ code" = c("GEINR-1","GEINR-2","GEINR-21","GEINR-22",
                    "GEINR-23","GEINR-24","GEINR-25","GEINR-26","GEINR-3",
                    "GEINR-31","GEINR-32","GEINR-33","GEINR-4","GEINR-5","GEINR-6"),
                    "PAID code" = c("Other","Header","GP","GP","GP","MED","GP","GP",
                                    "Header","HC","LTC","LTC","Other","Other","Other"))
  sectors <- c("HC", "LTC", "MED", "GP",  "Other")
  prov.list <- vector(mode = "list", length = length(sectors))
  names(prov.list) <- sectors
  prov.data$PAID_code <- map$PAID.code[match(prov.data$Provider_code,map$KvZ.code)]
  prov.data <- prov.data[prov.data$PAID_code!="Header",]
  prov.data$Men   <- as.numeric(prov.data$Men)
  prov.data$Women <- as.numeric(prov.data$Women)
  prov.data$Total <- as.numeric(prov.data$Total)
  
  #  Aggregate per provider
  prov.data <- prov.data %>%
    group_by(PAID_code,ICD_code) %>%
    summarize(Men = sum(Men,na.rm = TRUE),
              Women = sum(Women,na.rm = TRUE),
              Total = sum(Total,na.rm = TRUE))
  
  prov.data <- prov.data %>%
    group_by(ICD_code) %>%
    mutate(Men_ICDTotal = sum(Men,na.rm = TRUE),
           Women_ICDTotal = sum(Women,na.rm = TRUE),
           Total_ICDTotal = sum(Total,na.rm = TRUE))
  
  prov.data <-  na.omit(prov.data)
  
  #  Get probability for provider - p(provider | ICD-10, sex)
  prob <- cbind(prov.data[,c(1,2)],prov.data[,c("Men","Women")] / prov.data[,c("Men_ICDTotal","Women_ICDTotal")])
  prob <- tidyr::pivot_longer(prob,cols = Men:Women, names_to = "sex") %>% tidyr::pivot_wider(names_from = PAID_code,values_from = value)
  prob <- prob[order(prob$sex,prob$ICD_code),]
  prob$GP[is.na(prob$GP)] <- 0
  prob$HC[is.na(prob$HC)] <- 0
  prob$LTC[is.na(prob$LTC)] <- 0
  prob$MED[is.na(prob$MED)] <- 0
  prob$Other[is.na(prob$Other)] <- 0
  
  # Assume the same distribution across providers for all ages
  coi.data <- readxl::read_excel("data-raw/Germany_23631-0003_COI_disease_year_age_sex.xlsx", 
                                 sheet = paste(coi.DE),.name_repair = "unique_quiet")
  names(coi.data) <- c("ICD_code","ICD_label",paste(rep(c("Men","Women","Total"),each=7),
                                                    c("under 15 years",
                                                      "15 to under 30 years",
                                                      "30 to under 45 years",
                                                      "45 to under 65 years",
                                                      "65 to under 85 years",
                                                      "85 years and over",
                                                      "Total"), sep="_"))
  
  # Step 2. Disaggregare total costs into provider-specific costs
  # Load COI data
  coi.data <- coi.data[6:146,-2]
  coi.data <- tidyr::pivot_longer(coi.data,cols = !ICD_code, names_to = c("sex","age"), names_sep = "_")
  coi.data <- coi.data[coi.data$age!="Total",]
  coi.data <- coi.data[coi.data$sex!="Total",]
  coi.data <- coi.data[grepl("ICD10-",coi.data$ICD_code),]
  
  # Get mid-age
  coi.data$age[coi.data$age=="under 15 years"]        <- 7.5
  coi.data$age[coi.data$age=="15 to under 30 years"]  <- 22.5
  coi.data$age[coi.data$age=="30 to under 45 years"]  <- 37.5
  coi.data$age[coi.data$age=="45 to under 65 years"]  <- 55
  coi.data$age[coi.data$age=="65 to under 85 years"]  <- 75
  coi.data$age[coi.data$age=="85 years and over"]     <- 92.5
  coi.data$age <- as.numeric(coi.data$age)
  coi.data$value <- as.numeric(coi.data$value)
  coi.data <- merge(coi.data,prob)
  coi.data <- coi.data[order(coi.data$sex,coi.data$ICD_code),]
  
  l.tc <- vector("list",5)
  names(l.tc) <- sectors
  
  coi.data$value[is.na(coi.data$value)] <- 0
  
  
  for (s in sectors) {
    temp <- cbind(coi.data[,c("sex","age","ICD_code")],
                  costs = coi.data$value * coi.data[,s])
    temp$ICD_code <- sub("ICD10-","",temp$ICD_code)
    temp <- tidyr::pivot_wider(temp,names_from = ICD_code,values_from = costs)
    temp[,-1] <- base::sapply(temp[, -1], as.numeric)
    temp[,"A00-T98"] <- NULL # This does not include Z00-Z99, which the column "Total" does.
    l.tc[[s]] <- temp
  }
  
  l.tc
}

#' Clean COI for FR
#'
#' @return a list of total spending by diseases and age
cleanCOIFR <- function() { 
  data     <- readxl::read_excel("data-raw/France_PAID FINAL.xlsx", .name_repair = "unique_quiet")
  provider <- available.providers[["France"]]
  l.tc <- vector("list", length = length(provider)) 
  names(l.tc) <- provider
  sexage      <- grepl("men",tolower(names(data))) 
  df <- apply(data[,sexage],2, function(x) {x * data[,provider]} )
  df <- as.data.frame(df)
  data <- cbind(data[,c("Code in PAID","Disease" )], df)
  data <- data %>% tidyr::pivot_longer(cols = 3:ncol(data),
                                       names_to = c("sex","agegr","provider"), names_pattern = "(.*)_(.*)\\.(.*)" )
  data$age[data$agegr=="0to14"]  <- 7
  data$age[data$agegr=="14to34"] <- 24
  data$age[data$agegr=="35to54"] <- 44.5
  data$age[data$agegr=="55to64"] <- 59.5
  data$age[data$agegr=="65to74"] <- 69.5
  data$age[data$agegr=="75"]     <- 87.5
  data <- data[,c("provider","Code in PAID","sex","age","value")]
  # France COI does not have a "Total" column by default for all costs across a sex/age/provider group.Instead, all we had was total spending by 
  # disease, which was then disaggregated into total spending by sex/age/provider based on number of patients, and proportion reimbursed by provider
  # See email from Baptiste Haon.
  # As such, a "Total" column needs to be created.
  # The next fucntion, getAC(), has a built-in functionality to ensure Total==sum(diag) after smoothing.
  for (j in provider) {
    df  <- data[data$provider==j,] %>% tidyr::pivot_wider(names_from = "Code in PAID", values_from = "value")
    df  <- df[order(df$sex, df$age),-1] 
    tot <- rowSums(df[,-c(1,2)])
    df  <- cbind(df,Total = tot)
    l.tc[[j]] <- df
  }
  l.tc
}


#' Format source mortality 
#'
#' @param country a string
#' @param year numerical
#'
#' @return a data frame
cleanMortRate <- function(country,year = 2019) {
  country <- sub(" ","",country)
  mort   <- readLines(paste("data-raw/",country,"_Mx_1x1.txt",sep="")) 
  header <- which(grepl("Year",mort))
  header.names <- mort[header:header]
  header.names <- strsplit(header.names, " +")
  header.names <- header.names[[1]][grepl("\\S", header.names[[1]])]
  mort    <- mort[(header+1):length(mort)]
  mort    <- strsplit(mort, " +")
  data    <- matrix(NA,nrow = length(mort), ncol = 5)
  colnames(data) <- header.names
  for (i in 1:length(mort)) {
    data[i,] <- mort[[i]][grepl("\\S", mort[[i]])]
  }
  mort <- as.data.frame(data)
  rm(data)
  
  mort <- mort[mort$Year==year,c("Age","Male","Female")]
  names(mort) <- c("age","Men","Women")
  mort <- tidyr::pivot_longer(mort, cols=!age, names_to = "sex", values_to = "mort")
  mort <- mort[,c("mort","sex","age")]
  mort <- mort[order(mort$sex),]
  mort <- mort[mort$age %in% 0:100,]
  mort$age <- as.numeric(mort$age)
  mort$mort <- as.numeric(mort$mort)
  mort
}

#' calculate probability to die TTD years ahead
#'
#' @param country a string 
#'
#' @return a data frame
#'
getProbDeath  <- function(country) {
  mx <- mort[[country]]
  mx <- as.data.frame(mx)
  
  mat <- within(mx,{
    prob_a_ttd0 <- 1 - exp(-mort)
    prob_a_ttd1 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * stats::ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(x))
    prob_a_ttd2 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(x, n=2))
    prob_a_ttd3 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x,n=2)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(x, n=3))
    prob_a_ttd4 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x,n=2)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(1-x,n=3)) * ave(prob_a_ttd0,sex, FUN = function(x) dplyr::lead(x, n=4))
  })
  
  mat <- as.matrix(mat[,c("prob_a_ttd0","prob_a_ttd1","prob_a_ttd2","prob_a_ttd3","prob_a_ttd4")])
  rownames(mat) <- c(paste("Men",0:100,sep=""),paste("Women",0:100,sep=""))
  
  r <- c(98,99,100,101)
  r <- c(r, r+101)
  
  for (i in r) {
    j        <- which(is.na(mat[i,]))
    mat[i,j] <- (1-rowSums(mat[i,,drop=FALSE], na.rm = TRUE))/length(j)
  }
  
  mat
  
}


#' Causes of death - NL
#'
#' @return a data.drame
#'
getDeathCauseNL <- function() {
  
  metadata    <- rjson::fromJSON('{
  "odata.metadata":"https://opendata.cbs.nl/ODataApi/OData/7233ENG/$metadata","value":[
    {
      "name":"TableInfos","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/TableInfos"
    },{
      "name":"UntypedDataSet","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/UntypedDataSet"
    },{
      "name":"TypedDataSet","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/TypedDataSet"
    },{
      "name":"DataProperties","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/DataProperties"
    },{
      "name":"CategoryGroups","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/CategoryGroups"
    },{
      "name":"Sex","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/Sex"
    },{
      "name":"Age","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/Age"
    },{
      "name":"CausesOfDeath","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/CausesOfDeath"
    },{
      "name":"Periods","url":"https://opendata.cbs.nl/ODataApi/odata/7233ENG/Periods"
    }
  ]
}')
  
  names(metadata$value) <- sapply(metadata$value, function(x) {x[["name"]]})
  
  # Retrieve data from API ----
  mainurl   <- metadata[["value"]][["TypedDataSet"]][["url"]]
  
  ##### Set up filters ------------
  # Pre-filter the data (500 Error if you try to pull all the data). Must be pre-filtered.
  
  # Sex filter by men and women separately
  sex         <- metadata[["value"]][["Sex"]][["url"]]
  sex         <- jsonlite::fromJSON(sex)$value
  ismale      <- sex$Key[sex$Title=="Male"]
  sex         <- sex$Key[sex$Title %in% c("Male","Female")]
  # Periods
  period      <- "2024JJ00"
  # Age
  age         <- metadata[["value"]][["Age"]][["url"]]
  age         <- jsonlite::fromJSON(age)$value
  agegroups   <- c("0 year","1 to 4 years","5 to 9 years","10 to 14 years","15 to 19 years","20 to 24 years",
                   "25 to 29 years","30 to 34 years","35 to 39 years","40 to 44 years","45 to 49 years","50 to 54 years","55 to 59 years",
                   "60 to 64 years","65 to 69 years","70 to 74 years","75 to 79 years","80 to 84 years","85 to 89 years","90 to 94 years" ,
                   "95 years or older")
  names(agegroups) <- c(0.5, 2.5,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,100)
  
  ##### Get the data ------------
  # Looping to get data one by one of the age groups, otherwise even with 2 age groups, data set becomes too big and errors out:
  for (a in 1:length(agegroups)) {
    
    agegr <- age$Key[age$Title %in% agegroups[a]]
    
    filter <- paste(
      toFilter(x = period, varname = "Periods"),
      toFilter(x = agegr, varname = "Age"),
      toFilter(x = sex, varname = "Sex", and = FALSE), 
      sep=""
    )
    
    url <- paste0(
      mainurl,"?",
      "$filter=", utils::URLencode(filter, reserved = FALSE),
      "&$select=Sex,Age,CausesOfDeath,Deaths_1"
    )
    
    if (a==1) {
      data     <- jsonlite::fromJSON(url)$value
      data$age <- names(agegroups)[a]
    } else {
      temp     <- jsonlite::fromJSON(url)$value
      temp$age <- names(agegroups)[a]
      data <- rbind(data,temp)
    }
  }
  
  data$Age <- as.numeric(data$Age)
  data$sex <- ifelse(data$Sex==ismale,"Men","Women")
  data$deaths <- as.numeric(data$Deaths_1)
  data$age  <- as.numeric(data$age)
  
  # T001075 total deaths per sex/age group
  total <- data %>%  group_by(sex,age) %>%
    filter(CausesOfDeath == "T001075") %>%
    summarise(D = deaths) %>%
    select(sex,age,D)
  
  # NL_map_cod was manually created to list all COD as in JSON API call but to also link them to COI codes. 
  # NL_map_cod file links COD codes ---> KvZ codes. It is the same as sheet "cod" within "mapping.xlsx" in RShiny under NL.
  NL_map_cod <- readxl::read_excel(paste("data-raw/Netherlands_mapping.xlsx",sep=""),sheet = "cod",.name_repair = "unique_quiet")
  diag <- unique(NL_map_cod$diag_cod)
  diag <- diag[!is.na(diag) & diag!="Header" & diag!="Total"]
  
  
  counter=0
  for (i in diag) {
    code <- NL_map_cod$code[grepl(i,NL_map_cod$diag_cod)]
    x <- data %>%
      filter(CausesOfDeath %in% code) %>%
      group_by(sex,age) %>%
      summarise(N = sum(deaths, na.rm = TRUE)) %>%
      select(sex,age,N)
    x$cod <- i
    if (counter==0) {df <- x} else {df <- rbind(df,x)}
    counter=counter+1
  }
  df    <- tidyr::pivot_wider(df,names_from = cod, values_from = N)
  # In this format, there are multiple disease assigned to one COD count ("A025527_A025511"), and headers, so the rowSums are NOT 1. 
  # But the column "T001178" is all causes of death within an age-sex group, which is what we need 
  
  # Calculate p(x|d=1,a), where the sum across all x = 1; The x's are in column 4 onwards
  probs <- apply(df[4:ncol(df)],2,FUN = function(x) {x/df$T001178})
  probs <- cbind(df[,c("sex","age")],probs)
  
  # Smooth
  ncols <- dim(probs)[2]
  nk         <<- 20
  v.age      <<- as.numeric(unique(probs$age))
  df_new     <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 ,
                                    apply(probs[,3:ncols],2, FUN = function(x) paid4::f.smoothAC(x,inc=1, lastsexage = 21 ) )))
  df_new[,3:ncols] <- apply(df_new[,3:ncols],2,as.numeric)
  
  # This is p(x|d=1,a). For PAID equation (5) we need p(x,d=1|a)
  # Thus, values need to be multiplied by p(d=1|a) [p(nox|d=1,a)*p(d=1|a) = p(nox,d=1|a) == D(nox,a)/N(a)]
  # This will be done in the renderUnrelSCDC()
  probDeathCause <- df_new
  
  # Calculate probability to die of a specific chapter | d=1, a,s = pr(chapterX|d=1,a,s). Used to weight the R(NOX) for related diseases.
  chapters <- c("A010840", "A011087", "A011307", "A011384")
  df    <- probDeathCause[, chapters]
  sums  <- rowSums(df)
  df <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 , df))
  df$`100` <- 1 - sums
  probChapters <- df

  return(list(probDeathCause    = probDeathCause,
              probDeathChapters = probChapters))
  
}


#' Causes of death - DE
#'
#' @return a data.drame
#'
getDeathCauseDE <- function() {
  map  <- readxl::read_excel("data-raw/Germany_mapping.xlsx",sheet = "cod",.name_repair = "unique_quiet")
  map  <- map[!is.na(map$diag_cod),]
  
  # Death counts
  deaths   <- readxl::read_excel("data-raw/Germany_23211-0002_en.xlsx", .name_repair = "unique_quiet")
  start    <- which(grepl("Certain infectious and parasitic diseases",deaths$...3))
  end      <- which(grepl("Total",deaths$...3))
  colnames <- unlist(deaths[start-1,])
  names(deaths) <- colnames
  names(deaths)[1:3] <- c("type", "code", "descr")
  deaths$type[stringr::str_length(deaths$code)==6] <- "Header"
  deaths$type[stringr::str_length(deaths$code)>6]  <- "Disease"
  deaths$type[deaths$descr=="Total"] <- deaths$code[deaths$descr=="Total"] <- "Total"
  deaths   <- deaths[start:end,colnames(deaths) !="age unknown"]
  names(deaths)[4:ncol(deaths)] <- paste(rep(c(0.5,7.5,17,22,27,32,37,42,47,52,57,62,67,72,77,82,92.5), 2), rep( c("Men","Women"), each = 17), sep="_")
  deaths[,4:ncol(deaths)] <- apply(deaths[,4:ncol(deaths)],2, function(x) {
    x <- sub("-",0,x)
    as.numeric(x)
  })
  
  # Insert categories from mapping - this is to be able to assign mortality to diseases that have some cause-specific mortality (generally), but are not listed as a separate group in the deaths.
  df <- data.frame(matrix(NA,10,ncol(deaths)))
  names(df) <- names(deaths)
  df$type  <- "Disease"
  v.codes  <- c("TDU-01","TDU-0211","TDU-0210","TDU-04","TDU-05","TDU-07","TDU-08","TDU-09","TDU-11","TDU-12")
  df$code  <- paste(v.codes,"Other",sep="")
  
  for (x in v.codes[-c(2,3)]) {
    header <- na.omit(deaths[deaths$code==x,4:ncol(deaths)])
    sub    <- deaths[grepl(x,deaths$code) & stringr::str_length(deaths$code)==7 ,4:ncol(deaths)]
    sub    <- colSums(sub,na.rm = TRUE)
    df[df$code == paste(x,"Other",sep=""), 4:ncol(deaths)] <- unlist(header - sub)
  }
  
  # Separately for TDU-0211Other and "TDU-0210Other" because they are not "header - listed"
  header <- na.omit(deaths[deaths$code=="TDU-02114",4:ncol(deaths)])
  sub    <- deaths[deaths$code %in% c("TDU-02117","TDU-02118"),4:ncol(deaths)]
  sub    <- colSums(sub,na.rm = TRUE)
  df[df$code == "TDU-0211Other", 4:ncol(deaths)] <- unlist(header-sub)
  
  sub    <- deaths[deaths$code %in% c("TDU-02102", "TDU-02103", "TDU-02104", "TDU-02105", "TDU-02108"),4:ncol(deaths)]
  sub    <- colSums(sub,na.rm = TRUE)
  df[df$code == "TDU-0210Other", 4:ncol(deaths)] <- unlist(sub)
  
  # Finish deaths
  deaths <- rbind(deaths,df)
  deaths <- deaths[order(deaths$code),]
  deaths <- deaths[deaths$code %in% c("Total",map$code),]
  
  nrep  <- stringr::str_count(map$diag_cod,",") + 1
  names <- stringr::str_split(map$diag_cod,", ")
  names <- Reduce(c,names)
  
  # Do probs
  probs <- deaths
  denom <- unlist(probs[probs$type=="Total",4:ncol(probs)])
  probs[, 4:ncol(probs)] <- sweep(probs[, 4:ncol(probs)], 2, denom, "/")
  probs <- probs[1:(nrow(probs)-1),]
  
  #Reshape so that each code is a column
  probs <- tidyr::pivot_longer(probs, cols = 4:ncol(probs), names_to = c("age","sex"), names_sep = "_", values_to = "px")
  probs <- tidyr::pivot_wider(probs[,c("code","age","sex","px")], names_from = code, values_from = px)
  probs$age <- as.numeric(probs$age)
  
  # By sex & disease, smooth across age
  ncols <- ncol(probs)
  nk         <<- 15
  v.age      <<- as.numeric(unique(probs$age))
  df         <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 ,
                                    apply(probs[,3:ncols],2, FUN = function(x) paid4::f.smoothAC(x,inc=1, lastsexage = 17) )))
  df[,3:ncols] <- apply(df[,3:ncols],2,as.numeric)
  
  #Standardize p(x|d=1,a)
  # Unlike the Dutch causes of death, the DE has overlapping categories of causes of deaths (example: malignant neoplasms of digestive system AND malignant neoplasms of stomach)
  # Therefore, sum across Headers only. Note: I checked and in the "deaths" sum of headers == total death counts per age/sex group.
  # All codes that have 6 characters are headers (e.g. "TDU-01")
  sums  <- names(df)[stringr::str_length(names(df))==6]
  sums  <- rowSums(df[sums], na.rm = TRUE)
  df[,3:ncols] <- apply(df[,3:ncols],2,FUN = function(x) {x/sums})
  
  # Convert from COD code to diagnoses code (e.g., TDU-011 --> A15-A19) as per mapping and expand. Expand so that each diagnosis has its own named column even if it the same as another one
  df_new <- sapply(3:ncol(df), function(x) {df[,rep(x,nrep[x-2])]})
  df_new <- Reduce(cbind,df_new)
  names(df_new) <- names
  probDeathCause <- df_new
  
  # Get the chapter probabilities (for weights)
  # Calculate probability to die of a specific chapter | d=1, a,s = pr(chapterX|d=1,a,s). Used to weight the R(NOX) for related diseases.
  map   <- mapping[["Germany"]][["2020"]]
  names(map) <- c("code","group","chapter","descr","header")
  providers <- available.providers[["Germany"]]
  probCause <- df_new
  rm(df_new)
  
  chapters  <- as.numeric(unique(map$chapter[!(map$chapter %in% c("Header","Total"))]))
  chapters  <- chapters[order(chapters)]
  chap.names <- unique(map[map$chapter %in% chapters,c("chapter","code")])
  chap.names <- chap.names[!duplicated(chap.names$chapter),]
  chap.names$code[chap.names$chapter=="100"] <- "100"
  chap.names <- chap.names[order(as.numeric(chap.names$chapter)),]
  chap.names <- chap.names$code
  
  probChapters           <- matrix(NA,nrow = 202,ncol = length(chapters))
  colnames(probChapters) <- chap.names
  
  counter = 1
  for (chap in chap.names[-5]) {
    probChapters[,counter] <- rowSums(probCause[,chap,drop=FALSE],na.rm = TRUE)
    counter = counter +1
  }
  probChapters[,"100"] <- 1 - rowSums(probChapters[,1:4], na.rm = TRUE)
  
  probDeathChapters <- data.frame(sex = rep(c("Men","Women"), each = 101), 
                                  age = rep(0:100, 2),
                                  probChapters)
  names(probDeathChapters)[3:7] <- colnames(probChapters)
  
  return(list(probDeathCause    = probDeathCause,
              probDeathChapters = probDeathChapters))
  
}


getDeathCauseFR <- function() {
  data <- eurostat::get_eurostat("hlth_cd_aro") %>% 
    filter(sex %in% c("M","F"),
           !(age %in% c("TOTAL", "Y_GE65", "Y_GE85", "Y_LT15", "Y_LT25", "Y_LT65",
                        "Y15-24","Y15-29","Y30-44","Y45-64","Y65-74", "Y75-84")),
           geo == "FR", 
           TIME_PERIOD == "2019-01-01", 
           resid == "TOT_RESID") # All deaths of residents in or outside their home country
  map <-  readxl::read_excel("data-raw/France_mapping.xlsx",sheet = "cod",.name_repair = "unique_quiet")
  
  data$age[data$age=="Y_GE95"] <- "Y95-100"
  data$age[data$age=="Y_LT1"]  <- "Y0-1"
  data$agegr <- sub("Y","",data$age)
  age        <- stringr::str_split(data$agegr,"-")
  age        <- sapply(age,function(x) {c(x[1],x[2])}, simplify = TRUE)
  age        <- t(age)
  colnames(age) <- c("start","end")
  data <- cbind(data,age)
  data$start <- as.numeric(data$start)
  data$end   <- as.numeric(data$end)
  for (i in 1:nrow(data)) {
    data$age[i] <- median(data$start[i]:data$end[i])
  }
  data$age <- as.numeric(data$age)
  data$sex[data$sex=="M"] <- "Men"
  data$sex[data$sex=="F"] <- "Women"
  
  # Diag to COD is many to many:
  # Diag maps to COD 1 to many - 1 diag can have multiple corresponding COD, the sum of which is the true mortality for that diag.
  # 1 COD maps to many diag  - 1 COD can be attributed to many DIAG:
  # Other diseases of the nervous system and the sense organs (remainder of G00-H95) maps to DIS26, DIS27, DIS30, DIS28, DIS31
  # But if I exclude costs of DIS26 and DIS27, mortality of remainder of G00-H95 must be excluded only once, not once per diag.
  # Hence, cod must be grouped in accordance with how diag_cod are grouped.
  diag <- unique(map$diag_cod)
  diag <- diag[!is.na(diag) & diag!="Header" & diag!="Total"]
  
  counter=1
  for (i in diag) {
    code <- map$code[map$diag_cod %in% i]
    temp <- data %>% 
      group_by(sex,age) %>%
      filter(icd10 %in% code) %>%
      summarise(deaths = sum(values,na.rm = TRUE)) %>%
      select(sex,age,deaths)
    temp$diag_code <- i
    if (counter==1) {df <- temp} else {df <- rbind(df,temp)}
    counter = counter+1
  }
  
  total <- data %>%  group_by(sex,age) %>%
    filter(icd10 == "TOTAL") %>%
    summarise(D = values) %>%
    select(sex,age,D)
  
  
  df       <- df %>% tidyr::pivot_wider(names_from = "diag_code", values_from = "deaths")
  probs    <- apply(df[,-c(1,2)],2, function(x) {x / total$D})
  
  # Smooth out into 1-year age groups
  nk       <<- 15
  v.age    <<- as.numeric(unique(df$age))
  probs    <-  apply(probs,2, FUN = function(x) paid4::f.smoothAC(x,inc=1, lastsexage = 21) )
  probs    <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 , probs))
  probDeathCause <- probs
  
  # Get chapter weights
  chapters <- c("C","F","I","J")
  names(chapters) <- c("CH02","CH05","CH09","CH10")
  
  counter = 1
  for (i in chapters) {
    temp <- data %>% group_by(sex, age) %>%
      filter(icd10 %in% i) %>%
      summarise(deaths = values) %>%
      select(sex,age,deaths) 
    temp$probs <- temp$deaths / total$D
    temp$diag_code <- names(chapters[chapters==i])
    if (counter==1) {df <- temp} else {df <- rbind(df,temp)}
    counter = counter+1
  }
  
  df <- df[,c("sex","age","probs" ,"diag_code")]
  df <- df %>% tidyr::pivot_wider(names_from = "diag_code", values_from = "probs")
  probs <- apply(df[,-c(1,2)],2, FUN = function(x) paid4::f.smoothAC(x,inc=1, lastsexage = 21) )
  sums  <- rowSums(probs)
  probs <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 , probs))
  probs$`100` <- 1 - sums
  
  probChapters <- probs
  
  return(list(probDeathCause    = probDeathCause,
              probDeathChapters = probChapters))
}


#' Get population numbers
#' 
#' Retrieve from EUROSTAT population count projections
#'
#' @return
cleanPopCounts <- function() {
  
  # Retrieve the data
  # Source: https://ec.europa.eu/eurostat/databrowser/bookmark/0f193cab-1fcf-407a-bea8-a3b330e76d99?lang=en&createdAt=2026-04-14T10:07:18Z
  pop <- eurostat::get_eurostat("proj_23np") %>% 
    filter(projection=="BSL", 
           sex != "T",
           age %in% c("Y_LT1",paste("Y",1:99,sep=""), "Y_GE100"),
           !(geo %in% c("EU27_2020", "EA_20","EA20")),
           TIME_PERIOD == "2024-01-01")
  geo <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU" ,
           "MT", "NL", "AT" ,"PL", "PT", "RO", "SI" ,"SK" ,"FI" ,"SE", "IS" ,"NO" ,"CH")
  names(geo) <- c("Belgium","Bulgaria","Czechia","Denmark","Germany","Estonia","Ireland","Greece","Spain","France","Croatia","Italy","Cyprus",
                  "Latvia","Lithuania",  "Luxembourg","Hungary","Malta", "Netherlands","Austria","Poland","Portugal" ,
                  "Romania","Slovenia","Slovakia","Finland","Sweden","Iceland","Norway" ,"Switzerland")
  for (i in 1:nrow(pop)) {
    pop$country[i] <- names(geo)[pop$geo[i]==geo]
  }
  pop$age[pop$age=="Y_LT1"]   <- "Y0"
  pop$age[pop$age=="Y_GE100"] <- "Y100"
  pop$age <- as.numeric(gsub("Y","",pop$age))
  pop$sex <- ifelse(pop$sex=="F","Women","Men")
  pop$pop <- pop$values
  pop <- pop[,c("country","sex","age","pop")]
  pop <- pop[order(pop$country,pop$sex,pop$age),]
  rownames(pop) <- NULL
  pop
}


#' Clean up ratios
#'
#' @return
cleanRatios   <- function() {
  
  providers <- c("totalcosts" = "ALL", "zvwkziekenhuis" = "HC", "LTC" = "LTC", "zvwkhuisarts" = "GP", "zvwkfarmacie" = "MED")
  ages      <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
  det_ratios <- psa_ratios    <- vector(mode = "list", length = 6)
  names(det_ratios) <- names(psa_ratios) <- c(providers,"Other")
  
  for (p in names(providers)) {
    ratio   <- RatiosDTA
    ratio   <- ratio[ratio$year == 2023 & ratio$provider == p & ratio$wc=="ALL",]
    
    base_ratio   <- ratio[,c("male","age","TTD","mean", "lower","upper")]
    psa_ratio    <- ratio[,c("male","age","TTD","logmean", "sd")]
    
    names(base_ratio)[names(base_ratio) == "male"] <- names(psa_ratio)[names(psa_ratio) == "male"] <- "sex"
    base_ratio <- base_ratio[order(base_ratio$sex, base_ratio$TTD, base_ratio$age), ]
    base_ratio   <- tidyr::pivot_wider(base_ratio,names_from = TTD, values_from = c("mean", "lower","upper") )
    base_ratio$sex <- ifelse(base_ratio$sex==0,"Women","Men")
    psa_ratio   <- psa_ratio[order(psa_ratio$sex, psa_ratio$TTD, psa_ratio$age), ]
    psa_ratio   <- tidyr::pivot_wider(psa_ratio,names_from = TTD, values_from = c("logmean", "sd") )
    psa_ratio$sex <- ifelse(psa_ratio$sex==0,"Women","Men")
    
    base_ratio <- merge(base_ratio,ages, all= TRUE)
    base_ratio <- base_ratio[order(base_ratio$sex, base_ratio$age),]
    rownames(base_ratio) <- NULL
    
    psa_ratio <- merge(psa_ratio,ages, all= TRUE)
    psa_ratio <- psa_ratio[order(psa_ratio$sex, psa_ratio$age),]
    rownames(psa_ratio) <- NULL
    
    agelow  <- switch(p,"totalcosts" = 51, "zvwkziekenhuis" = 36, "LTC" = 18, "zvwkhuisarts" = 26,  "zvwkfarmacie" = 31)
    agehigh <- switch(p,"totalcosts" = 100, "zvwkziekenhuis" = 100, "LTC" = 100, "zvwkhuisarts" = 100, "zvwkfarmacie" = 100)
    
    base_ratio[,3:17]   <- apply(base_ratio[,3:17], 2,FUN = function(x) {paid4::f.lowhighages(x,a1=agelow,a2=agehigh)})
    psa_ratio[,3:12]    <- apply(psa_ratio[,3:12], 2,FUN = function(x) {paid4::f.lowhighages(x,a1=agelow,a2=agehigh)})
    
    det_ratios[[providers[names(providers) == p]]] <- base_ratio
    psa_ratios[[providers[names(providers) == p]]] <- psa_ratio
  }
  
  # base_ratio ratios for Other = 1 (for now)
  base_ratio[,3:17] <- 1
  det_ratios[["Other"]] <- base_ratio
  
  # Create ratios for Other = 1 (for now)
  psa_ratio[,3:7]  <- 1
  psa_ratio[,8:12] <- 0
  psa_ratios[["Other"]] <- psa_ratio
  
  return(list(Deterministic = det_ratios,PSA = psa_ratios))
}


#' Ratios in absence of chapters
#' 
#' The same base ratios are used across all countries. However, each country uses different disease codes. 
#'
#' @param country a string. Only applicable to COI countries - NL and DE.
#'
#' @return 
cleanRatiosNOTChapter <- function(country, coi.year) {
  
  providers <- c("totalcosts" = "Total", "zvwkziekenhuis" = "HC", "LTC" = "LTC", "zvwkhuisarts" = "GP", "zvwkfarmacie" = "MED")
  ages      <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
  map       <- mapping[[country]][[paste(coi.year)]]
  map       <- map[,c(1,2,3,5)]
  names(map) <- c("code","group","chapter","header")
  notratio    <- vector(mode = "list", length = 6)
  names(notratio) <- c(providers,"Other")
  res    <- vector(mode = "list", length = 2)
  names(res) <- c("Deterministic","PSA")
  
  for (m in c("PSA","Deterministic")) {
    for (p in names(providers)) {
      ratio   <- RatiosDTA
      ratio   <- ratio[ratio$year == 2023 & ratio$provider == p & !(ratio$wc %in% c("ALL","22")),]
      
      if (m=="Deterministic") {
        ratio  <- ratio[,c("male","age","TTD","wc","mean", "lower","upper")] 
      } else {
        ratio <- ratio[,c("male","age","TTD","wc","logmean", "sd")]
      }
      
      names(ratio)[names(ratio)=="male"] <- "sex"
      ratio$sex <- ifelse(ratio$sex==0,"Women","Men")
      
      # Expand the wc100 data frame - one for each header in category 100
      ratio$chapter <- NA
      
      for (i in c(2,5,9,10)) {
        ratio$chapter[ratio$wc==i] <- unique(map$header[map$chapter == i])
      }
      ratio$chapter[ratio$wc==100] <- "100"
      ratio$wc <- factor(ratio$wc, levels = c(2,5,9,10,100))
      ratio <- ratio[order(ratio$sex, ratio$wc, ratio$TTD, ratio$age),]
      ratio$wc <- NULL
      
      if (m=="Deterministic") {
        ratio  <- tidyr::pivot_wider(ratio,names_from = c("TTD","chapter"), values_from = c("mean", "lower","upper"))
      } else {
        ratio <-  tidyr::pivot_wider(ratio,names_from = c("TTD","chapter"), values_from = c("logmean", "sd"))
      }
      
      ratio <- merge(ratio,ages,all=TRUE)
      agelow  <- switch(p,"totalcosts" = 51, "zvwkziekenhuis" = 36, "LTC" = 18, "zvwkhuisarts" = 26,  "zvwkfarmacie" = 31)
      agehigh <- switch(p,"totalcosts" = 100, "zvwkziekenhuis" = 100, "LTC" = 100, "zvwkhuisarts" = 100, "zvwkfarmacie" = 100)
      # 
      # agelow  <- switch(p,"totalcosts" = 51, "zvwkziekenhuis" = 51, "LTC" = 18, "zvwkhuisarts" = 1,  "zvwkfarmacie" = 51)
      # agehigh <- switch(p,"totalcosts" = 96, "zvwkziekenhuis" = 96, "LTC" = 96, "zvwkhuisarts" = 100, "zvwkfarmacie" = 96)
      ratio <- ratio[order(ratio$sex,ratio$age),]
      cols <- dim(ratio)[2]
      ratio[,3:cols]   <- apply(ratio[,3:cols], 2,FUN = function(x) {paid4::f.lowhighages(x,a1=agelow,a2=agehigh)})
      notratio[[providers[names(providers) == p]]] <- ratio
    }
    
    # Create ratios for Other = 1 (for now)
    if (m=="Deterministic") {
      ratio[,3:cols] <- 1
    } else {
      ratio[,3:7]  <- 1
      ratio[,8:12] <- 0
    }
    
    notratio[["Other"]] <- ratio
    res[[m]] <- notratio
    
  }
  
  return(res)
  
}

