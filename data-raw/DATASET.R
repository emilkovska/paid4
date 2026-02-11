## code to prepare `DATASET` dataset goes here

all.country <- c("Netherlands","Germany", "Greece","Spain","United Kingdom")
coi.country <- c("Netherlands", "Germany")
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
                             "Greece"      = c("Inpatient hospital" = "HC"),
                             "Spain"       = c("Inpatient, specialized outpatient, prescription drugs, transport, and other healthcare categories" = "ALL"),
                             "United Kingdom" = c("Inpatient and outpatient care, and GP and pharmaceutical spending" = "ALL"))

available.costmethods <- list("Netherlands"  = c("From cost of illness" = "dscosts",
                                                 "From own data"        = "totcosts"),
                              "Germany"      = c("From cost of illness" = "dscosts",
                                                 "From own data"        = "totcosts"),
                              "Greece"       = c("From own data"        = "totcosts"),
                              "Spain"        = c("From own data"        = "totcosts"),
                              "United Kingdom" = c("From own data"        = "totcosts")
)

##### MORTALITY  ######
cleanMortRate <- function(country,year = 2019) {
  mort   <- readLines(paste(sub(" ","",country),"/Mx_1x1.txt",sep=""))
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
  mort <- mort %>% pivot_longer(cols=!age, names_to = "sex", values_to = "mort")
  mort <- mort[,c("mort","sex","age")]
  mort <- mort[order(mort$sex),]
  mort <- mort[mort$age %in% 0:100,]
  mort$age <- as.numeric(mort$age)
  mort$mort <- as.numeric(mort$mort)
  mort
}
getProbDeath  <- function(country) {
  mx <- mort[[country]]
  mx <- as.data.frame(mx)

  mat <- within(mx,{
    prob_a_ttd0 <- 1 - exp(-mort)
    prob_a_ttd1 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * stats::ave(prob_a_ttd0,sex, FUN = function(x) lead(x))
    prob_a_ttd2 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(x, n=2))
    prob_a_ttd3 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x,n=2)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(x, n=3))
    prob_a_ttd4 <- ave(prob_a_ttd0,sex, FUN = function(x) 1-x) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x,n=2)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(1-x,n=3)) * ave(prob_a_ttd0,sex, FUN = function(x) lead(x, n=4))
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

folders <- list.dirs("inst/extdata")[-1]
ctry    <- paid4::f.right(folders,nchar(folders) - 13)
probDeath <- mort  <- vector(mode = "list", length = length(ctry))
names(mort) <- names(probDeath) <- ctry

for (i in 1:length(ctry)) {
  mort[[i]] <- cleanMortRate(country = folders[i])
  mort <<- mort
  probDeath[[i]] <- getProbDeath(ctry[i])
}

# Calculate p(x,d=1|a,s) - Onlt for NL & DE (COI countries)

probDeathCause <- probDeathChapters <- vector("list", length(coi.country))
names(probDeathCause) <- names(probDeathChapters) <- coi.country

getDeathCauseNL <- function() {

  data    <- rjson::fromJSON(file="inst/extdata/Netherlands/7233ENG_UntypedDataSet_10072025_133443.json")
  mapping <- readxl::read_excel("inst/extdata/Netherlands/mapping.xlsx", sheet = "cod",.name_repair = "unique_quiet")
  data <- data$value
  df   <- do.call("rbind",data)
  data <- as.data.frame(df)
  rm(df)

  cod <- unique(mapping$diag_cod[!(mapping$type %in% "Header")]) #Get only the actual diseases, not headers
  cod <- cod[!is.na(cod)]
  cod <- cod[nchar(cod)==7] # This is to get unique cod because in some cases a cod code can be linked to multiple diagnoses per ISHMT.
  # All other causes of death that could not be linked to a ISHMT diagnosis were coded as "Otherca" (7 characters)
  # The first code is T001178 - all diseases combined, aka q(a)

  data$Sex <- as.numeric(data$Sex)
  data$Age <- as.numeric(data$Age)
  data$deaths <- as.numeric(data$Deaths_1)

  data$sex <- ifelse(data$Sex == 3000,"Men","Women")
  data$age[data$Age==10010] <- 0.5       # 0 jaar
  data$age[data$Age==51300] <- 2.5       # 1 tot 5 jaar (not including 5)
  data$age[data$Age==70200] <- 7         # 5 tot 10 jaar (not including 10)
  data$age[data$Age==70300] <- 12        # 10 tot 15 jaar (not including 15)
  data$age[data$Age==70400] <- 17        # 15 tot 20 jaar (not including 20)
  data$age[data$Age==70500] <- 22        # 20 tot 25 jaar (etc...)
  data$age[data$Age==70600] <- 27        # 25 tot 30 jaar
  data$age[data$Age==70700] <- 32        # 30 tot 35 jaar
  data$age[data$Age==70800] <- 37        # 35 tot 40 jaar
  data$age[data$Age==70900] <- 42        # 40 tot 45 jaar
  data$age[data$Age==71000] <- 47        # 45 tot 50 jaar
  data$age[data$Age==71100] <- 52        # 50 tot 55 jaar
  data$age[data$Age==71200] <- 57        # 55 tot 60 jaar
  data$age[data$Age==71300] <- 62        # 60 tot 65 jaar
  data$age[data$Age==71400] <- 67        # 65 tot 70 jaar
  data$age[data$Age==71500] <- 72        # 70 tot 75 jaar
  data$age[data$Age==71600] <- 77        # 75 tot 80 jaar
  data$age[data$Age==71700] <- 82        # 80 tot 85 jaar
  data$age[data$Age==71800] <- 87        # 85 tot 90 jaar
  data$age[data$Age==71900] <- 92        # 90 tot 95 jaar
  data$age[data$Age==22000] <- 100       # 95 jaar of ouder

  data <- data[!is.na(data$age),]

  # Count the number of deaths by diagnose | age,sex
  counter=0
  for (i in cod) {
    v.codcode <- mapping$code[grepl(i,mapping$diag_cod)]
    x <- data %>%
      filter(CausesOfDeath %in% v.codcode) %>%
      group_by(sex,age) %>%
      summarize(N = sum(deaths, na.rm = TRUE))
    x$cod <- i
    if (counter==0) {df <- x} else {df <- rbind(df,x)}
    counter=counter+1
  }

  df    <- pivot_wider(df,names_from = cod, values_from = N)
  # rowSums(df[,4:113], na.rm = TRUE) generally equals df$T001178 with 1-person deviations here and there (rounding errors, inevitable).
  # Nevertheless, ensure it's an exact match:
  ncols <- dim(df)[2]
  df$T001178 <- rowSums(df[,4:ncols], na.rm = TRUE)

  # Calculate p(x|d=1,a), where the sum across all x = 1; The x's are in column 4 onwards
  probs <- apply(df[4:ncols],2,FUN = function(x) {x/df$T001178})
  probs <- cbind(df[,c("sex","age")],probs)

  # Smooth
  ncols <- dim(probs)[2]
  lastsexage <<- 21
  nk         <<- 20
  v.age      <<- as.numeric(unique(probs$age))
  df_new     <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 ,apply(probs[,3:ncols],2, FUN = function(x) f.smoothAC(x,inc=1) )))
  df_new[,3:ncols] <- apply(df_new[,3:ncols],2,as.numeric)

  #Standardize p(x|d=1,a)
  sums <- rowSums(df_new[,3:ncols], na.rm = TRUE)
  df_new[,3:ncols] <- apply(df_new[,3:ncols],2,FUN = function(x) {x/sums})

  # This is p(x|d=1,a). For PAID equation (5) we need p(x,d=1|a)
  # Thus, values need to be multiplied by p(d=1|a) [p(nox|d=1,a)*p(d=1|a) = p(nox,d=1|a) == D(nox,a)/N(a)]
  # This will be done in the renderUnrelSCDC()

  probDeathCause <- df_new

  # Calculate probability to die of a specific chapter | d=1, a,s = pr(chapterX|d=1,a,s). Used to weight the R(NOX) for related diseases.
  mapping   <- readxl::read_excel("inst/extdata/Netherlands/mapping.xlsx", sheet = "diag")
  names(mapping) <- c("code","group","chapter","descr","header")
  providers <- available.providers[["Netherlands"]]
  probCause <- df_new
  rm(df_new)
  probCause <- probCause[,-c(1,2)]

  chapters  <- as.numeric(unique(mapping$chapter[!(mapping$chapter %in% c("Header","Total"))]))
  chapters  <- chapters[order(chapters)]
  chap.names <- unique(mapping[mapping$chapter %in% chapters,c("chapter","header")])
  chap.names <- chap.names[!duplicated(chap.names$chapter),]
  chap.names$header[chap.names$chapter=="100"] <- "100"
  chap.names <- chap.names[order(as.numeric(chap.names$chapter)),]
  chap.names <- chap.names$header

  probChapters           <- matrix(NA,nrow = 202,ncol = length(chapters))
  colnames(probChapters) <- chap.names

  counter = 1
  for (chap in chapters) {
    disnames <- mapping$code[mapping$chapter==chap]
    # Select only those diseases that have cause of death coded
    disnames <- disnames[disnames %in% colnames(probCause)]
    probChapters[,counter] <- rowSums(probCause[,disnames,drop=FALSE],na.rm = TRUE)
    counter = counter +1
  }

  sums <-  rowSums(probChapters,na.rm = TRUE)
  probChapters <- apply(probChapters,2,function(x) {x/sums})
  probDeathChapters <- probChapters

  return(list(probDeathCause    = probDeathCause,
              probDeathChapters = probChapters))

}
getDeathCauseDE <- function() {
  mapping  <- readxl::read_excel("inst/extdata/Germany/mapping.xlsx", sheet = "cod",.name_repair = "unique_quiet")
  mapping  <- mapping[!is.na(mapping$diag_cod),]

  # Death counts
  deaths   <- readxl::read_excel("inst/extdata/Germany/23211-0002_en.xlsx", .name_repair = "unique_quiet")
  start    <- which(grepl("Certain infectious and parasitic diseases",deaths$...3))
  end      <- which(grepl("Total",deaths$...3))
  colnames <- unlist(deaths[start-1,])
  names(deaths) <- colnames
  names(deaths)[1:3] <- c("type", "code", "descr")
  deaths$type[str_length(deaths$code)==6] <- "Header"
  deaths$type[str_length(deaths$code)>6]  <- "Disease"
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
    sub    <- deaths[grepl(x,deaths$code) & str_length(deaths$code)==7 ,4:ncol(deaths)]
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
  deaths <- deaths[deaths$code %in% c("Total",mapping$code),]

  nrep  <- stringr::str_count(mapping$diag_cod,",") + 1
  names <- stringr::str_split(mapping$diag_cod,", ")
  names <- Reduce(c,names)

  # Do probs
  probs <- deaths
  denom <- unlist(probs[probs$type=="Total",4:ncol(probs)])
  probs[, 4:ncol(probs)] <- sweep(probs[, 4:ncol(probs)], 2, denom, "/")
  probs <- probs[1:(nrow(probs)-1),]

  #Reshape so that each code is a column
  probs <- probs %>% pivot_longer(cols = 4:ncol(probs), names_to = c("age","sex"), names_sep = "_", values_to = "px")
  probs <- pivot_wider(probs[,c("code","age","sex","px")], names_from = code, values_from = px)
  probs$age <- as.numeric(probs$age)

  # By sex & disease, smooth across age
  ncols <- ncol(probs)
  lastsexage <<- 17
  nk         <<- 15
  v.age      <<- as.numeric(unique(probs$age))
  df         <- as.data.frame(cbind(sex = rep(c("Men","Women"), each = 101), age = 0:100 ,apply(probs[,3:ncols],2, FUN = function(x) f.smoothAC(x,inc=1) )))
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
  mapping   <- readxl::read_excel("inst/extdata/Germany/mapping.xlsx", sheet = "diag")
  names(mapping) <- c("code","group","chapter","descr","header")
  providers <- available.providers[["Germany"]]
  probCause <- df_new
  rm(df_new)

  chapters  <- as.numeric(unique(mapping$chapter[!(mapping$chapter %in% c("Header","Total"))]))
  chapters  <- chapters[order(chapters)]
  chap.names <- unique(mapping[mapping$chapter %in% chapters,c("chapter","code")])
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

  probDeathChapters <- probChapters

  return(list(probDeathCause    = probDeathCause,
              probDeathChapters = probChapters))

}

x <- getDeathCauseNL()
probDeathCause[["Netherlands"]]    <- x[["probDeathCause"]]
probDeathChapters[["Netherlands"]] <- x[["probDeathChapters"]]

x <- getDeathCauseDE()
probDeathCause[["Germany"]]    <- x[["probDeathCause"]]
probDeathChapters[["Germany"]] <- x[["probDeathChapters"]]


##### POPULATION  ######
cleanPopCounts <- function() {

  counter = 0
  for (i in 3:204) {
    counter <- counter + 1
    temp    <- readxl::read_excel("inst/extdata/EUROSTAT_Population_projections.xlsx", sheet = i)

    # Get range of countries
    start <- which(temp[,1] == "GEO (Labels)") + 1
    end   <- which(temp[,1] == "Special value") - 2

    # Get attributes
    sex   <- unlist(temp[which(temp[,1] == "Sex"),3])
    age   <- unlist(temp[which(temp[,1] == "Age class"),3])
    temp  <- temp[start:end,1:2]
    names(temp) <- c("country","pop")
    temp$sex <- sex
    temp$age <- age

    # Stack
    if (counter==1) {
      data <- temp
    } else {
      data <- rbind(data,temp)
    }

  }

  data$sex[data$sex=="Males"]   <- "Men"
  data$sex[data$sex=="Females"] <- "Women"
  data$pop <- as.numeric(data$pop)
  data$age <- gsub(" years","",data$age)
  data$age <- gsub(" year","",data$age)
  data <- data[order(data$country),]
  data$age[data$age == "Less than 1"] <- 0
  data$age[data$age == "100 or over"] <- 100
  data
}
pop            <- cleanPopCounts()

##### RATIOS  ######
cleanRatios   <- function() {

  providers <- c("totalcosts" = "ALL", "zvwkziekenhuis" = "HC", "LTC" = "LTC", "zvwkhuisarts" = "GP", "zvwkfarmacie" = "MED")
  ages      <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
  ratios    <- vector(mode = "list", length = 6)
  names(ratios) <- c(providers,"Other")

  for (p in names(providers)) {
    ratio   <- haven::read_dta("inst/extdata/Ratios.dta")
    ratio   <- ratio[ratio$year == 4 & ratio$provider == p & ratio$wc==0,]
    ratio$TTD   <- ratio$TTD - 1
    ratio   <- ratio[,c("male","age","TTD","deterministic", "lower","upper")]
    names(ratio)[names(ratio) %in% c("male","deterministic")] <- c("sex","mean")
    ratio   <- pivot_wider(ratio,names_from = TTD, values_from = c("mean", "lower","upper") )
    ratio$sex <- ifelse(ratio$sex==0,"Women","Men")
    ratio <- merge(ratio,ages, all= TRUE)
    ratio <- ratio[order(ratio$sex, ratio$age),]
    rownames(ratio) <- NULL

    agelow  <- switch(p,"totalcosts" = 51, "zvwkziekenhuis" = 51, "LTC" = 18, "zvwkhuisarts" = 1,  "zvwkfarmacie" = 51)
    agehigh <- switch(p,"totalcosts" = 96, "zvwkziekenhuis" = 96, "LTC" = 95, "zvwkhuisarts" = 96, "zvwkfarmacie" = 100)

    ratio[,3:17]   <- apply(ratio[,3:17], 2,FUN = function(x) {paid4::f.lowhighages(x,a1=agelow,a2=agehigh)})

    ratios[[providers[names(providers) == p]]] <- ratio
  }

  # Create ratios for Other = 1 (for now)
  ratio[,3:17] <- 1
  ratios[["Other"]] <- ratio
  ratios
}
ratios        <- cleanRatios()

##### NOT RATIOS - for COI studies only (DE & NL)  ######

cleanRatiosNOTChapter <- function(country) {

  providers <- c("totalcosts" = "Total", "zvwkziekenhuis" = "HC", "LTC" = "LTC", "zvwkhuisarts" = "GP", "zvwkfarmacie" = "MED")
  ages      <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
  mapping   <- readxl::read_excel(paste("inst/extdata/",sub(" ","",country),"/mapping.xlsx",sep=""), sheet = "diag")
  mapping   <- mapping[,c(1,2,3,5)]
  names(mapping) <- c("code","group","chapter","header")
  notratios    <- vector(mode = "list", length = 6)
  names(notratios) <- c(providers,"Other")

  for (p in names(providers)) {
    ratio   <- haven::read_dta("inst/extdata/Ratios.dta")
    ratio   <- ratio[ratio$year == 4 & ratio$provider == p & ratio$wc!=0,]
    ratio$TTD <- ratio$TTD - 1
    ratio     <- ratio[,c("male","age","TTD","wc","deterministic", "lower","upper")]
    names(ratio)[names(ratio) %in% c("male","deterministic")] <- c("sex","mean")
    ratio$sex <- ifelse(ratio$sex==0,"Women","Men")

    # Expand the wc100 data frame - one for each header in category 100
    ratio$chapter <- NA

    for (i in c(2,5,9,10)) {
      ratio$chapter[ratio$wc==i] <- unique(mapping$header[mapping$chapter == i])
    }

    ratio$chapter[ratio$wc==100] <- "100"
    ratio <- ratio[,c("sex","age","TTD","chapter", "mean","lower","upper")]
    ratio <- pivot_wider(ratio,names_from = c("TTD","chapter"), values_from = c("mean", "lower","upper") )
    ratio <- merge(ratio,ages,all=TRUE)

    agelow  <- switch(p,"totalcosts" = 51, "zvwkziekenhuis" = 51, "LTC" = 18, "zvwkhuisarts" = 1,  "zvwkfarmacie" = 51)
    agehigh <- switch(p,"totalcosts" = 96, "zvwkziekenhuis" = 96, "LTC" = 95, "zvwkhuisarts" = 96, "zvwkfarmacie" = 100)

    ratio <- ratio[order(ratio$sex,ratio$age),]

    cols <- dim(ratio)[2]
    ratio[,3:cols]   <- apply(ratio[,3:cols], 2,FUN = function(x) {paid4::f.lowhighages(x,a1=agelow,a2=agehigh)})
    notratios[[providers[names(providers) == p]]] <- ratio
  }

  # Create ratios for Other = 1 (for now)
  ratio[,3:cols] <- 1
  notratios[["Other"]] <- ratio
  notratios
}
coi_ctry  <- c("Netherlands", "Germany")
notratios <- vector("list", 2)
names(notratios) <- coi_ctry

for (i in coi_ctry) {
  notratios[[i]] <- cleanRatiosNOTChapter(i)
}

##### GENERATE TC files ######
cleanKvZ <- function() {
  data <- rjson::fromJSON(file="inst/extdata/Netherlands/50091NED_UntypedDataSet_20032024_113542.json")
  data <- data$value
  df   <- do.call("rbind",data)
  data <- as.data.frame(df)
  rm(df)

  data$Geslacht <- as.numeric(data$Geslacht)
  data$Leeftijd <- as.numeric(data$Leeftijd)
  data$TotaleKostenRIVM_3 <- as.numeric(data$TotaleKostenRIVM_3)

  data$sex <- ifelse(data$Geslacht == 3000,"Men","Women")
  data$age[data$Leeftijd==10010] <- 0.5       # 0 jaar
  data$age[data$Leeftijd==51300] <- 2.5       # 1 tot 5 jaar (not including 5)
  data$age[data$Leeftijd==70200] <- 7         # 5 tot 10 jaar (not including 10)
  data$age[data$Leeftijd==70300] <- 12        # 10 tot 15 jaar (not including 15)
  data$age[data$Leeftijd==70400] <- 17        # 15 tot 20 jaar (not including 20)
  data$age[data$Leeftijd==70500] <- 22        # 20 tot 25 jaar (etc...)
  data$age[data$Leeftijd==70600] <- 27        # 25 tot 30 jaar
  data$age[data$Leeftijd==70700] <- 32        # 30 tot 35 jaar
  data$age[data$Leeftijd==70800] <- 37        # 35 tot 40 jaar
  data$age[data$Leeftijd==70900] <- 42        # 40 tot 45 jaar
  data$age[data$Leeftijd==71000] <- 47        # 45 tot 50 jaar
  data$age[data$Leeftijd==71100] <- 52        # 50 tot 55 jaar
  data$age[data$Leeftijd==71200] <- 57        # 55 tot 60 jaar
  data$age[data$Leeftijd==71300] <- 62        # 60 tot 65 jaar
  data$age[data$Leeftijd==71400] <- 67        # 65 tot 70 jaar
  data$age[data$Leeftijd==71500] <- 72        # 70 tot 75 jaar
  data$age[data$Leeftijd==71600] <- 77        # 75 tot 80 jaar
  data$age[data$Leeftijd==71700] <- 82        # 80 tot 85 jaar
  data$age[data$Leeftijd==71800] <- 87        # 85 tot 90 jaar
  data$age[data$Leeftijd==71900] <- 92        # 90 tot 95 jaar
  data$age[data$Leeftijd==22000] <- 97        # 95 jaar of ouder

  data <- data[!is.na(data$age),]

  dist.cost <- data %>%
    group_by(Zorgsector) %>%
    summarize(total = sum(TotaleKostenRIVM_3,na.rm = TRUE))

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
    df <- df %>% pivot_wider(names_from = diag, values_from = totalcosts)
    l.tc[[names(sectors)[sectors == i]]] <- df
  }

  df <- data %>%
    filter(Zorgsector %in% sectors[5:11]) %>%
    group_by(sex,age,Diagnose) %>%
    summarize(totalcosts = sum(TotaleKostenRIVM_3,na.rm = TRUE))
  names(df) <- c("sex","age","diag","totalcosts")
  df <- df %>% pivot_wider(names_from = diag, values_from = totalcosts)
  l.tc[[5]] <- df
  l.tc
}
tc_NL    <- cleanKvZ()

cleanCOIDE <- function(year=2020) {

  # Step 1. Get the ICD-10 and sex-specific probability for each provider
  # Load data
  prov.data <- readxl::read_excel("inst/extdata/Germany/23631-0003_COI_disease_year_providers.xlsx", sheet = paste(year),.name_repair = "unique_quiet")
  prov.data <- prov.data[6:2216,]
  names(prov.data) <- c("Provider_code","Provider_name","ICD_code","ICD_label","Men","Women","Total","header")

  # Fill in missing provider codes/labels at all rows
  for (c in 1:2) {
    for (r in 1:dim(prov.data)[1]){
      prov.data[r,c] <- ifelse(is.na(prov.data[r,c]),prov.data[r-1,c],prov.data[r,c])
    }
  }
  # Group providers as per mapping
  mapping <- readxl::read_excel("inst/extdata/Germany/mapping.xlsx", sheet = "sector",.name_repair = "unique_quiet")
  mapping <- mapping[!(mapping[,3] == "Header") ,]
  sectors <- unique(mapping$`PAID Code`)
  prov.list <- vector(mode = "list", length = length(sectors))
  names(prov.list) <- sectors
  prov.data$PAID_code <- mapping$`PAID Code`[match(prov.data$Provider_code,mapping$`KvZ code`)]
  prov.data <- prov.data[!is.na(prov.data$PAID_code),]
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

  #  Get probability for provider - p(provider | ICD-10, sex)
  prob <- cbind(prov.data[,c(1,2)],prov.data[,c("Men","Women")] / prov.data[,c("Men_ICDTotal","Women_ICDTotal")])
  prob <- prob %>% pivot_longer(cols = Men:Women, names_to = "sex") %>% pivot_wider(names_from = PAID_code,values_from = value)
  prob <- prob[order(prob$sex,prob$ICD_code),]


  # Assume the same distribution across providers for all ages
  coi.data <- readxl::read_excel("inst/extdata/Germany/23631-0003_COI_disease_year_age_sex.xlsx", sheet = paste(year),.name_repair = "unique_quiet")
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
  coi.data <- coi.data %>% pivot_longer(cols = !ICD_code, names_to = c("sex","age"), names_sep = "_")
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

  for (s in sectors) {
    temp <- cbind(coi.data[,c("sex","age","ICD_code")],
                  costs = coi.data$value * coi.data[,s])
    temp$ICD_code <- sub("ICD10-","",temp$ICD_code)
    temp <- temp %>% pivot_wider(names_from = ICD_code,values_from = costs)
    temp[,-1] <- base::sapply(temp[, -1], as.numeric)
    temp[,"A00-T98"] <- NULL
    l.tc[[s]] <- temp
  }


  l.tc
}
tc_DE      <- cleanCOIDE()
tc         <- list("Netherlands" = tc_NL, "Germany" = tc_DE)

usethis::use_data(mort, probDeath, pop, ratios, notratios, tc, probDeathCause, probDeathChapters, internal = TRUE, overwrite = TRUE)
load_all()

##### GENERATE AC files #########

group.pop.age  <- list("Netherlands" =  c(0,1,seq(5,101,by = 5)),
                       "Germany"     =  c(0,15,30,45,65,85,100))

getAC <- function(country, providers, cut.pop.age) {

  mapping <- readxl::read_excel(paste("inst/extdata/",sub(" ","",country),"/mapping.xlsx",sep=""), sheet = "diag",.name_repair = "unique_quiet")
  ac        <- vector("list", length(providers))
  names(ac) <- providers
  pop     <- pop[pop$country == country,c("sex","age","pop")]
  mort    <- mort[[country]]
  mort    <- mort[,c("sex","age","mort")]
  pop$age[pop$age==100] <- 99.5
  pop$agecat <- cut(as.numeric(pop$age),cut.pop.age, right = FALSE)
  pop <- pop %>%
    group_by(agecat,sex) %>%
    dplyr::summarize(popcat = sum(pop))
  pop <- pop[order(pop$sex),]
  mort <- mort[order(mort$sex),]

  for (p in providers) {
    # Load data
    tc      <- TC[[country]][[p]]
    tc          <- tc[order(tc$sex),]
    dis.code    <- unlist(mapping[!(mapping$`Group in PAID` %in% c("Header","Total")),1])
    total.code  <- unlist(mapping[mapping$`Group in PAID` == "Total",1])
    tc          <- tc[,c("sex","age",total.code,dis.code)]
    # For NL, I checked and rowSums(tc[,dis.code], na.rm=TRUE) == tc[,total.code] (almost exactly)
    # Get average costs (in euros)
    ac0    <- 1000000 * tc[,-c(1,2)] / pop$popcat
    v.age        <<- as.numeric(unique(tc$age))
    lastsexage   <<- ifelse(country=="Netherlands",21,6)
    nk           <<- ifelse(country=="Netherlands",20,5)
    # Step 1 Smooth average costs into 0-100 ages
    ac1 <- apply(as.matrix(ac0),2,f.smoothAC)
    # Step 2 Ensure average across all diseases equals total average
    sums <- rowSums(ac1[,dis.code], na.rm=TRUE)
    ac0  <- apply(ac1[,dis.code],2,function(x) {x/sums})
    ac1[,2:ncol(ac1)] <- apply(ac0,2,function(x) {x * ac1[,1]})
    # Note: The first 101 rows are for men & the first column is Total
    ac[[p]] <- ac1
  }

  ac
}

ac <- vector("list",5)
names(ac) <- all.country

for (coi in c("Netherlands","Germany")) {
  TC  <<- tc
  pop <<- pop
  mort <<- mort
  ac[[coi]] <- getAC(country = coi, cut.pop.age = group.pop.age[[coi]], providers = available.providers[[coi]])
}


# The ones from Hamraz'  paper where I have their dc/sc but for TTD=0 only. Need to convert them to AC -> DC(ttd) & SC.
for (cntry in c("Greece","Spain","United Kingdom")) {
  # Calculate AC
  data <- readxl::read_excel("inst/extdata/Mokriestimates.xlsx", sheet = cntry,.name_repair = "unique_quiet")
  mx   <- mort[[cntry]]
  qx   <- 1-exp(-mx$mort)
  AC   <- as.data.frame(data$sc * (1-qx) + data$dc*qx)
  names(AC) <- "Total"

  # Smooth out ac results & save
  nk             <<- 12
  v.age          <<- 0:100
  lastsexage     <<- 101
  AC$Total       <- f.smoothAC(AC$Total, inc=0)
  ac[[cntry]]    <- list(as.matrix(AC))
  names(ac[[cntry]])        <- available.providers[[cntry]]
}

usethis::use_data(mort, probDeath, pop, ratios, notratios, tc, ac, probDeathCause, probDeathChapters, internal = TRUE, overwrite = TRUE)
load_all()

##### GENERATE SC DC files #####

getSCDC <- function(country, providers, repif) {

  DC <- SC <- vector("list", length(providers) )
  names(DC) <- names(SC) <- providers

  for (j in providers) {
    probs   <- probDeath[[country]]
    ac      <- AC[[country]][[j]]
    ratio   <- ratios[[j]]
    res     <- data.frame(sex = rep(c("Men","Women"), each = 101), age = 0:100)
    ac      <- ac[,1] # The first column is always the total (= all diseases)
    r       <- ratio[,3:17] # The first two columns are "sex" and "age"
    probs   <- probs[,rep(1:5,3)]
    denom   <- (r - 1) * probs
    denom   <- cbind(mean = rowSums(denom[,1:5], na.rm = TRUE), # 1:5 = 5 years of TTD
                     lower = rowSums(denom[,6:10], na.rm = TRUE),
                     upper = rowSums(denom[,11:15], na.rm = TRUE))
    denom  <- 1 + denom
    #  denom  <- f.replacespecial(denom,repif=repif)
    sc     <- ac / denom

    # #Smooth out sc
    mean  <- "mean"
    lower <- "upper"
    upper <- "lower"
    # nk             <<- 10
    # lastsexage     <<- 101
    # v.age          <<- 0:100
    # sc[,mean]  <- ifelse(sc[,mean]<0,1,sc[,mean])
    # sc[,lower] <- ifelse(sc[,lower]<0,1,sc[,lower])
    # sc[,upper] <- ifelse(sc[,upper]<0,1,sc[,upper])
    # smooth_mean        <- f.smoothAC(sc[,mean], inc=1)
    # smooth_lower <- sc[,lower] <- f.smoothAC(sc[,lower], inc=1)
    # smooth_upper <- sc[,upper] <- f.smoothAC(sc[,upper], inc=1)
    # sc[,mean] <- pmin(pmax(smooth_mean, smooth_lower), smooth_upper)
    sc     <- sc[,c(mean, lower, upper)]
    dc     <- r * sc[,rep(1:3, each = 5)]

    sc     <- sc[,c(mean,lower,upper)]
    colnames(sc) <- c("mean","lower","upper")

    SC[[j]] <- sc
    DC[[j]] <- dc
  }

  return(list(SC = SC,DC = DC))
}

scdc <- vector("list",5)
names(scdc) <- all.country

for (cntry in all.country) {
    AC  <<- ac
    scdc[[cntry]] <- getSCDC(cntry,providers = available.providers[[cntry]], repif = 0)

}


##### GENERATE MAPPING DIAG ####

# Every country must have a mapping > diag file even if empty, as it allows for easier update is the country gains any COI

mapping <- vector("list",length(all.country))
names(mapping) <- all.country

for (ctry in all.country) {
  mapping[[ctry]]      <- readxl::read_excel(paste("inst/extdata/",sub(" ","",ctry),"/mapping.xlsx",sep=""), sheet = "diag",.name_repair = "unique_quiet")
}

# Make some available to users
related.costs.scdc <- list(sc = rep(0,202),dc = rep(0,202))
names(related.costs.scdc$sc) <- names(related.costs.scdc$dc) <- paste(rep(c("Men","Women"), each=101),0:100,sep = "_")
related.costs.ac <- related.costs.scdc$sc

usethis::use_data(mort, probDeath, pop, ratios, notratios, tc, ac, scdc, all.country,
                  probDeathCause, probDeathChapters,mapping, available.costmethods, available.providers, internal = TRUE, overwrite = TRUE)

usethis::use_data(mapping, available.costmethods, available.providers, related.costs.scdc, related.costs.ac, overwrite = TRUE)
