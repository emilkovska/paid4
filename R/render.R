
#' Generates unrelated survivor and decedent costs
#' Adjusts spending by time to death with the probability to NOT die of either related diseases conditional on death at age a. Then sums op the spending across selected providers.
#'
#' @param country Takes the same `country` as the main function pai4::paid
#' @param user.providers a character vector. Takes the same `user.providers` as the main function pai4::paid
#' @param user.diseases a data.frame of related diseases. See `related.diseases`in the main function pai4::paid
#'
#' @export
renderUnrelSCDC <- function(country, user.providers, user.diseases) {

  # Setup
  map <- mapping[[country]]

  if (country=="Germany") {
    names(map) <- c("code","group","chapter","description","header","tree")
  } else {
    names(map) <- c("code","group","chapter","description","header")
  }

  if (any(available.costmethods[[country]] %in% "dscosts")) {
    probCause    <- probDeathCause[[country]] # These are p(x|d=1,a,s) and must stay so!
    probChapters <- probDeathChapters[[country]]
  }

  probAll      <- probDeath[[country]]
  var.vector   <- c(paste("mean",0:4,sep="_"),paste("lower",0:4,sep="_"),paste("upper",0:4,sep="_"))

  nprov <- length(user.providers)
  list_res_dc <- list_res_sc <- vector(mode = "list", length = nprov)
  names(list_res_dc) <- names(list_res_sc) <- user.providers

  # Set related diseases, if any
  if (any(user.diseases$bool==FALSE)) {

    related.dis     <- as.numeric(unlist(user.diseases$Group[user.diseases$bool==FALSE]))
    related.dis     <- unlist(map$code[map$group %in% related.dis])
    names(related.dis) <- apply(as.matrix(related.dis),1,FUN = function(x) {map$header[map$code == x]})
    related.chapter <- unique(map$header[map$code %in% related.dis])
    # If any chapters are from the 100 category, change the name to 100 (to match ratio file)
    related.chapter[!(related.chapter %in% map$header[map$chapter!=100])] <- "100"
    related.chapter <- unique(related.chapter)


    # This part for Germany is to account for the fact that costs (and COD) have overlapping categories.
    # One can deselect "Malignant neoplasms" AND "Malignant neoplasms of stomach" and if I take the same approach as for NL, then there's double-counting in removing related diseases
    if (country=="Germany") {
      related.branch <- map$tree[map$code %in% related.dis]
      # Find the nearest parent group of each branch
      names(related.branch) <-  sapply(related.branch, function(x) {
        parent  <- f.left(x,stringr::str_length(x)-1)
        r.name  <- map$tree[map$tree %in% parent]
        ifelse(length(r.name)!=0,r.name,"Header")
      })
      # If user (de)selected both a branch and a tree, take only the nearest tree
      related.branch <- related.branch[!(names(related.branch) %in% related.branch)]
      # Get the codes for the corrected disease selection
      related.dis    <- map$code[map$tree %in% related.branch]
    }

    # Get 1-p(x,y,z|d=1,a,s) - probability to NOT die of either related diseases conditional on death at age a
    # Select only those diseases that have cause of death coded
    disnames <- related.dis[related.dis %in% colnames(probCause)]

    p_x      <- as.matrix(probCause[,disnames])
    p_x      <- rowSums(p_x[,,drop=FALSE], na.rm = TRUE)
    p_nox   <- 1-p_x
    p_nox   <- cbind(x_0 = p_nox,
                     x_1 = c(dplyr::lead(p_nox[1:101]),dplyr::lead(p_nox[102:202])),
                     x_2 = c(dplyr::lead(p_nox[1:101], n=2),dplyr::lead(p_nox[102:202],n=2)),
                     x_3 = c(dplyr::lead(p_nox[1:101], n=3),dplyr::lead(p_nox[102:202],n=3)),
                     x_4 = c(dplyr::lead(p_nox[1:101], n=4),dplyr::lead(p_nox[102:202],n=4)))
    p_nox   <- p_nox[,rep(1:5,3)]
  }

  for (j in user.providers) {

    sc  <- scdc[[country]][["SC"]][[j]]
    dc  <- scdc[[country]][["DC"]][[j]]

    if (any(user.diseases$bool==FALSE)) {

      AC  <- ac[[country]][[j]]
      R   <- notratios[[country]][[j]]
      R   <- R[,-c(1,2)]

      # Expand the frames
      sc      <- sc[,rep(1:3,each= 5)]

      # Calculate d(nox)
      # Get only the related chapter ratios
      R_nox <- lapply(related.chapter, FUN = function(x) {R[,grepl(x,names(R))]})
      R_nox <- do.call(cbind,R_nox)
      # Get weights for R(NOX)
      wR_nox <- as.matrix(1-probChapters[,related.chapter])
      wR_nox <- wR_nox / rowSums(wR_nox[,,drop=FALSE])
      wR_nox <- wR_nox[,rep(1:length(related.chapter), each = 15)]
      # Get weighted R(NOX)
      R_nox  <- R_nox * wR_nox
      R_nox <-  lapply(var.vector,FUN = function(x) {
        sumnames <- names(R_nox)[grepl(x,names(R_nox))]
        rowSums(R_nox[,sumnames,drop=FALSE], na.rm = TRUE)
      })
      names(R_nox) <- var.vector
      R_nox        <- do.call(cbind,R_nox)

      # Calculate unrelated dc
      dc_unrel <- R_nox * sc * p_nox
      colnames(dc_unrel) <- colnames(R_nox)

      dc_unrel[is.na(dc_unrel)] <- 0
      dc[is.na(dc)] <- 0

      dc_unrel <- as.matrix(dc_unrel)
      dc       <- as.matrix(dc)
      dc_unrel[dc_unrel > dc] <- dc[dc_unrel > dc]

      # Calculate sc_unrel
      # The first column of each ac is the total
      ac_unrel <- as.matrix(AC[,1] - rowSums(AC[,related.dis,drop=FALSE], na.rm = TRUE))
      ac_unrel <- ac_unrel[,rep(1,3)]
      num      <- dc_unrel * probAll[,rep(1:5,3)]
      colnames(num) <- colnames(dc_unrel)

      num <- cbind(mean  = rowSums(num[,grepl("mean",colnames(num))], na.rm = TRUE),
                   lower = rowSums(num[,grepl("lower",colnames(num))], na.rm = TRUE),
                   upper = rowSums(num[,grepl("upper",colnames(num))], na.rm = TRUE))

      sc_unrel <- (ac_unrel - num) / (1-rowSums(probAll, na.rm = TRUE))

      # Ensure no negatives
      sc_unrel <- f.replacespecial(sc_unrel, repif = 0)
      sc_unrel <- sc_unrel[,c(1,3,2)]
      colnames(sc_unrel) <- c("mean","lower","upper")

      # At older ages the probability to die in the text five years is close to 1, meaning that sc_unrel costs will be inflated exponentially because
      # (1-rowSums(probAll, na.rm = TRUE)) --> 0
      # Since we assume that all die after 100, survivor costs after 95 do not matter, as they are never used for the LHCE calculation, setting them to 0
      sc_unrel[c(97:101,97:101+101),] <- 0

      sc_unrel <- as.matrix(sc_unrel)
      sc       <- as.matrix(sc[,c("mean","lower","upper")])

      sc_unrel[sc_unrel > sc] <- sc[sc_unrel > sc]

      # Record
      list_res_dc[[j]] <- dc_unrel
      list_res_sc[[j]] <- sc_unrel

    } else {
      # Record
      list_res_dc[[j]] <- dc
      list_res_sc[[j]] <- sc

    }
  }

  sc_unrel <- Reduce("+",list_res_sc)
  dc_unrel <- Reduce("+",list_res_dc)

  return(list(sc = sc_unrel,
              dc = dc_unrel))
}


#' Generate unrelated lifetime healthcare spending
#' Takes the survivor and decedent costs by age, removes any spending if specified by user, and combines into lifetime spending.
#'
#' @param country  Takes the same `country` as the main function pai4::paid
#' @param user.providers a character vector. Takes the same `user.providers` as the main function pai4::paid
#' @param user.diseases a data.frame of related diseases. See `related.diseases`in the main function pai4::paid
#' @param disc_percentage Takes the same `disc_percentage` as the main function pai4::paid
#' @param user.uploaded Takes the same `user.uploaded` as the main function pai4::paid
#'
#' @return list
#' @export
renderLHCE <- function(country, user.providers, user.diseases, disc_percentage, user.uploaded) {

  # Render unrelated dc and sc costs
  unrel          <- renderUnrelSCDC(country = country, user.providers = user.providers, user.diseases = user.diseases)
  sc             <- as.matrix(unrel[["sc"]])
  dc             <- as.matrix(unrel[["dc"]])
  rm(unrel)

  var.vec <- c("mean","lower","upper")

  sex  <- list(1:101,102:202)
  lhce <- array(NA, dim = c(202,101,3), dimnames = list(start = rep(0:100,2), end = 0:100,var.vec))
  dc[c(101,202),!grepl("_0",colnames(dc))] <- NA
  dc[c(100,201),!(grepl("_0",colnames(dc)) | grepl("_1",colnames(dc)))] <- NA
  dc[c(99,200),!(grepl("_0",colnames(dc)) | grepl("_1",colnames(dc)) | grepl("_2",colnames(dc)))] <- NA
  dc[c(98,199), grepl("_4",colnames(dc))] <- NA

  for (v in var.vec) {
    for (s in 1:2) {
      for (i in sex[[s]]) {
        lastrow <- switch(paste(s), "1" = 101, "2" = 202)
        j       <- switch(paste(s), "1" = i, "2" = i - 101)
        t       <- 1:length(i:lastrow)
        discR   <- 1/(1+disc_percentage/100)^t
        dx      <- f.sumdiag(dc[,grepl(v,colnames(dc))], from = i, to = lastrow, disc_vector = discR) # Get the sum of the last 5 years of life at each age i to 100
        dx      <- stats::na.omit(dx)
        sx      <- sc[i:lastrow,v] * discR
        sx      <- dplyr::lag(sx, n = 5)
        sx[is.na(sx)]   <- 0
        sx      <- cumsum(sx)
        lhce[i,j:101,v] <- sx + dx
      }
    }
  }

  if (!is.null(user.uploaded)) {

    user_lhce <- matrix(NA, nrow = 202,ncol = 101)

    if (inherits(user.uploaded,"list")) {

      sc <- user.uploaded[["sc"]]
      dc <- user.uploaded[["dc"]]

      for (s in 1:2) {
        for (i in sex[[s]]) {
          lastrow        <- 101 + 101*(s-1)
          j              <- switch(paste(s), "1" = i, "2" = i - 101)
          sx             <- sc[i:lastrow]
          sx             <- dplyr::lag(sx)
          sx[is.na(sx)]  <- 0
          sx             <- cumsum(sx)
          dx             <- dc[i:lastrow]
          user_lhce[i,j:101] <- sx + dx
        }
      }
    } else {
      ac <- user.uploaded
      for (s in 1:2) {
        for (i in sex[[s]]) {
          lastrow        <- 101 + 101*(s-1)
          j              <- switch(paste(s), "1" = i, "2" = i - 101)
          ax             <- cumsum(ac[i:lastrow])
          user_lhce[i,j:101] <- ax
        }
      }
    }

    user_lhce <- array(user_lhce, dim = c(202,101,3), dimnames = list(start = rep(0:100,2), end = 0:100,var.vec))
    lhce <- lhce - user_lhce
  }

  res <- list(lhce = lhce, sc = sc, dc = dc)

  return(res)

}


#' Combines lifetime spending with survival data
#'
#' @param survdata Takes the same `survdata` as the main function pai4::paid
#' @param costlist Takes the `lhce` output from renderLHCE
#' @param pmen     Takes the same `pmen` as the main function pai4::paid
#' @param cycle_length Takes the same `cycle_length` as the main function pai4::paid
#' @param start_age Takes the same `start_age` as the main function pai4::paid
#'
#' @return a list
renderCohort <- function(survdata,costlist, pmen, cycle_length, start_age) {

  var.vec    <- c("mean","lower","upper")
  sex.vec    <- c("Men","Women")
  nrows      <- dim(survdata)[1]

  costlist <- list(Men   = stats::na.omit(costlist[start_age+1,,]),
                   Women = stats::na.omit(costlist[start_age+102,,]))

  # Generate cost vector in accordance with user cycle-length
  if (cycle_length!=1) {
    user_agevec   <- rep(cycle_length,nrows-1)
    user_agevec   <- c(0,user_agevec)
    user_agevec   <- t <- cumsum(user_agevec)
    user_agevec   <- user_agevec + start_age
    last_user_age <- round(user_agevec[nrows])
    user_agevec   <- user_agevec + cycle_length/2
    orig_agevec   <- start_age:last_user_age
    # The original costs consider costs accrued towards the end of the interval. But with a shorter cycle, the start of the interval is needed.
    # For example, with a start_age=20 and 3-week cycle length, the original file will give costs at age 20 of approx 30000.
    # But that 30000 is the costs of the last year of life of a 20-year-old if they'd died at age 20.999.
    # So, starting from age 20 the accrued costs should be 0 (20) to 30000 (20.999).
    # Half-cycle correction would be finding the LHCE at the age between interval 1 & interval 2 (thus, xout = user_agevec + cycle_length/2)
    costlist <-   lapply(sex.vec,FUN = function(s) {
      sapply(var.vec, FUN = function(v) {
        f0   <- stats::approx(x = c(orig_agevec + 0.9999), y = costlist[[s]][paste(orig_agevec),v], xout = user_agevec)$y
        data <- data.frame(x = user_agevec, y = f0)
        pred <- unlist(stats::predict(gam(y ~ s(x, bs = "ps", k = 15),family = "gaussian", data), newdata = data.frame(x = user_agevec[is.na(f0)])))
        pred <- c(pred,f0[!is.na(f0)])
        names(pred) <- NULL
        pred
      })
    })
  } else {
    # Half-cycle correction
    costlist <- lapply(sex.vec, function(s) {
      new <- rbind(c(0,0,0),costlist[[s]])
      new <- (new + dplyr::lead(new))/2
      new <- stats::na.omit(new)
      rownames(new) <- 0:100
      costlist[[s]] <- new
    })
    names(costlist) <- sex.vec
    t               <- 1:length(start_age:100)
  }

  names(costlist) <- sex.vec

  # Get sex-weighted costs
  uwcosts <- costlist
  costlist <- costlist[["Men"]]*pmen + (1-pmen)*costlist[["Women"]]

  # Get survival and number of people dying each cycle
  cohort_f   <- as.data.frame(sapply(c(1,2),function(x) {dplyr::lag(survdata[,x])-survdata[,x]}))
  cohort_f[is.na(cohort_f)]  <- 0

  # Calculate unrelated costs
  coh.output        <- lapply(1:2,function(x) {costlist * cohort_f[,x]})
  names(coh.output) <- colnames(survdata)

  return(list(output  = coh.output, # Survival * Discounted, half-cycle corrected, sex-weighted costs
              wcosts  = costlist,   # Discounted, half-cycle corrected, sex-weighted unrelated costs
              uwcosts = uwcosts))   # Discounted, half-cycle corrected unrelated costs for men & women separately in the pre-specified cycle length.
}
