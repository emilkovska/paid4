
#' Generates unrelated survivor and decedent costs
#' Adjusts spending by time to death with the probability to NOT die of either related diseases conditional on death at age a. Then sums op the spending across selected providers.
#'
#' @param country Takes the same `country` as the main function [paid4::paid()]
#' @param user.providers a character vector. Takes the same `user.providers` as the main function [paid4::paid()]
#' @param user.diseases a data.frame of related diseases. See `related.diseases`in the main function [paid4::paid()]
#' @param PSA logical takes the same value as `PSA` as the main function [paid4::paid()]
#' @param coi.year  Cost of illness year. Taken from main function.
#' @param psa.N  number of draws. Taken from main function.
#'
#' @export
renderUnrelSCDC <- function(country, user.providers, user.diseases, coi.year, PSA, psa.N) {

  # Setup
  map <- mapping[[country]][[paste(coi.year)]]

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

  # Set related diseases, if any & calculate p_nox
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
    
    disnames <- apply(sapply(related.dis, function(x) {grepl(x,colnames(probCause))}), 2, function(xx) {colnames(probCause)[xx]} )
    if (is.list(disnames)) {
      disnames <- do.call(c,disnames)
    } 
    disnames <- unique(disnames)
    p_x      <- as.matrix(probCause[,disnames])
    p_x      <- apply(p_x[,,drop = FALSE],2,as.numeric)
    p_x      <- rowSums(p_x[,,drop=FALSE], na.rm = TRUE)
    p_nox   <- 1-p_x
    p_nox   <- cbind(x_0 = p_nox,
                     x_1 = c(dplyr::lead(p_nox[1:101]),dplyr::lead(p_nox[102:202])),
                     x_2 = c(dplyr::lead(p_nox[1:101], n=2),dplyr::lead(p_nox[102:202],n=2)),
                     x_3 = c(dplyr::lead(p_nox[1:101], n=3),dplyr::lead(p_nox[102:202],n=3)),
                     x_4 = c(dplyr::lead(p_nox[1:101], n=4),dplyr::lead(p_nox[102:202],n=4)))
   
    # Expand p_nox
     if (!PSA) {
       p_nox   <- p_nox[,rep(1:5,3)]
     } else {
       p_nox   <- array(matrix(p_nox, nrow=202, ncol=5), dim = c(202,5,psa.N)) 
     }
    
  }
  
  if (PSA) {a.scdc <- getSCDC(country,providers = user.providers, coi.year = coi.year, PSA = TRUE, psa.N = psa.N)}

  for (j in user.providers) {
  #  browser()
    
    AC  <- ac[[country]][[paste(coi.year)]][[j]]
    
    if (PSA) {
      a.sc   <- a.scdc[["SC"]][[j]]
      a.dc   <- a.scdc[["DC"]][[j]]
      
      if (any(user.diseases$bool==FALSE)) {
        
        R   <- notratios[[country]][[paste(coi.year)]][["PSA"]][[j]]
        R   <- R[,-c(1,2)]
        rownames(R) <- NULL
        # Get only the related chapter ratios
        R_nox <- lapply(related.chapter, FUN = function(x) {R[,grepl(x,names(R))] })
        R_nox <- do.call(cbind,R_nox)
        # Get weights for R(NOX)
        wR_nox <- as.matrix(1-apply(probChapters[,related.chapter, drop = FALSE],2,as.numeric))
        wR_nox <- wR_nox / rowSums(wR_nox[,,drop=FALSE])
        
        # Draw notratios for related chapters & Get weighted R(NOX)
        a.dimnames  <- list(paste(rep(c("Men","Women"),each=101),0:100,sep="_"),
                            paste("mean",0:4,sep="_"),
                            NULL) 
        a.temp      <- array(NA, dim = c(202,5,psa.N), 
                             dimnames = a.dimnames)
        a.loop      <- names(R_nox)[grepl("logmean",names(R_nox))]
        counter=1
        for (chap in related.chapter) {
          v.loop <- a.loop[grepl(chap, a.loop)]
          for (t in v.loop) {
            par1  <- R_nox[,t]
            par2  <- R_nox[,sub("logmean","sd",t)]
            tsave <- f.mid(t,4,6)
            wname <- sapply(colnames(wR_nox), function(x) grepl(x,t))
            a.temp[,tsave,] <- t(mapply(f.draw, par1, par2, N = psa.N)) * wR_nox[,wname]
          }
          
          if (counter==1) {
            a.R_nox <- a.temp
          } else {
            a.R_nox <- a.R_nox + a.temp
          }
          counter = counter + 1
        }
        
        # Calculate unrelated dc & ensure no negatives
        dc_unrel <- a.R_nox * a.sc * p_nox
        dc_unrel <- array(apply(dc_unrel,3,f.replacespecial, repif = 0), dim = c(202,5,psa.N))
        a.dc     <- array(apply(a.dc,3,f.replacespecial, repif = 0), dim = c(202,5,psa.N))
        
        #temps 
        dcu_temp <- dc_unrel
        dcu_temp[is.na(dcu_temp)] <- 0
        dc_temp  <- a.dc
        dc_temp[is.na(dc_temp)] <- 0
        
        # Sub
        dc_unrel[dcu_temp > dc_temp] <- a.dc[dcu_temp > dc_temp]
        
        # Calculate sc_unrel
        # The first column of each ac is the total
        ac_unrel <- as.matrix(AC[,1] - rowSums(AC[,related.dis,drop=FALSE], na.rm = TRUE))
        
        # Expand ac_unrel & probAll
        ac_unrel <- array(matrix(ac_unrel, nrow=202, ncol=1), dim = c(202,1,psa.N)) 
        probAll  <- array(probAll, dim = c(202,5,psa.N)) 
        num      <- dc_unrel * probAll
        num      <- array(apply(num,3,rowSums, na.rm = TRUE), dim = c(202,1,psa.N))
        probAll  <- array(apply(probAll,3,rowSums), dim = c(202,1,psa.N))
        
        sc_unrel <- (ac_unrel - num) / (1-probAll)
   
        # At older ages the probability to die in the text five years is close to 1, meaning that sc_unrel costs will be inflated exponentially because
        # (1-rowSums(probAll, na.rm = TRUE)) --> 0
        
        # Retract a.sc - all columns have the same value, so a rowMeans just gives the same value in 1 column
        a.sc <- array(apply(a.sc,3,rowMeans), dim = c(202,1,psa.N))
        
        # Validate unrelated sc
        # Ensure no negatives
        sc_unrel <- array(apply(sc_unrel,3,f.replacespecial, repif = 0), dim = c(202,1,psa.N))
        # Ensure unrelated sc are not greater than regular sc
        sc_unrel[sc_unrel > a.sc] <- a.sc[sc_unrel > a.sc]
        
        # Record
        list_res_dc[[j]] <- dc_unrel
        list_res_sc[[j]] <- sc_unrel
        
      } else {
        # Retract a.sc - all columns have the same value, so a rowMeans just gives the same value in 1 column
        a.sc <- array(apply(a.sc,3,rowMeans), dim = c(202,1,psa.N))
        # Record
        list_res_dc[[j]] <- a.dc
        list_res_sc[[j]] <- a.sc
      }
      
      
    } else {
      # No PSA
      sc  <- scdc[[country]][[paste(coi.year)]][["SC"]][[j]]
      dc  <- scdc[[country]][[paste(coi.year)]][["DC"]][[j]]
      if (any(user.diseases$bool==FALSE)) {
 
        R   <- notratios[[country]][[paste(coi.year)]][["Deterministic"]][[j]]
        R   <- R[,-c(1,2)]
        rownames(R) <- NULL
  
        # Expand the frames
        sc      <- sc[,rep(1:3,each= 5)]
  
        # Calculate d(nox)
        # Get only the related chapter ratios
        R_nox <- lapply(related.chapter, FUN = function(x) {R[,grepl(x,names(R))]})
        R_nox <- do.call(cbind,R_nox)
        # Get weights for R(NOX)
        wR_nox <- as.matrix(1-apply(probChapters[,related.chapter, drop = FALSE],2,as.numeric))
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
  #      sc_unrel <- sc_unrel[,c(1,3,2)]
        colnames(sc_unrel) <- c("mean","lower","upper")
  
        # At older ages the probability to die in the text five years is close to 1, meaning that sc_unrel costs will be inflated exponentially because
        # (1-rowSums(probAll, na.rm = TRUE)) --> 0
        # Since we assume that all die after 100, survivor costs after 95 do not matter, as they are never used for the LHCE calculation, setting them to 0
  #      sc_unrel[c(97:101,97:101+101),] <- 0
  
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
  }

  
  sc_unrel <- Reduce("+",list_res_sc)
  dc_unrel <- Reduce("+",list_res_dc)

  return(list(sc = sc_unrel,
              dc = dc_unrel))
}


#' Generate unrelated lifetime healthcare spending
#' Takes the survivor and decedent costs by age, removes any spending if specified by user, and combines into lifetime spending.
#'
#' @param country  Takes the same `country` as the main function [paid4::paid()]
#' @param user.providers a character vector. Takes the same `user.providers` as the main function [paid4::paid()]
#' @param user.diseases a data.frame of related diseases. See `related.diseases`in the main function [paid4::paid()]
#' @param disc_percentage Takes the same `disc_percentage` as the main function [paid4::paid()]
#' @param user.uploaded Takes the same `user.uploaded` as the main function [paid4::paid()]
#' @param coi.year  Cost of illness year. Taken from main function.
#' @param PSA  logical. Taken from main function.
#' @param psa.N  number of draws. Taken from main function.
#'
#' @return list
#' @export
renderLHCE <- function(country, user.providers, user.diseases, disc_percentage, user.uploaded, coi.year, PSA, psa.N ) {
  
  # Render unrelated dc and sc costs
  unrel          <- renderUnrelSCDC(country = country, user.providers = user.providers, 
                                    user.diseases = user.diseases, coi.year = coi.year, PSA, psa.N = psa.N)
  
  # Combine into discounted LHCe
  sex     <- list(1:101,102:202)
  v.disc  <- f.discount(x = disc_percentage)
  
  if (PSA) {
      sc <- unrel[["sc"]]
      dc <- unrel[["dc"]]
      rm(unrel)
      lhce    <- array(NA, dim = c(202,101,psa.N), dimnames = list(start = rep(0:100,2), end = 0:100,NULL))
      
      pb      <- txtProgressBar(min = 1, max = 202, style = 3)
      for (s in 1:2) {
        for (i in sex[[s]]) {
            setTxtProgressBar(pb, i)
            lastrow <- switch(paste(s), "1" = 101, "2" = 202)
            j       <- switch(paste(s), "1" = i, "2" = i - 101)
            t       <- seq_along(i:lastrow)
            N       <- length(t)
            DF      <- 1/(1+v.disc[t])^t
            dx      <- array(apply(dc,3,f.sumdiag, from = i, to = lastrow, disc_vector = DF), dim = c(N,1,psa.N))
            sub     <- array(sc[i:lastrow,,], dim = c(N,1,psa.N))
            sx      <- array(
              apply(sub,3,function(x) {
                sx0 <- x*DF
                sx1 <- dplyr::lag(sx0, n = 5)
                sx1[is.na(sx1)]   <- 0 
                cumsum(sx1)
              }),
              dim = c(N,1,psa.N)
              
            )

            lhce[i,j:101,] <- sx + dx
            
        }
      }
      close(pb)
      
  } else {
      var.vec <- c("mean","lower","upper")
      sc <- as.matrix(unrel[["sc"]])
      dc <- as.matrix(unrel[["dc"]])
      rm(unrel)
      lhce    <- array(NA, dim = c(202,101,3), dimnames = list(start = rep(0:100,2), end = 0:100,var.vec))
    
    for (v in var.vec) {
      for (s in 1:2) {
        for (i in sex[[s]]) {
          lastrow <- switch(paste(s), "1" = 101, "2" = 202)
          j       <- switch(paste(s), "1" = i, "2" = i - 101)
          t       <- seq_along(i:lastrow)
          DF      <- 1/(1+v.disc[t])^t
          dx      <- f.sumdiag(dc[,grepl(v,colnames(dc))], from = i, to = lastrow, disc_vector = DF) # Get the sum of the last 5 years of life at each age i to 100
          sx      <- sc[i:lastrow,v] * DF
          sx      <- dplyr::lag(sx, n = 5)
          sx[is.na(sx)]   <- 0
          sx      <- cumsum(sx)
          lhce[i,j:101,v] <- sx + dx
        }
      }
    }

  }      
      
  if (!is.null(user.uploaded)) {
  

    user_lhce <- matrix(NA, nrow = 202,ncol = 101)

    if (inherits(user.uploaded,"list")) {

      SC <- user.uploaded[["sc"]]
      DC <- user.uploaded[["dc"]]

      for (s in 1:2) {
        for (i in sex[[s]]) {
          lastrow        <- 101 + 101*(s-1)
          j              <- switch(paste(s), "1" = i, "2" = i - 101)
          t              <- seq_along(i:lastrow)
          DF             <- 1/(1+v.disc[t])^t
          sx             <- SC[i:lastrow] * DF
          sx             <- dplyr::lag(sx)
          sx[is.na(sx)]  <- 0
          sx             <- cumsum(sx)
          dx             <- DC[i:lastrow] * DF
          user_lhce[i,j:101] <- sx + dx
        }
      }
    } else {
      ac <- user.uploaded
      for (s in 1:2) {
        for (i in sex[[s]]) {
          lastrow        <- 101 + 101*(s-1)
          j              <- switch(paste(s), "1" = i, "2" = i - 101)
          t              <- seq_along(i:lastrow)
          DF             <- 1/(1+v.disc[t])^t
          ax             <- ac[i:lastrow] * DF
          ax             <- cumsum(ax)
          user_lhce[i,j:101] <- ax
        }
      }
    }

    user_lhce <- array(user_lhce, dim = dim(lhce), dimnames = list(start = rep(0:100,2), 
                                                                   end = 0:100, 
                                                                   switch(paste(PSA), "TRUE" = NULL, "FALSE" = c("mean","lower","upper"))
                                                                   )
                       )
    lhce      <- lhce - user_lhce
  }
  
  if (!PSA) {
    for (i in 1:202) {
      lower <- lhce[i,,"lower"]
      upper <- lhce[i,,"upper"]
      lhce[i,,"lower"] <-  pmin(lower, upper)
      lhce[i,,"upper"] <-  pmax(lower, upper)
    }
  }

  res <- list(lhce = lhce, sc = sc, dc = dc)

  return(res)

}




#' Combines lifetime spending with survival data
#'
#' @param survdata Takes the same `survdata` as the main function [paid4::paid()]
#' @param costlist Takes the `lhce` output from renderLHCE
#' @param pmen     Takes the same `pmen` as the main function [paid4::paid()]
#' @param cycle_length Takes the same `cycle_length` as the main function [paid4::paid()]
#' @param start_age Takes the same `start_age` as the main function [paid4::paid()]
#' @param justcosts logical. Not used in the package, was added for the Shiny App.
#' @param PSA  logical. Taken from main function.
#' @param psa.N  number of draws. Taken from main function.
#'
#' @return a list
#' @export
renderCohort <- function(survdata,costlist, pmen, cycle_length, start_age, justcosts = FALSE, PSA, psa.N) {

  var.vec    <- switch(paste(PSA), "TRUE" = 1:psa.N, "FALSE" = c("mean","lower","upper"))
  sex.vec    <- c("Men","Women")
  nrows      <- dim(survdata)[1]

  costlist <- list(Men   = stats::na.omit(costlist[round(start_age)+1,,]),
                   Women = stats::na.omit(costlist[round(start_age)+102,,]))
  
  # Generate cost vector in accordance with user cycle-length
  user_agevec   <- rep(cycle_length,nrows-1)
  user_agevec   <- c(0,user_agevec)
  user_agevec   <- t <- cumsum(user_agevec)
  user_agevec   <- user_agevec + start_age
  
  # Extend costlist in case cohort lives beyond 100 years.
  if (any(round(user_agevec)>100)) {
    add_ages <- unique(round(user_agevec)[round(user_agevec)>100])
    costlist <- lapply(costlist,function(x) {
      repn <- rep(which(rownames(x)==100),length(add_ages))
      repx <- matrix(x[repn,], ncol=length(var.vec))
      rownames(repx) <- add_ages
      rbind(x,repx)
      })
  }

  
  if (cycle_length!=1) {
    orig_agevec   <- unique(round(user_agevec))
    user_agevec   <- user_agevec + cycle_length/2
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
      bindwith <- switch(paste(PSA), "TRUE" = rep(0,psa.N), "FALSE" = rep(0,3))
      new <- rbind(bindwith,costlist[[s]])
      new <- (new + dplyr::lead(new))/2
      new <- stats::na.omit(new)
      rownames(new) <- user_agevec
      costlist[[s]] <- new
    })
    names(costlist) <- sex.vec
    t               <- 1:length(start_age:100)
  }

  names(costlist) <- sex.vec

  # Get sex-weighted costs
  uwcosts  <- costlist
  costlist <- costlist[["Men"]]*pmen + (1-pmen)*costlist[["Women"]]

  if (justcosts) {
    return(costlist)
  }

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



#' Disaggregate per-capita spending
#'
#' Attributes costs to survivors and decedents. 
#' 
#' @param country a string
#' @param providers a string vector
#' @param coi.year  year as taken from main function.
#' @param PSA  logical. Taken from main function.
#' @param psa.N  number of draws. Taken from main function.
#'
#' @return a list
#' @export
getSCDC <- function(country, providers, coi.year, PSA, psa.N) {
  
  DC <- SC <- vector("list", length(providers) )
  names(DC) <- names(SC) <- providers
  psa <- ifelse(PSA,"PSA","Deterministic")
  
  for (j in providers) {
    probs   <- probDeath[[country]]
    avrc    <- ac[[country]][[paste(coi.year)]][[j]]
    ratio   <- ratios[[psa]][[j]]
    r       <- ratio[,3:ncol(ratio)] # The first two columns are "sex" and "age"
    avrc    <- avrc[,1] # The first column is always the total (= all diseases)
    
    
    if (PSA) {
      a.dimnames <- list(paste(rep(c("Men","Women"),each=101),0:100,sep="_"),
                        paste("mean",0:4,sep="_"),
                        NULL) 
      a.ratio <- array(NA, dim = c(202,5,psa.N), 
                               dimnames = a.dimnames)
      for (t in 1:5) {
        par1 <- r[,t]
        par2 <- r[,t+5]
        a.ratio[,t,] <- t(mapply(f.draw, par1, par2, N = psa.N))
      }
      
      # Calculate SC
      a.sc <- array(
        apply(a.ratio, 3, function(x) {
          denom <- (x - 1) * probs
          denom <- rowSums(denom,na.rm = TRUE) + 1
          res   <- avrc / denom
          matrix(res, nrow = 202, ncol = 5 )
          }),
        dim = c(202,5,psa.N),
        dimnames = a.dimnames
        )
      
      # Back-calculate DC
      a.dc <- a.sc * a.ratio
      
      SC[[j]] <- a.sc
      DC[[j]] <- a.dc
      
      
    } else {
      
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

  }
  
  return(list(SC = SC,DC = DC))
}

