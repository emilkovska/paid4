

#' Helper function to set the same value at low and high ages
#'
#'
#' @param d  A numeric vector of length 202. First 101 are for men.
#' @param a1 An integer defining the first location of the lowest age.
#' @param a2 An integer defining the first location of the highest age.
#'
#' @return A numeric vector
#' @export
f.lowhighages <- function(d,a1=51,a2=96){
  d[1:a1]         <-d[a1]
  d[a2:101]       <-d[a2]
  d[102:(101+a1)] <-d[101+a1]
  d[(101+a2):202] <-d[101+a2]
  d
}

#' Extract the first characters from text
#'
#' This is the equivalent of Excel LEFT function.
#'
#' @param text a character vector
#' @param num_char an integer or an integer vector
#'
#' @return a character vector
#' @export
#'
#' @examples
#' f.left("This text", 3)
#' f.left(c("This text","That text"), c(3,4))
f.left = function(text, num_char) {
  substr(text, 1, num_char)
}

#' Extract from the middle of text
#'
#' This is the equivalent of Excel MID function.
#'
#' @param text a character vector
#' @param start_num an integer denoting the start
#' @param num_char  an integer denoting how many characters to extract
#'
#' @return a character vector
#' @export
#'
#' @examples
#' f.mid("This text", 6,2)
f.mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}


#' Extract from the right of text
#'
#' This is the equivalent of RIGHT MID function.
#'
#' @param text a character vector
#' @param num_char an integer or an integer vector
#'
#' @return a character vector
#' @export
f.right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

#' Smooths out any vector into 1-year age groups
#'
#' This package assumes throughout that data for men and women are stacked and are all thus sized 2*N, where N is the length of the vector for men.
#' A Generalized additive model with P-splines is used for the smoothing.
#'
#' @param dep numerical vector where the first N values are for men and the next N are for women.
#' @param inc ensures a small increment is added to any value that is a perfect 0. This ensures correct log transformation.
#' @param lastsexage location within dep vector of the last age group for men.
#'
#' @return numerical vector of length 202
#' @export
f.smoothAC <- function(dep,inc = 1, lastsexage) {
  dep[is.na(dep)] <- 0

  d.pred <- data.frame(v.age=(0:100+1) - 0.5)
  dep1  <- log(dep[1:lastsexage]+inc)
  dep2  <- log(dep[(lastsexage+1):length(dep)]+inc)

  notinf <- !is.infinite(dep1)

  dep1 <- dep1[notinf]
  dep2 <- dep2[notinf]
  v.age <- v.age[notinf]

  mod1  <- gam(dep1 ~ s(v.age, bs = "ps", k = nk),family = "gaussian")
  mod2  <- gam(dep2 ~ s(v.age, bs = "ps", k = nk),family = "gaussian")
  pred1  <- pmax(0,-1+exp(stats::predict(mod1,newdata=d.pred,se.fit=F)))
  pred2  <- pmax(0,-1+exp(stats::predict(mod2,newdata=d.pred,se.fit=F)))

  pred      <- c(pred1,pred2)
  pred[1]   <- dep[1]
  pred[102] <- dep[lastsexage+1]

  pred

}


#' Ensures no negative values for mean (low, high)
#'
#' Substitutes with the latest non-negative value but also ensures the accompanying confidence interval remains correct.
#'
#' @param x a matrix with at least three columns containing the names "mean","lower", and "upper".
#' @param repif replace values of `x` if they are equal or less than `repif`
#' @param repwith replace values of `x` if they are equal or less than `repif` with `repwith`
#'
#' @return a matrix
#' @export
f.replacespecial <- function(x,repif = NULL, repwith = NULL) {

  x           <- as.matrix(x)
  name.x      <- colnames(x)
  mean.names  <- name.x[grepl("mean",name.x)]

  for (q in 1:length(mean.names)) {
    mean.name  <- mean.names[q]
    suffix     <- sub("mean","",mean.name)
    lower.name <- paste("lower",suffix,sep="")
    upper.name <- paste("upper",suffix,sep="")

    # Record positions in original x
    col.num   <- which(colnames(x) %in% c(mean.name,lower.name,upper.name))
    df        <- x[,col.num]
    n_i <- switch(paste(is.na(repif)),
                  "TRUE"  = apply(df,2,FUN = function(x) which(is.na(x))),
                  "FALSE" = apply(df,2,FUN = function(x) which(x<=repif)))
    n_i    <- Reduce(union,n_i)
    if (length(n_i)==0) {next}
    n_i    <- n_i[order(n_i)]


    # Replace with previous available
    if (is.null(repwith)) {
      if (!any(n_i %in% c(1,102))) {
        for (i in n_i) {
          for (j in 1:3) {
            df[i,j] <- df[i-1,j]
          }
        }
      } else {
        n     <- n_i[n_i %in% c(1,102)]
        for (ii in n) {
          y <- switch(paste(ii), "1" = min(setdiff(1:101,n_i)),"102" = min(setdiff(102:202,n_i)))
          for (j in 1:3) {
            df[ii,j] <- df[y,j]
          }
        }

        n_i <- setdiff(n_i,n)

        for (i in n_i) {
          for (j in 1:3) {
            df[i,j] <- df[i-1,j]
          }
        }
      }

    } else {
      for (i in n_i) {
        for (j in 1:3) {
          df[i,j] <- repwith
        }
      }
    }

    x[,col.num] <- df

  }

  return(x)


}


#' Sums the diagonal of the decedent costs files
#'
#' Produces a vector of the spending within the last 5 years of life.
#'
#' @param x a matrix or data.frame with spending at different ages as rows and spending by time-to-death as columns - 5 columns total.
#' @param from an integer showing location of the starting age. Ages start from 0 to 100, hence age 50 will have location 51 for example.
#' @param to   an integer showing location of the end age.
#' @param disc_vector numerical vector of the discount needed to be applied to costs. Depends of user-chosen discount percentage and cycle length.
#'
#' @return a numerical vector of spending in the last 5 years of life given age.
#' @export
f.sumdiag <- function(x, from, to, disc_vector) {

  v_age <- from:to
  N     <- length(v_age)
  xdiag <- vector(length = N)
  mat   <- matrix(0,4,4)

  for (i in 1:N) {
    # First 5 occurrences
    if (i < 5) {
      a  = v_age[i]
      J  = 5-i
      ii = i-1
      for (j in 1:J) {
        mat[j+ii,j] <- x[a,j] * disc_vector[i]
      }
    } else { # Rest
      for (j in 1:5) {
        ii       <- v_age[i]-j+1
        di       <- i-j+1
        xdiag[i] <- xdiag[i] + x[ii,j] * disc_vector[di]
      }
    }
  }

  xdiag[1:4] <- rowSums(mat)

  return(xdiag)

}
