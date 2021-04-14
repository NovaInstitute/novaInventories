#' @title roep_param
#' @description Generate n or length(x) values from the distribution described in distlist l
#' @param l distlist
#' @param x Numeric. Sobol sequence between 0 and 1 or NULL. If NULL, will be generated using sobol(n)
#' @param n Integer. Length of Sobol sequence or NULL. Will be used if x is NULL
#' @param verbose Logical. Talk back or not
#' @import randtoolbox
#' @export

roep_param <- function(l, x = NULL, n = NULL, verbose = FALSE){
  if (class(l) != "distlist") stop("l must be of the class distlist, use maak_distlist() to create a list of parameters")
  if (require(randtoolbox) == FALSE) {
    message("You need randtoolbox, my brother. Don't worry, I will install it")
    install.packages("randtoolbox", dependencies = TRUE)
    if (require(randtoolbox) == TRUE) message("Sien jy, ek het gesê alles gaan regkom") else ("Dit wou nie werk nie, jys op jou eie, my ou")
  }
  # If x is NULL use a sobol object but then n cannot be NULL
  if (is.null(x)){
    if(is.null(n)){
      stop("n and x cannot both be NULL")
    }
    x = randtoolbox::sobol(n)
  }
  if (!is.numeric(x)) stop("x must be  numeric")
  if (any(x < 0)) stop("x must be between 0 and 1. If you don't have x you can set n to a suitably high number, e.g. n=1000")
  if (any(x > 1)) stop("x must be between 0 and 1. If you don't have x you can set n to a suitably high number, e.g. n=1000")

  agn = lapply(c("p", names(l@params)), as.name)
  d = paste("q", l@dist, sep="")
  vls = list()
  vls[[1]] = x
  for (i in 1:length(l@params)){
    vls[[i+1]] <- l@params[i]
  }
  names(vls) <- agn
  do.call(d, vls)
}
