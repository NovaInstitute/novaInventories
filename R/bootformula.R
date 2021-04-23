#' @title bootformula
#' @description Generate values from a formula using bootstrapping
#' @param frm Formula
#' @param rep Integer. Number of repeat
#' @export

bootformula <- function(frm = "mean(rpois(n = 31, lambda = 0.73)) * 31", rep = 1000){
  l <- integer()
  for(i in 1:rep) l[i] <- eval(parse(text = frm))
  l
}

