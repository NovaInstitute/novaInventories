#' @title distlist
#' @description distlist class
#' @exportClass distlist

setClass("distlist",
         representation = list(nm = "character",
                               dist = "character",
                               params="numeric"),
         prototype = list(nm="a", dist = "unif", params = c(min=1, max=2)))

#' @title maak.distlist
#' @description Create an instiance of the class distlist
#' @param nm Character. Name of variable
#' @param dist Character. Name of distribution.
#' @param params Numeric. Named parameter values
#' @export

maak_distlist <- function(nm,  dist , params){
  out = new(Class = "distlist", nm = nm, dist=dist, params = params)
  out
}
