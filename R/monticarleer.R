
#' @title monticarleer
#' @description Do a Monte Carlo simulation with a list of distribution proporties and a formula using them
#' @param prmlst List with items of the class distlist
#' @param n Integer. Number of samples to draw
#' @param df_out Logical. Return a dataframe
#' @param plot Logical. Plot or not
#' @import reshape2
#' @import ggplot2
#' @export
#'
monticarleer <- function(prmlst, my.formula = "a * b ^ c", n = 100, df_out = FALSE, plot = TRUE, verbose = FALSE){
  if(require(reshape2) == FALSE){install.packages("reshape2", dependencies = TRUE); message("Ek laai vir jou reshape2")}
  if(require(ggplot2) == FALSE){install.packages("ggplot2", dependencies = TRUE); message("Ek laai vir jou ggplot2")}
  names(prmlst) = sapply(prmlst, function(x) x@nm)
  res = lapply(prmlst, function(x) roep_param(l = x, n=n))
  names(res) = names(prmlst)
  out = with(res, eval(parse(text = my.formula)))
  rs = do.call("cbind", res)
  out = cbind(out, rs)

  if (plot == TRUE){
    if (verbose) message("jys in die plot loop")
    mr = out %>% as.data.frame() %>% pivot_longer(values_to = "Value", names_to = "Variable", cols = everything())
    p <- ggplot2::ggplot(data = mr, aes(x = Value, group = Variable, fill = Variable)) +
     geom_histogram(aes(y = ..density..), bins = n) +
      geom_density(aes(y =  ..density.., fill = NULL)) +
      facet_wrap(.~Variable, scales = "free") +
      ggtitle(label = "Calculation of:" ,subtitle = my.formula)
    plot(p)
  }
  if (df_out == TRUE) {
    return( data.frame(out) )
  } else {return(invisible(NULL))}
}


