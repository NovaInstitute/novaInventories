
#' @title monticarleer
#' @description Do a Monte Carlo simulation with a list of distribution proporties and a formula using them
#' @param prmlst List with items of the class distlist
#' @param n Integer. Number of samples to draw
#' @param tbl_out Logical. Return the tibble
#' @param plot Logical. Plot or not
#' @param nbin Integer. Number of bins
#' @param saveplot Logical. Save the plot to file or not
#' @param fn Character. Filename (and path) to be used if saveplot = TRUE. Default to "mc_plot"
#' @import reshape2
#' @import ggplot2
#' @export
#'
monticarleer <- function(prmlst,
                         my.formula = "a * b ^ c",
                         n = 1000,
                         tbl_out = FALSE,
                         plot = TRUE,
                         nbin = 50,
                         verbose = FALSE,
                         saveplot = FALSE, fn = "mc_plot"){
  if(require(reshape2) == FALSE){install.packages("reshape2", dependencies = TRUE); message("Ek laai vir jou reshape2")}
  if(require(ggplot2) == FALSE){install.packages("ggplot2", dependencies = TRUE); message("Ek laai vir jou ggplot2")}
  names(prmlst) = sapply(prmlst, function(x) x@nm)
  res = lapply(prmlst, function(x) roep_param(l = x, n=n))
  names(res) = names(prmlst)

  out = as_tibble(
    cbind(
      do.call("cbind", res),
      Result = with(res, eval(parse(text = my.formula)))
    )
  )

  if (plot == TRUE){
    if (verbose) message("jys in die plot loop")
    mr = out %>% as.data.frame() %>% pivot_longer(values_to = "Value", names_to = "Variable", cols = everything())
    p <- ggplot2::ggplot(data = mr, aes(x = Value, group = Variable, fill = Variable)) +
      geom_histogram(aes(y = ..density..), bins = nbin) +
      geom_density(aes(y =  ..density.., fill = NULL)) +
      facet_wrap(.~Variable, scales = "free") +
      theme(legend.position = "bottom") +
      ggtitle(label = "Calculation of:" ,subtitle = my.formula)

    plot(p)
    if (saveplot) { ggsave(fn, plot = p, width = 12) }
  }
  if (tbl_out == TRUE) {
    return( out)
  } else {return(list(out))}
}


