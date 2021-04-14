
#' Kg Hour
#'
#' Uses output of binconf.uur and obtains kg per hour while also plotting a graph using Monte Carlo
#'
#' @param x Data frame such as the tt data frame generated in binconf.uur
#' @param propvar Character vector. Name of the proportion column
#' @param fuelprop Numeric. The fuel proportion
#' @param nnn Numeric vector. The number of timesteps for use in monte carlo simulation
#' @param mcspecfn Character Vector. Filename for MC simulation of specific variables
#' @param mcspecfn.add Character vector. Additional extention to filename mcspecfn
#' @param mcsimfn Character Vector. Filename for MC simulation of coal burned per hour of day
#' @param mcsimfn.add Character vector. Additional extention to filename mcsimfn
#' @param yl Numeric. Y axis limit
#' @export

kg_uur <- function(x = tt, propvar = "prop", fuelprop = 0.65, nnn = c(200, 14),
                   mcspecfn = "~/tmp/MC_estimation_kg_coal_hod_MC_vars", mcspecfn.add = NULL,
                   mcsimfn = "~/tmp/MC_estimation_kg_coal_hod", mcsimfn.add = NULL, yl = 1000){
  #message(ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] )
  #stopifnot("mc.sim" %in% ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] , " jy benodig mc.sim")
  #stopifnot("generateMCSample" %in% ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] , " jy benodig generateMCSample")
  #ll = list(A= list(var = "pop", dist = "unif", params = c(min=1000, max = 1100)), B = list(var = "consumption", dist = "weibull", params = c(shape=2.669757, scale= 4.8/3)))
  require(reshape2)
  require(ggplot2)

  if (!is.null(mcspecfn)) {mcspecfn = paste(mcspecfn, mcspecfn.add, ".pdf", sep="")}

  if (!is.null(mcsimfn)) {mcsimfn = paste(mcsimfn, mcsimfn.add, ".pdf", sep="")}

  uurvuur <- x[ ,propvar]
  l = ll
  rs =  lapply(uurvuur, function(x) {
    mc.sim(l = l, prop=c(fuelprop, x), nn = nnn, plot = TRUE)})
  message(paste(str(rs)," "))
  rsm=melt(rs)
  dev.off()  # modeleer verskil tussen Julie en sept
  #qplot(data=rsm, x=L1, y=value, geom="boxplot", group=L1)
  qplot(data=rsm, x=L1, y=value, geom="jitter", group=L1, alpha=I(1/10)) +
    geom_smooth(aes(group=1), size = I(2)) +
    labs(x = "hour of day", y = "kg coal per hour",
         title = "Estimation of coal burned per hour of day for Kwadela")
  ggsave(filename = mcsimfn)

  plot(sapply(rs, mean), ylim=c(0, yl), type = "n", xlab = "Hour of day", ylab = "kg. coal per household")
  lines(sapply(rs, mean))
  lines(sapply(rs, function(x) mean(x) - sd(x) ), lty = 2)
  lines(sapply(rs, function(x) mean(x) + sd(x) ), lty = 2)


  res = do.call("rbind", (lapply(rs, summary)))
  res
}
