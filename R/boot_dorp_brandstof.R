#' @title boot_dorp_brandstof
#' @description Function to estimate township fuel use with bootstrapping
#' @param pop dataframe
#' @param dta data frame giving indication of proportions who use the fuel and format
#' @param placename Character. Default: "place"
#' @param unitname Character. Default: "sol_energy_coal_consumptionwinter"
#' @param formatname  Character. Default: "sol_energy_coal_format"
#' @param notusename Character. Default: "none"
#' @param unitweights Named list with unit weights
#' @param rep Integer. Default: 1000
#' @param plt Logical. Plot or not. Default: TRUE
#' @param debug Logical.
#' @param dropperc Numeric. Drop proportions smaller than dropperc. Default 0.025
#' @param graphdir Character. Directory to write graphs to. Default: "Grafieke",
#' @param stoor Logical: Save output or not. Default: TRUE
#' @export

boot_dorp_brandstof <- function(pop,
                                dta,
                                placename = "place",
                                unitname = "sol_energy_coal_consumptionwinter",
                                formatname  = "sol_energy_coal_format",
                                notusename = "none",
                                unitweights,
                                rep = 1000,
                                plt = TRUE, debug = FALSE,
                                dropperc = 0.025,
                                graphdir = "Grafieke", stoor = TRUE){
  out = lapply(1:rep, function(x){x; resample_dorp_brandstof(dta,
                                                             placename = get("placename"),
                                                             unitname = get("unitname"),
                                                             formatname  = get("formatname"),
                                                             pop = get("pop"),
                                                             notusename = get("notusename"),
                                                             unitweights = get("unitweights"))})

  res = do.call("rbind", lapply(out, function(x) x$result))
  inp = lapply(out, function(x) x$inputs)
  if (debug == TRUE) assign("inp", inp, envir = .GlobalEnv)
  m.inp = reshape2::melt(inp)
  m.inp <- m.inp[!is.na(m.inp$value), ]
  names(m.inp) = c("variable", "sub", "format", "value", "repitition")
  drpidx = which(m.inp$value == 0)
  if (length(drpidx) > 0) m.inp = m.inp[-drpidx, ]
  drpidx2 = which(m.inp[m.inp$variable == "proportion.of.households","value"] < dropperc)
  patt = "([[:print:]]+)(\\([[:print:]]+\\))"
  m.inp$format <- gsub(patt, "\\1", x = m.inp$format)
  if (length(drpidx2) > 0) m.inp = m.inp[-drpidx2, ]
  if (plt == TRUE){
    if (require(ggplot2) == FALSE) install.packages("ggplot2", dependencies = TRUE)
    if (require(reshape2) == FALSE) install.packages("reshape2", dependencies = TRUE)
    resm = reshape2::melt(res)
    names(resm)[2] <- "variable"
    mx = max(resm$value)
    p <- qplot(x = value, geom="density", data = resm,
               group = variable, fill = variable, alpha = I(1/5), xlim = c(0,mx),
               main = paste("Density of the estimation of total fuel use in tonne based on a bootstap sample of ", rep))  +
      theme(legend.position = "bottom")
    if (stoor == TRUE )ggsave(filename = paste(graphdir, "TownWinterMonth_kg.png", sep=""), plot = p, width = 12)
    plot(p)
    p2 = qplot(data = m.inp, x = value, group = format, fill = format,
               main = paste("Input variables (", rep, " repitions)")) +
      facet_grid(sub ~ variable, scales = "free") +
      geom_density(adjust = 10) +
      theme(legend.position = "bottom")

    if (stoor == TRUE ) ggsave(filename = paste(graphdir, "Inputs_TownWinterMonth_kg.png", sep=""), plot = p2, width = 12)
    plot(p2)
  }
  as.data.frame(res)
}
