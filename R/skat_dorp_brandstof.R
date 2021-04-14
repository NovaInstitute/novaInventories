#' @title skat_dorp_brandstof
#' @description Function to estimate township fuel. You need: a
#' population data frame, a data frame giving indication of proportions who use
#' the fuel and format, a consumption variable in unit counts, a dataframe with
#' format weights (1 if units are already standardised)
#' lapply this on a bootstrap sample to estimate uncertainty
#' @param pop dataframe
#' @param dta data frame giving indication of proportions who use the fuel and format
#' @param placename Character. Default: "place"
#' @param unitname Character. Default: "sol_energy_coal_consumptionwinter"
#' @param formatname  Character. Default: "sol_energy_coal_format"
#' @param notusename Character. Default: "none"
#' @param unitweights Named list with unit weights
#' @param som Logical
#' @param debug Logical
#' @import scales
#' @import abind
#' @export

skat_dorp_brandstof <- function(pop,
                                dta,
                                placename = "place",
                                unitname = "sol_energy_coal_consumptionwinter",
                                formatname  = "sol_energy_coal_format",
                                notusename = "none",
                                unitweights, som = FALSE, debug = FALSE){
  # drop all NA from pop
  # make NA none in formats and 0 in weights
    dta <- as.data.frame(dta)

	  dta[, formatname] = as.character(dta[, formatname])
	  dta[is.na(dta[,formatname]), formatname] <- "None"
	  dta[, formatname] = as.factor(dta[, formatname])

	  # Check if pop and placename are aligned
	  names(pop) = tolower(gsub("[[:space:]]+", ".", names(pop)))
	  dta[,placename] = as.factor(tolower(gsub("[[:space:]]+", ".", as.character(dta[,placename]))))
	  if (any(is.na(match(levels(dta[,placename]), names(pop))))){
	    stop("All the names of pop and the placename in the data frame you provide must match")
	  }
  # Check if formatname and unitweights are aligned
  notusename = tolower(gsub("[[:space:]]+", ".", as.character(notusename)))
  names(unitweights) = tolower(gsub("[[:space:]]+", ".", names(unitweights)))
  if (debug == TRUE) assign("unitweights2", unitweights, envir = .GlobalEnv)
  #if (is.na(match(notusename, names(unitweights)))) unitweights[,notusename] = 0

  if (!is.na(match(notusename, names(unitweights)))){
    unitweights[,notusename] = 0
  } else {
    unitweights[[length(unitweights)+1]] = 0
    names(unitweights)[length(unitweights)] <- notusename
  }

  dta[[formatname]] <- forcats::as_factor(dta[[formatname]])
  if (all(is.na(match(levels(dta[,formatname]), names(unitweights))))){
    stop("! At least some names of unitweights and the formatname in the data frame you provide must match")
  }

  formatprop = prop.table(table(dta[,formatname]))
  if (debug == TRUE) assign("formatprop", formatprop, envir = .GlobalEnv)
  formatpopprop = as.data.frame.matrix(prop.table(table(dta[,placename], dta[,formatname]), 1))
  fpp  = matrix(nrow = nrow(formatpopprop), ncol=ncol(formatpopprop))
  for (i in 1:nrow(formatpopprop)){
    for (j in 1:ncol(formatpopprop))
      fpp[i,j] <- formatpopprop[i,j]
  }
  dimnames(fpp) = dimnames(formatpopprop)
  unitmean = tapply(dta[,unitname], list(dta[,placename], dta[,formatname]), mean, na.rm=TRUE)
  if (!is.na(match(notusename, colnames(unitmean)))){
    unitmean[,notusename] = 0
  } else {
    unitmean = cbind(unitmean, rep(0, nrow(unitmean)))
    colnames(unitmean)[ncol(unitmean)] <- notusename
  }
  unitsd = tapply(dta[,unitname], dta[,formatname], sd, na.rm=TRUE)
  unitsd[is.na(unitsd)] <- 0

  pop.m = matrix(nrow = nrow(formatpopprop), ncol=ncol(formatpopprop))
  for (i in 1:nrow(formatpopprop)){
    for (j in 1:ncol(formatpopprop))
    pop.m[i,j] <- pop[1,i]
  }
  dimnames(pop.m) = dimnames(formatpopprop)
  if (debug) assign("pop.m", pop.m, envir = .GlobalEnv)
  if (debug) assign("formatpopprop", formatpopprop, envir = .GlobalEnv)

  unit.w.m = matrix(nrow = nrow(formatpopprop), ncol=ncol(formatpopprop))
  for (i in 1:nrow(formatpopprop)){
    for (j in 1:ncol(formatpopprop)){
    	 unit.w.m[i,j] <- sapply(unitweights, mean, na.rm = TRUE)[match(colnames(fpp)[j], names(sapply(unitweights, mean, na.rm =TRUE)))]
    }
  }
 dimnames(unit.w.m) = dimnames(fpp)
 if (!is.na(match(notusename, colnames(unit.w.m)))){
   unit.w.m[,notusename] = 0
 } else {
   unit.w.m = cbind(unit.w.m, rep(0, nrow(unit.w.m)))
   colnames(unit.w.m)[ncol(unit.w.m)] <- notusename
 }

 kg.weight = unit.w.m * unitmean
 pop.format = (pop.m * fpp)
 if (debug == TRUE){assign("pop.format", pop.format, envir = .GlobalEnv)
                    assign("fpp", fpp, envir = .GlobalEnv)
                    assign("kg.weight", kg.weight, envir = .GlobalEnv)}
 pop.weight = pop.format * kg.weight
 sub.kg = rowSums(pop.weight, na.rm = T)
 tot.kg = sum(sub.kg, na.rm = T)
 if (som == TRUE){
   tot.kg
 } else {
   z = abind(unit.weight.mean = unit.w.m,
             units.used.mean = unitmean,
             kg.used = kg.weight,
             proportion.of.households = fpp,
             total.mass.used = pop.weight,
             along = 0)
   list(result = sub.kg, inputs = z) # was sub.kg
 }
}



