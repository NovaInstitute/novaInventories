#' @title resample_dorp_brandstof
#' @description Function to estimate township fuel using resampling. Used within boot_dorp_brandstof
#' @param pop dataframe
#' @param dta data frame giving indication of proportions who use the fuel and format
#' @param placename Character. Default: "place"
#' @param unitname Character. Default: "sol_energy_coal_consumptionwinter"
#' @param formatname  Character. Default: "sol_energy_coal_format"
#' @param notusename Character. Default: "none"
#' @param unitweights Named list with unit weights
#' @param ... Currently ignored
#' @param debug Logical
#' @import scales
#' @export


resample_dorp_brandstof <- function(dta,
                                    placename = "place",
                                    unitname = "sol_energy_coal_consumptionwinter",
                                    formatname  = "sol_energy_coal_format",
                                    pop, unitweights, notusename = "none",
                                    rep =1000, ...){
  idx = sample(1:nrow(dta), size = nrow(dta), replace = TRUE)
  dta = dta[idx, ]
  skat_dorp_brandstof(dta,
                      notusename = get("notusename"),
                      placename = get("placename"),
                      unitname = get("unitname"),
                      formatname  = get("formatname"),
                      pop = get("pop"),
                      unitweights = get("unitweights"))
}

# gebruik: out = lapply(1:rep, function(x){x; resample_dorp_brandstof(dta, pop = pop, unitweights = unitweights)})
