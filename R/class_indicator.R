# Date created: 2017-09-01
# Owner: Nova Institute (Reg# 1994/002614/08).

setClass("indicator",
         slots = list(
           "name" = "character",
           "type" = "character", # one of c(continuous, single_nominal, multi_nominal)
           "val" = "ANY",
           "conf_print" = "character",
           "unit" = "character",
           "formula" = "ANY",
           "sourcepath" = "character",
           "date_created" = "character",
           "description" = "ANY",
           "comments" = "ANY",
           "plots" = "ANY"))


#'@param x An object of class 'indicator'
print.indicator <- function(x, inclCreatedBy = FALSE, inclComments = TRUE) {

  print(sprintf("Name: %s", ifelse(length(x@name) == 0, "-", x@name)),
        quote = FALSE)
  print(sprintf("Unit of measurement: %s",
                ifelse(length(x@unit) == 0, "-", x@unit)),
        quote = FALSE)
  print(sprintf("Description: %s",
                ifelse(length(x@description) == 0, "-", x@description)),
        quote = FALSE)
  print("Value:", quote = FALSE)
  if (is.null(x@val)) {
    print("", quote=FALSE)
  } else {
    if (is.data.frame(x@val)) {
      if (nrow(x@val) <= 6) {
        print(x@val, quote = FALSE)
      } else {
        print(head(x@val, n = 6), quote = FALSE)
        cat(sprintf("...%d rows omitted...\n", nrow(x@val) -6))
      }
    } else {
      print(x@val, quote=FALSE)}
  }

  print(sprintf("Created on: %s",
                ifelse(length(x@date_created) == 0, "-", x@date_created)),
        quote = FALSE)
  if (inclCreatedBy) {
    print(sprintf("Created by: %s",
                  ifelse(length(x@sourcepath) == 0, "-", x@sourcepath)),
          quote = FALSE)
  }
  if (inclComments) {
    print(sprintf("Comments: %s", ifelse(is.null(x@comments), "-", x@comments)),
          quote = FALSE)
  }
}



#'@param x An object of class 'indicator'
report.indicator <- function(x,
                             cap = "",
                             descr = TRUE,
                             comm = TRUE,
                             plt = TRUE,
                             nm = FALSE,
                             headLev = 2,
                             tblFirst = FALSE) {


  # call the end of the following functions as appropriate on x@val:
  # tableContinuous3
  # tableNominal3
  # tableMulti2

  # then call plot(p) on each of the plots in x@plots

  if (nm) {
    cat(sprintf("%s %s",
            paste(rep(x = "#", headLev), collapse = ""),
            x@name),
        "\n")
  }

  if (descr) {
    cat(x@description, "\n")
  }

  if (comm) {
    if (!is.null(x@comments)) {
      if (nchar(x@comments) > 0) {
        cat("Note: ", x@comments, "\n")
      }
    }
  }

  if (!tblFirst) {
    if (plt) {
      if (length(x@plots) > 0) {
        for (p in x@plots) { plot(p) }
      }
    }
  }

  if (x@type == "continuous") {
    reportContinuous(out = x@val,
                     lab = sprintf("tab:%s", x@name),
                     cap = cap)
  }

  if (x@type == "single_nominal") {
    reportSingleNominal(out = x@val,
                        vnmGrp = setdiff(names(x@val), c("response", "n", "perc")),
                        lab = sprintf("tab:%s", x@name),
                        cap = cap)
  }

  if (x@type == "multi_nominal") {
    reportMultiNominal(out = x@val,
                       vnmsGrping = setdiff(names(x@val), c("response", "n", "perc")),
                       lab = sprintf("tab:%s", x@name),
                       cap = cap)
  }

  if (tblFirst) {
    if (plt) {
      if (length(x@plots) > 0) {
        for (p in x@plots) { plot(p) }
      }
    }
  }

  return(invisible(0))
}

