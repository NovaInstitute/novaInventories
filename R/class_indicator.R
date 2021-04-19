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

#' @title NA_vir_niks
#' @description Turn NULL and character(0) into NA
#' @param x


NA_vir_niks <- function(x){
  if (is.null(x)) return(NA)
  if (length(x) == 0) return(NA)
  x
}

#' @title tibble.indicator
#' @description Turn an indicator object into a tibble
#' @param x indicator
#' @export

tibble.indicator <- function(x){
  l <- map(slotNames(x), ~slot(x, .))
  names(l) <- slotNames(x)
  d <-
    tibble(
      name = NA_vir_niks(l[["name"]]),
      type =  NA_vir_niks(l[["type"]]),
      val = list( NA_vir_niks(l[["val"]])),
      conf_print =  NA_vir_niks(l[["conf_print"]]),
      unit =  NA_vir_niks(l[["unit"]]),
      formula =  NA_vir_niks(l[["formula"]]),
      sourcepath =  NA_vir_niks(l[["sourcepath"]]),
      date_created =  NA_vir_niks(l[["date_created"]]),
      description =  NA_vir_niks(l[["description"]]),
      comments =  NA_vir_niks(l[["comments"]]),
      plots = list(l[["comments"]])
      )
  d
}

#' @title tibble.indicator.plek
#' @description Turn an indicator object into a tibble with site and town variable
#' @param x indicator
#' @export

tibble.indicator.plek <- function(x, nms = c("name", "description", "val", "unit")){
  d <- tibble.indicator(x)
  l2 <- substitute(x) %>% as.character()
  l3 <- strsplit(l2[[2]], "\\$")[[1]]

  d %>% select(matches(nms)) %>%  mutate(site = l3[[2]],  town = l3[[3]])
}


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
