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

  d %>% dplyr::select(matches(nms)) %>%  mutate(site = l3[[2]],  town = l3[[3]])
}

#' @title NA_vir_niks
#' @description Turn NULL and character(0) into NA
#' @param x


NA_vir_niks <- function(x){
  if (is.null(x)) return(NA)
  if (length(x) == 0) return(NA)
  x
}




# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
