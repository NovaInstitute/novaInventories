#' @title maakIndicResTab
#' @name maakIndicTab
#' @description Make a tibble with values for a number of indicators per site and place
#' @param reftab tibble with site and place
#' @param i Environment with indicators
#' @param props Character. Names of individual indicators
#' @param slot Character. Name of slot in indicator
#' @param var Character. Name of variable in slot
#' @param notibble Logical. Are the results contained in a tibble
#' @export

maakIndicResTab <- function(reftab, i = indicators, props,  slot = "val", var = "PointEst", notibble = FALSE){

  if (notibble){
    return(
      reftab %>% select(site, town) %>% expand_grid(props = props) %>%
        mutate(wastebreek = pmap(list(site, town, props), ~tibble.indicator(i[[tolower(..1)]][[tolower(..2)]][[..3]]) %>%
                                   select(matches({{slot}}) )
                                 )
        ) %>%
        unnest(wastebreek) %>%
        distinct() %>%
        pivot_wider(id_cols = c(site, town), names_from = props, values_from = {{slot}}, values_fn = list) %>%
        unnest(cols = c({{props}}))
    )
  }

  reftab %>% select(site, town) %>% expand_grid(props = props) %>%
    mutate(wastebreek = pmap(list(site, town, props), ~tibble.indicator(i[[tolower(..1)]][[tolower(..2)]][[..3]]) %>%
                              select(matches({{slot}})) %>% unnest(cols = c({{slot}})) %>% select(matches({{var}}))
                             )
    ) %>%
    unnest(wastebreek) %>%
    distinct() %>%
    pivot_wider(id_cols = c(site, town), names_from = props, values_from = {{var}})

}



