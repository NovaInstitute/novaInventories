#' @title genereerTydMat
#' @description genereer 'n matriks met 0 en 1 vir gebruik in Aermod ewekansig met waarskynlikheid vir dag en uur
#' @param week_p Numeriese vektor van lengte 2 met waarskynlikehede van vir 0 en 1 (ja en). Verstek 1 dag per week
#' @param uur_p Numeriese vektor van lengte 24 met waarskynlikehede vir uur van dag. Verstek 1 uur per dag
#' @return Matriks van 7x24
#' @export

genereerTydMat <- function(week_p = c(6/7, 1/7),
                           uur_p = c(23/24, 1/24)){


  branddag <- sample(x = c(0,1), size = 7, replace = TRUE, prob = week_p)
  branduur <- matrix(sample(x = c(0,1), size = 24, replace = TRUE, prob = uur_p), nrow = 1)
  nuluur <- matrix(rep(0, 24), nrow = 1)
  do.call("rbind", map(.x = branddag, ~{if (. != 0) return(branduur) else return(nuluur)}))
}
