#' @title deriveGammaParams
#' @description Derive the parameters for the Gamma distribution from the mean and sd
#' @param Mean Numeric. The mean
#' @param Sd Numeric. The standard deviation
#' @param Var Numeric. The variance (alternative to sd)
#' @export

deriveGammaParams <- function(Mean = NULL, Sd = NULL, Var = NULL){
  if (is.null(mean)) {stop("You have to specify Mean")}
  if (is.null(Sd) & is.null(Var)) {stop("You have to specify either Sd or Var")}

  if (is.null(Var)){
    Var <- Sd^2
  }

  theta_hat <- Var / Mean
  alpha_hat <- Mean / theta_hat
  lambda_hat <- 1 / theta_hat

  c(shape = alpha_hat, scale = theta_hat)
}
