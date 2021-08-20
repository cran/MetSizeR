#' Simulate data using pilot data
#'
#' Simulate data for use in the MetsizeR algorithm using parameters gleaned from
#' available pilot data for the experiment
#'
#' @param n1 Group 1 minimum sample size to test.
#' @param n2 Group 2 minimum sample size to test.
#' @param p Number of bins.
#' @param Zerop Vector of zeros of length p.
#' @param Ip Identity matrix of dimension p.
#' @param Zeroq Vector of zeros where length is the number of dimensions of the latent space.
#' @param Iq Identity matrix with same dimensions as the latent space.
#' @param ZeroL Vector of zeros where length is the number of covariates present. For PPCCA only.
#' @param IL Identity matrix where dimension is the number of covariates present. For PPCCA only.
#' @param ZeroL1 Vector of zeros where length is the number of covariates present, plus 1. For PPCCA only.
#' @param IL1 Identity matrix where dimension is the number of covariates present, plus 1. For PPCCA only.
#' @param sig Posterior mode estimate of the variance of the error terms of the model.
#' @param W Loadings matrix.
#' @param Alpha Maximum likelihood estimates of the regression coefficients associated with the covariates in the PPCCA model. For PPCCA only.
#' @param mu Mean vector for pilot data metabolites.
#' @param mod Vector of possible models.
#' @param model Chosen model.
#'
#' @return Matrix of simulated data.
#'
#' @noRd
sim.pilot.data <- function(n1, n2, p, Zerop, Ip, Zeroq, Iq, ZeroL, IL, ZeroL1, IL1, sig, W, Alpha, mu, mod, model) {
  # generate parameters
  n <- n1 + n2
  if (mod[1] == model) {  # PPCA
    u <- Rfast::rmvnorm(n, Zeroq, Iq)
  } else {  # PPCCA
    C <- Rfast::rmvnorm(n, ZeroL, IL)
    # Standardize covariates for stability
    C <- standardize(C)
    C <- rbind(rep(1, n), t(C))
    u <- Rfast::rmvnorm(n, Zeroq, Iq) + t(Alpha %*% C)
  }
  # generate data
  x <- Rfast::rmvnorm(n, Zerop, sig * Ip) + tcrossprod(u, W) + matrix(mu, n, p, byrow = TRUE)  # keep tcrossprod in base because faster for needed dimensions
  return(x)
}

#' Simulate data without using pilot data
#'
#' Simulate data for use in the MetsizeR algorithm when pilot data are not
#' available. Parameters are gleaned from the expected structure of the data.
#'
#' @param n1 Group 1 minimum sample size to test.
#' @param n2 Group 2 minimum sample size to test.
#' @param p Number of bins.
#' @param Zerop Vector of zeros of length p.
#' @param Ip Identity matrix of dimension p.
#' @param q Number of dimensions of the latent space.
#' @param Zeroq Vector of zeros where length is the number of dimensions of the latent space.
#' @param Iq Identity matrix with same dimensions as the latent space.
#' @param ZeroL Vector of zeros where length is the number of covariates present. For PPCCA only.
#' @param IL Identity matrix where dimension is the number of covariates present. For PPCCA only.
#' @param ZeroL1 Vector of zeros where length is the number of covariates present, plus 1. For PPCCA only.
#' @param IL1 Identity matrix where dimension is the number of covariates present, plus 1. For PPCCA only.
#' @param alpha.sigma Scale parameter of the prior distribution of the variance.
#' @param beta.sigma Shape parameter of the prior distribution of the variance.
#' @param mod Vector of possible models.
#' @param model Chosen model.
#' @param ao1 A vector of length q where each entry is the scale parameter of the prior distribution of the variance.
#' @param bo A vector containing c(0.5*(ao-1),0.25*(ao-1)).
#'
#' @return Matrix of simulated data.
#'
#' @noRd
sim.pilot <- function(n1, n2, p, Zerop, Ip, q, Zeroq, Iq, ZeroL, IL, ZeroL1, IL1, alpha.sigma, beta.sigma, mod, model, ao1, bo) {
  # generate parameters
  n <- n1 + n2
  sig <- 1 / stats::rgamma(1, alpha.sigma, beta.sigma)
  if (mod[1] == model) {  # PPCA
    u <- Rfast::rmvnorm(n, Zeroq, Iq)
  } else {  # PPCCA
    Alpha <- Rfast::rmvnorm(q, ZeroL1, 3 * IL1)
    C <- Rfast::rmvnorm(n, ZeroL, IL)
    # Standardize covariates for stability
    C <- standardize(C)
    C <- rbind(rep(1, n), t(C))
    u <- Rfast::rmvnorm(n, Zeroq, Iq) + t(Alpha %*% C)
  }
  v <- 1 / stats::rgamma(q, ao1, bo)
  W <- Rfast::rmvnorm(p, Zeroq, v * Iq)
  # generate data
  x <- Rfast::rmvnorm(n, Zerop, sig * Ip) + tcrossprod(u, W)
  return(x)
}
