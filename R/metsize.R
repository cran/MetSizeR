#' Metsize
#'
#' Function to estimate sample size required to achieve a desired statistical
#' power.
#'
#' @param pilot A matrix containing pilot data, if available.
#' @param n1 Sample group 1 minimum sample size to consider.
#' @param n2 Sample group 2 minimum sample size to consider.
#' @param p Number of bins in the data.
#' @param prop Expected proportion of significant bins.
#' @param covars Matrix of covariates, if available. Categorical covariates
#'   should be included as X-1 dummy variables for a covariate with X levels.
#' @param ncovar The number of covariates present. For Z numeric covariates and Y categorical covariates with X total levels, this should be Z + X - Y.
#' @param model The chosen model.
#' @param plot.prop TRUE/FALSE should sample size be calculated for varying proportions of significant bins?
#' @param target.fdr The desired false discovery rate.
#' @param Targeted TRUE/FALSE is the experiment intended as targeted analysis?
#'
#' @return List containing the estimated sample size with the per-group sample
#'   size breakdown, results of FDR calculations for each sample size tested,
#'   results for varying the proportion of significant metabolites, the number
#'   of bins input, the number of covariates input, the model type used, the
#'   input minimum sample sizes for group 1 and group 2, and the desired FDR.
#'
#' @export
#'
#' @references G. Nyamundanda, I. C. Gormley, Y. Fan, W. M. Gallagher and L. Brennan, BMC Bioinformatics, 2013, 14, 338.
#'
#' @examples
#' metsize(pilot = NULL, n1 = 4, n2 = 4, p = 200, prop = 0.2, covars = NULL,
#'     ncovar = 0, model = "PPCA", plot.prop = FALSE, target.fdr = 0.10,
#'     Targeted = FALSE)
#' metsize(pilot = NULL, n1 = 6, n2 = 4, p = 100, prop = 0.1, covars = NULL,
#'     ncovar = 2, model = "PPCCA", plot.prop = FALSE, target.fdr = 0.05,
#'     Targeted = TRUE)
metsize <- function(pilot = pilot, n1 = 4, n2 = 4, p = 200, prop = 0.25, covars = covars, ncovar = 0, model = "PPCA", plot.prop = FALSE, target.fdr = 0.05, Targeted = FALSE) {
  mod <- c("PPCA", "PPCCA")
  # get relevant parameters from pilot data
  if (length(pilot) != 0) {
    if (mod[2] == model) { # ppcca
      ncovar <- ncol(covars)
      resppcca <- MetabolAnalyze::ppcca.metabol(pilot, covars, 2, 2, scale = "unit")
      W <- resppcca$loadings
      sig <- resppcca$sig
      Alpha <- resppcca$coefficients
      mu <- colMeans(pilot)
      p <- ncol(pilot) # gets number of bins/metabs from pilot data
    } else { # ppca
      pilot <- sweep(pilot, 2, colMeans(pilot), "-")
      resppca <- MetabolAnalyze::ppca.metabol(pilot, 2, 2, scale = "unit")
      W <- resppca$loadings
      sig <- resppca$sig
      mu <- colMeans(pilot)
      p <- ncol(pilot) # gets number of bins/metabs from pilot data
    }
  }

  if (plot.prop == FALSE) {
    mprop <- c(prop) # proportion of significant metabolites
    mrange <- ceiling(mprop * p) # different number significant metabolites
    mtry <- length(mrange) # number of different m values
    nprop.increment <- 1 # Sample size increments
    sfactors <- (n1 + n1) * nprop.increment
  } else { # vestigal proportion plotting
    mprop <- c(prop, seq(0.1, 0.5, 0.1)) # proportion of significant metabolites
    mrange <- ceiling(mprop * p) # different number significant metabo
    mtry <- length(mrange) # number of different m values
    nprop.increment <- c(1, 2, 4, 7) # Sample size increments
    sfactors <- (n1 + n2) * nprop.increment # four different sample sizes to be considered
  }

  # Setting up the initial values and the prior parameters
  T <- 20 # number of permutations to estimate the sampling distribution.
  Sim <- 20 # Number of pilot data sets simulated.
  L <- ncovar # number of covariates
  n <- n1 + n2 # sample size for pilot data
  q <- 2 # dimension of the latent space
  alpha.sigma <- ao <- 5 # the scale parameter of the prior distribution of the variance.
  beta.sigma <- 2 * (alpha.sigma - 1) # the shape parameter of the prior distribution of the variance.
  bo <- c(0.5 * (ao - 1), 0.25 * (ao - 1))
  ao1 <- rep(ao, q)
  n.increment <- c(1:8) # Sample size increments considered.
  ntry <- length(n.increment) # number of different samples sizes to be considered.

  # Storing statistics for each sample size
  fdr50_sim <- matrix(NA, ntry, 1)
  fdr90_sim <- matrix(NA, ntry, 1)
  fdr10_sim <- matrix(NA, ntry, 1)
  fdr_sim <- matrix(NA, ntry, Sim)

  # Storing statistics for each m values
  fdr50_prop <- matrix(NA, mtry - 1, length(sfactors))
  fdr90_prop <- matrix(NA, mtry - 1, length(sfactors))
  fdr10_prop <- matrix(NA, mtry - 1, length(sfactors))
  Add <- array(NA, c(p, T, ntry))
  TSstore <- array(NA, c(p, T, ntry))

  # define effect size
  # Based on variance of underlying simulation model
  delta <- stats::qnorm(0.99)
  if (length(pilot) != 0) {
    delta <- stats::qnorm(0.89)
  }

  # objects for data simulation and storage
  cf <- 0.05 # for correction factor
  Zeroq <- rep(0, q)
  Iq <- diag(q) # no need to change to Rfast because small dims, q = 2
  Zerop <- rep(0, p)
  Ip <- Rfast::Diag.matrix(p, v = 1)
  if (mod[2] == model) {
    ZeroL <- rep(0, L)
    IL <- diag(L) # no need to change to Rfast because small dims, L < 10 typically
    ZeroL1 <- rep(0, (L + 1))
    IL1 <- diag(L + 1) # no need to change to Rfast because small dims, L < 10 typically
  }
  TS <- S <- matrix(NA, p, T)
  cpcf <- ceiling(p * cf) # for correction factor

  # for each prop
  for (m in 1:mtry) {
    pstat <- 1 - (mrange[m] / p)
    ind <- matrix(FALSE, p, T)
    pos <- sample(1:p, size = mrange[m]) # sampling mrange[m] metabolites
    ind[pos, ] <- TRUE # matrix indicating truly significant and non significant metabolites
    i <- 0 # counter

    # for each sample size
    for (k in 1:ntry)
    {
      n1star <- n1 * n.increment[k] # sample size for treatment group 1
      n2star <- n2 * n.increment[k] # sample size for treatment group 2
      # further objects needed
      nstar <- n1star + n2star
      in1n2 <- 1 / n1star + 1 / n2star
      n11 <- n1star - 1
      n22 <- n2star - 1
      nn2 <- n1star + n2star - 2
      Add.sd <- sqrt(in1n2)

      y <- c(rep(1, n1star), rep(2, n2star))

      # If influence of different proportion of significant metabolites is not of interest
      if (m == 1) {

        # assessing the effect of repeated simulations from the underlying model
        for (s in 1:Sim) {
          # Simulating the pilot data
          if (length(pilot) != 0) {
            x <- sim.pilot.data(n1star, n2star, p, Zerop, Ip, Zeroq, Iq, ZeroL, IL, ZeroL1, IL1, sig, W, Alpha, mu, mod, model)
          } else {
            x <- sim.pilot(n1star, n2star, p, Zerop, Ip, q, Zeroq, Iq, ZeroL, IL, ZeroL1, IL1, alpha.sigma, beta.sigma, mod, model, ao1, bo)
          }

          # Estimating the sampling distribution of the test statistic using permutations
          res.sampdist <- samp.dist(T, S, TS, x, y, n11, n22, in1n2, nn2, cpcf)
          TS <- res.sampdist$TS
          S <- res.sampdist$S

          # Store the test statistics for the current sample size
          TSstore[, , k] <- TS

          # calculating the shift in metabolites
          vars <- S / Add.sd
          Add[, , k] <- delta / (vars * Add.sd)

          # estimating the FDR
          tsB <- TS
          # Add an increment factor to the test statistics of the truly significant metabolites
          tsB[pos, ] <- tsB[pos, ] + Add[pos, , k]
          atsB <- abs(tsB)
          # identifying a cut-off point (m-th largest absolute value of the p TsB values)
          crit <- stats::quantile(atsB, pstat)
          # false discovery rate calculation
          errors <- (colSums(atsB > crit & !ind)) / (colSums(atsB > crit))
          fdr_sim[k, s] <- stats::quantile(errors[!is.na(errors)], 0.5) # median FDR of the Tstar permutations
        }

        # assessing the effect of repeated simulations from the underlying model
        emp <- stats::quantile(fdr_sim[k, ], c(0.1, 0.5, 0.9))
        fdr10_sim[k] <- emp[1]
        fdr50_sim[k] <- emp[2]
        fdr90_sim[k] <- emp[3]
      } else {
        # assessing the effect of varying the proportion of truly significant metabolites (increasing m values) on four different sample sizes.
        if (any(nprop.increment == n.increment[k])) {
          i <- i + 1
          # adding a shift component to truly significant metabolites
          tsB <- TSstore[, , k]
          tsB[pos, ] <- tsB[pos, ] + Add[pos, , k]
          atsB <- abs(tsB)
          crit <- stats::quantile(atsB, 1 - (mrange[m] / p))
          # calculate FDR
          errors <- (colSums(atsB > crit & !ind)) / (colSums(atsB > crit))
          emp <- stats::quantile(errors[!is.na(errors)], c(0.1, 0.5, 0.9))
          fdr10_prop[m - 1, i] <- emp[1]
          fdr50_prop[m - 1, i] <- emp[2] # median FDR for Tstar permutations permutations
          fdr90_prop[m - 1, i] <- emp[3]
        } # if(any)
      } # if
    } # k
  } # m

  # store results and format nicely
  results_sim <- cbind(n.increment * n, fdr50_sim, fdr90_sim, fdr10_sim)
  results_prop <- cbind(mprop[-1], fdr50_prop, fdr10_prop, fdr90_prop)
  colnames(results_sim) <- c("sample_size", "fdr_50_percentile", "fdr_90_percentile", "fdr_10_percentile")
  colnames(results_prop) <- c("prop", paste("fdr50_prop", sfactors, sep = "_"), paste("fdr10_prop", sfactors, sep = "_"), paste("fdr90_prop", sfactors, sep = "_"))
  results_prop <- list(results_prop, sfactors)
  names(results_prop) <- c("results_prop", "sample_sizes")

  # Determine the sample size at which the FDR line is equal to 0.05
  # by linear regression
  opty <- rep(0, 2)
  ind1 <- min(c(1:nrow(results_sim))[results_sim[, 2] < target.fdr])
  ind2 <- max(c(1:ind1)[results_sim[1:ind1, 2] > target.fdr])
  opty <- c(results_sim[ind1, 2], results_sim[ind2, 2])
  optx <- c(results_sim[ind1, 1], results_sim[ind2, 1])
  optres <- stats::lm(opty ~ optx)
  nhat <- round((target.fdr - optres$coef[1]) / optres$coef[2])

  # calculate returned values based on input sample sizes for groups
  if (n1 == n2) {
    if (Rfast::is_integer(nhat / 2)) {
      n1 <- n2 <- nhat / 2
    } else {
      nhat <- nhat + 1
      n1 <- n2 <- nhat / 2
    }
  } else {
    if (Rfast::is_integer(n1 * nhat / (n1 + n2))) {
      n1 <- n1 * nhat / (n1 + n2)
      n2 <- nhat - n1
    } else {
      n1.user <- n1
      n2.user <- n2
      n1 <- ceiling(n1.user * nhat / (n1.user + n2.user))
      n2 <- ceiling(n2.user * nhat / (n1.user + n2.user))
      nhat <- n1 + n2
    }
  }
  est <- c(nhat, n1, n2)
  names(est) <- c("n", "n1", "n2")

  return(list(nhat = est, results_sim = results_sim, results_prop = results_prop, p = p, prop = prop, ncovar = ncovar, model = model, n1 = n1, n2 = n2, target.fdr = target.fdr))
}
