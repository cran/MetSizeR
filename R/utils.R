#' Unit Scale Covariates
#'
#' @param C Matrix containing covariates.
#'
#' @return Matrix containing covariates whose values are scaled to lie between 0 and 1.
#'
#' @noRd
standardize <- function(C) {
  for (i in 1:ncol(C))
  {
    rg <- range(C[, i])
    C[, i] <- (C[, i] - min(C[, i])) / (rg[2] - rg[1])
  }
  C
}


#' Sample from the null distribution for test statistics
#'
#' @param T Number of permutations to estimate for each sampling distribution.
#' @param S Matrix of NA values of size p times T where p is the number of
#'   metabolites and T is the number of permutations to estimate for each
#'   sampling distribution. Use in function is as a storage matrix for standard
#'   deviation with correction factor added.
#' @param TS Matrix of NA values of size p times T where p is the number of
#'   metabolites and T is the number of permutations to estimate for each
#'   sampling distribution. Use in function is as a storage matrix for test
#'   statistics.
#' @param x Matrix of simulated data.
#' @param y Vector containing group membership labels for samples. Should
#'   contain entry 1 for samples in group 1 and entry 2 for samples in group 2,
#'   for a total of n1star + n2star entries, where n1star and n2star are the
#'   group sample sizes at the current sample size iteration.
#' @param n11 The current group 1 sample size minus 1.
#' @param n22 The current group 2 sample size minus 1.
#' @param in1n2 The sum of the reciprocals of current group 1 sample size and
#'   the current group 2 sample size.
#' @param nn2 The sum of the current group 1 sample size and
#'   the current group 2 sample size, minus 2.
#' @param cpcf The 5th percentile of the number of bins.
#'
#' @return List containing matrix S and matrix TS. Matrix S contains the
#'   pooled standard deviations with correction factor added. Matrix TS contains the
#'   test statistics for the designated test - a two-sample test statistic used
#'   to test a difference in the means of two groups.
#'
#' @noRd
samp.dist <- function(T, S, TS, x, y, n11, n22, in1n2, nn2, cpcf) {
  for (t in 1:T)
  {
    yperm <- sample(y, replace = FALSE)
    x1 <- x[yperm == 1, ]
    x2 <- x[yperm == 2, ]
    # sd
    Sj <- sqrt(in1n2 * (n11 * Rfast::colVars(x1) + n22 * Rfast::colVars(x2)) / nn2)
    # stored sd with correction factor
    S[, t] <- Sj + Rfast::Sort(Sj, partial = cpcf)[cpcf]
    TS[, t] <- (Rfast::colmeans(x1) - Rfast::colmeans(x2)) / S[, t]
  }
  return(list(S = S, TS = TS))
}
