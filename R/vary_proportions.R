#' Runs metsize function for specified proportions of significant bins.
#'
#' @param props A vector containing the proportions of significant bins on which to run metsize funtion.
#' @param p The number of bins.
#' @param fdr The target FDR.
#'
#' @return A list containing the metsize results for each of the input proportions.
#'
#' @noRd
metsize_props <- function(props, p, fdr, mod, num_covs = 0) {
  props <- as.vector(stats::na.omit(props))

  results_list <- vector(mode = "list", length = length(props))

  # run metsize function for each proportion given and store results
  for (i in 1:length(props)) {
    result <- metsize(pilot = NULL, n1 = 4, n2 = 4, p = p, prop = props[i], covars = NULL, ncovar = num_covs, model = mod, plot.prop = FALSE, target.fdr = fdr, Targeted = FALSE)

    results_list[[i]] <- list(results_sim = result$results_sim, nhat = result$nhat)
  }

  return(results_list)
}
