#' Plot metsize results
#'
#' @param plot_data Dataframe containing results_sim data from metsize function.
#' @param FDR The target FDR for the experiment.
#' @param nhat The estimate optimal sample size for the experiment.
#'
#' @return Plot object containing plot of FDR vs sample size for metsize data.
#'
#' @noRd
plot_fun <- function(plot_data, FDR, nhat) {
  # aes_ and ~variable_name  used here to pass CRAN checks
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes_(x = ~sample_size, y = ~fdr_50_percentile)
  ) +
    ggplot2::geom_point(colour = 4, size = 2) +
    ggplot2::geom_line(colour = 4, size = 1) +
    # percentile lines
    ggplot2::geom_ribbon(
      ggplot2::aes_(ymin = ~fdr_10_percentile, ymax = ~fdr_90_percentile),
      linetype = 2,
      alpha = 0.1,
      colour = 4,
      size = 1
    ) +
    # target fdr line
    ggplot2::geom_hline(yintercept = FDR, linetype = 3, size = 1) +
    # result line
    ggplot2::geom_vline(
      ggplot2::aes_(
        xintercept = nhat[1],
        linetype = paste(
          "Group 1: ",
          nhat[2],
          "\nGroup 2: ",
          nhat[3]
        )
      ),
      colour = "darkblue",
      size = 1,
      key_glyph = "blank"
    ) +
    ggplot2::labs(
      x = "Sample Size",
      y = "FDR",
      linetype = paste("   Estimated Optimal Sample Size: ", nhat[1], "  ")
    ) +
    ggplot2::annotate(
      geom = "text",
      x = min(plot_data$sample_size),
      y = FDR,
      vjust = -1,
      label = paste("FDR = ", FDR)
    ) +
    ggplot2::theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12),
      legend.key.size = ggplot2::unit(0, "lines"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 22),
      axis.title = ggplot2::element_text(size = 12)
    )
}
