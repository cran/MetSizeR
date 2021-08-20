
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetSizeR

MetSizeR is a Shiny application developed for metabolomic scientists to
estimate the optimal sample size required for a study to achieve a
desired statistical power for their study, even in cases where pilot
data are not available. The MetSizeR application is developed based on
the methodology by Nyamundanda et al. (2013).

## Installation

MetSizeR can be installed from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MetSizeR")
```

## Accessing MetSizeR application

All MetSizeR functionality is accessed through a Shiny application,
which can be accessed by running the following command:

``` r
# load MetSizeR package
library(MetSizeR)

# launch MetSizeR Shiny application
MetSizeR()
```

## References

Nyamundanda, G., Gormley, I. C., Fan, Y., Gallagher, W. M., & Brennan,
L. (2013). MetSizeR: Selecting the optimal sample size for metabolomic
studies using an analysis based approach. BMC Bioinformatics, 14(1),
338. <https://doi.org/10.1186/1471-2105-14-338>
