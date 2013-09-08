\name{nmr_spectra}
\Rdversion{0.99}
\alias{nmr_spectra}
\docType{data}
\title{
NMR metabolomic spectra from urine samples of 18 mice.
}
\description{
NMR metabolomic spectra from urine samples of 18 mice. Each spectrum has 189 spectral bins, measured in parts per million (ppm). Covariate associated with the mice was also recorded: the weight. 
}
\usage{data(nmr_spectra)}
\format{
  A data frame with 18 observations on the following 190 variables.
  \describe{
    \item{\code{weight}}{a numeric vector}
    \item{\code{spectra}}{a numeric vector}
  }
}
\details{
The nmr_spectra data is included in nmr_spectra.txt which can be uploaded into MetSizeR. 
}
\source{
Nyamundanda, G., Gormley, I.C. and Brennan, L. (2010) Probabilistic principal components analysis for metabolomic data, Bioinformatics, 11.
}
\keyword{datasets}
