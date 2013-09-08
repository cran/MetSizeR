\name{MetSizeR}
\Rdversion{0.99}
\alias{MetSizeR}
\title{MetSizeR GUI 
}
\description{ Start the Graphical User Interface for MetSizeR. The MetSizeR GUI provides a user friendly method for sample size estimation for both targeted and NMR metabolomic experiments even when experimental pilot data is not available.
}
\usage{
MetSizeR()
}
\details{
\strong{MetSizeR GUI options:}
      
\tabular{lll}{
    \tab \strong{File}                       \tab Read in data and covariates, quit \cr
    \tab Open                                 \tab upload the txt file containing the spectrum \cr
    \tab covariates                          \tab upload the txt file containing the covariates \cr
    \tab demo_nmr_pilot_data	               \tab upload the demonstration NMR data already in MetSizeR \cr
    \tab Quit	                               \tab dispose the GUI \cr
    \tab --------------------\tab \cr              
    \tab \strong{Sample size}               \tab  sample size estimation    \cr
    \tab pilot data				                  \tab  sample size estimation with experimental pilot data   \cr
    \tab no pilot data				              \tab  sample size estimation with no experimental pilot data   \cr
    \tab NMR data       		                \tab	sample size estimation for a NMR experiment  \cr
    \tab Targeted     				              \tab  sample size estimation for a targeted experiment \cr
    \tab Spectral bins                      \tab  number of spectral bins from the NMR experiment\cr
    \tab Metabolites        	              \tab  number of metabolites for targeted analysis \cr
    \tab Proportion of significant bins	    \tab	proportion of spectral bins expected to be significant \cr
    \tab Proportion of significant metabolites \tab	proportion of metabolites expected to be significant \cr
    \tab Models 			                      \tab  different types of models available \cr
    \tab ncovars   		                      \tab  number of covariates for the PPCCA model \cr
    \tab Target FDR   		                  \tab  level of control over type I errors \cr
    \tab Sample size per group              \tab  smallest sample size to be considered in each group \cr
    \tab Save results in R directory        \tab  save results in the R working directory \cr
    \tab calculate                          \tab  estimate the sample size \cr
    \tab Plot proportion of significant bins \tab  assess the effect of varying the expected proportion of significant  \cr
    \tab                                     \tab bins \cr
    \tab Plot proportion of significant metabolites \tab assess the effect of varying the expected proportion of proportion \cr
    \tab                                     \tab of significant metabolites \cr
    \tab MetSizeR status \tab  displays if MetSize has finished estimating the sample size\cr
    \tab --------------------\tab  \cr              
    \tab \strong{Help}                      \tab manual for MetSizeR \cr
    \tab manual						                  \tab manual for MetSizeR \cr
}

}

\author{
Gift Nyamundanda \email{gift.nyamundanda@ucd.ie}, Isobel Claire Gormley, Yue Fan, William M Gallagher, Lorraine Brennan
}
\note{
MetSizeR is built upon gWidgets package. Make sure gWidgets package is properly installed.
}

\examples{

\dontrun{
MetSizeR()
}
}

