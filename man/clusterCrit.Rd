\name{clusterCrit}
\alias{clusterCrit}
\alias{clusterCrit-package}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{~ Overview: Clustering Indices ~}
\description{
  \tabular{ll}{
    Package: \tab clusterCrit\cr
    Type: \tab Package\cr
    Version: \tab \Sexpr[stage=render]{library(utils);packageVersion("clusterCrit")}\cr
    Date: \tab \Sexpr[stage=build]{format(Sys.time(),"\%Y-\%m-\%d")}\cr
    License: \tab \Sexpr[stage=build]{library(utils);packageDescription("clusterCrit")$License} \cr
  }
}
\details{
  \code{clusterCrit} computes various clustering validation or quality
  criteria and partition comparison indices. Type
\preformatted{
    library(help="clusterCrit")
} for more info about the available functions.
}

\section{Author}{
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre\cr
  Lab Modal'X (EA 3454)\cr \cr
}

\references{
For more information about the algebraic background of clustering indices
and their definition, see the vignette accompanying this package. To
display the vignette, type the following instruction in the R console :
\preformatted{> vignette("clusterCrit")}
}

\seealso{
\code{\link{extCriteria}},
\code{\link{getCriteriaNames}},
\code{\link{intCriteria}},
\code{\link{bestCriterion}},
\code{\link{concordance}}.
}


\keyword{package}
\keyword{partition}
\keyword{clusters}
\keyword{indices}




