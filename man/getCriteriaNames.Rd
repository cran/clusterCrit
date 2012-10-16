\name{getCriteriaNames}
\alias{getCriteriaNames}

\title{Get clustering criteria names}

\description{
  \code{getCriteriaNames} returns the available clustering criteria names.
}

\usage{
  getCriteriaNames(isInternal)
}

\arguments{
  \item{isInternal}{\code{[logical]} : get internal indices if TRUE, external indices otherwise.}

}

\details{
  \code{getCriteriaNames} returns a list of the available internal or
  external clustering indices depending on the \code{isInternal} logical argument.

   The internal indices can be used in the \code{crit} argument of the
   \code{\link{intCriteria}} function and the external indices similarly in
   the \code{\link{extCriteria}} function.
}

\value{
  A character vector containing the supported criteria names.
}

\section{Author}{
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre\cr
  Lab Modal'X (EA 3454)
}

\references{
See the bibliography at the end of the vignette.
}

\seealso{
\code{\link{intCriteria}}, \code{\link{extCriteria}},
\code{\link{bestCriterion}}.
}

\examples{
getCriteriaNames(TRUE)
getCriteriaNames(FALSE)
}

