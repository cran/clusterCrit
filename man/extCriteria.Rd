\name{extCriteria}
\alias{extCriteria}

\title{Compute external clustering criteria}

\description{
  \code{extCriteria} calculates various external clustering comparison indices.
}

\usage{
  extCriteria(part1, part2, crit)
}

\arguments{
  \item{part1}{\code{[vector]} : the first partition vector.}

  \item{part2}{\code{[vector]} : the second partition vector.}

  \item{crit}{\code{[vector]} : a vector containing the names of the
  indices to compute.}

}

\details{
  The function \code{extCriteria} calculates external clustering indices in
  order to compare two partitions.
  The list of all the supported criteria can be obtained with the
  \code{\link{getCriteriaNames}} function.
  
  The currently available indices are :
  \itemize{
	  \item \code{"Czekanowski_Dice"}
	  \item \code{"Folkes_Mallows"} 
	  \item \code{"Hubert"}
	  \item \code{"Jaccard"} 
	  \item \code{"Kulczynski"} 
	  \item \code{"McNemar"}
	  \item \code{"Phi"} 
	  \item \code{"Precision"} 
	  \item \code{"Rand"} 
	  \item \code{"Recall"} 
	  \item \code{"Rogers_Tanimoto"}
	  \item \code{"Russel_Rao"}
	  \item \code{"Sokal_Sneath1"}
	  \item \code{"Sokal_Sneath2"}
  }
  
   All the names are case insensitive and can be abbreviated. The keyword
   \code{"all"} can also be used as a shortcut to calculate all the
   external indices.
   
   The partition vectors should not have empty subsets. No attempt is made
   to verify this.
   
}

\value{
  A list containing the computed criteria, in the same order as in the
  \code{crit} argument.
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
\code{\link{getCriteriaNames}}, \code{\link{intCriteria}},
\code{\link{bestCriterion}},
\code{\link{concordance}}.
}

\examples{
# Generate two artificial partitions
part1<-sample(1:3,150,replace=TRUE)
part2<-sample(1:5,150,replace=TRUE)

# Compute all the external indices
extCriteria(part1,part2,"all")
# Compute some of them
extCriteria(part1,part2,c("Rand","Folkes"))
# The names are case insensitive and can be abbreviated
extCriteria(part1,part2,c("ra","fo"))
}

