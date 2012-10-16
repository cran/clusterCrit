\name{concordance}
\alias{concordance}

\title{Compute Concordance Matrix}

\description{
  \code{concordance} calculates the concordance matrix between two partitions
  of the same data.
}

\usage{
  concordance(part1, part2)
}

\arguments{
  \item{part1}{\code{[vector]} : the first partition vector.}

  \item{part2}{\code{[vector]} : the second partition vector.}

}

\details{

  Given two partitions, the function \code{concordance} calculates the
  number of pairs classified as belonging or not belonging to the same
  cluster with respect to partitions \code{part1} or \code{part2}.
   
}

\value{
  A 2x2 matrix of the form :
\preformatted{
      |  P1   |  P2   |
  ____________________
   P1 |  Nyy  |  Nyn  |
   P2 |  Nny  |  Nnn  |
  ____________________
}
where
  \itemize{
	  \item \code{Nyy} is the number of points belonging to the same cluster both in \code{part1} and \code{part2}
	  \item \code{Nyn} is the number of points belonging to the same cluster in \code{part1} but not in \code{part2}
	  \item \code{Nny} is the number of points belonging to the same cluster in \code{part2} but not in \code{part1}
	  \item \code{Nnn} is the number of points \emph{not} belonging to the same cluster both in \code{part1} and \code{part2}
  }
}

\section{Author}{
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre\cr
  Lab Modal'X (EA 3454)
}

\seealso{
\code{\link{extCriteria}}, \code{\link{intCriteria}}.
}


\examples{
# Generate two artificial partitions
part1<-sample(1:3,150,replace=TRUE)
part2<-sample(1:5,150,replace=TRUE)

# Compute the table of concordances and discordances
concordance(part1,part2)
}


