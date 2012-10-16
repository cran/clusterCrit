\name{bestCriterion}
\alias{bestCriterion}

\title{Best clustering index}

\description{
  \code{bestCriterion} returns the best index value according to a specified criterion.
}

\usage{
  bestCriterion(x, crit)
}

\arguments{
  \item{x}{\code{[matrix]} : a numeric vector of quality index values.}

  \item{crit}{\code{[character]} : a string specifying the name of the criterion which was used to compute the quality indices.}

}

\details{

	Given a vector of several clustering quality index values computed
	with a given criterion, the function \code{bestCriterion} returns the
	index of the \emph{"best"} one in the sense of the specified criterion.
	Typically, a set of data has been clusterized several times (using
	different algorithms or specifying a different number of clusters) and
	a clustering index has been calculated each time : the
	\code{bestCriterion} function tells which value is considered the best
	according to the given clustering index. For instance, if one uses
	the Calinski_Harabasz index, the best value is the largest one.

	A list of all the supported criteria can be obtained with the
	\code{\link{getCriteriaNames}} function. The criterion name
	(\code{crit} argument) is case insensitive and can be abbreviated.
  
}

\value{
  The index in vector \code{x} of the best value according to the criterion
  specified by the \code{crit} argument.
}

\section{Author}{
  Bernard Desgraupes \cr
  \email{bernard.desgraupes@u-paris10.fr}\cr
  University of Paris Ouest - Nanterre\cr
  Lab Modal'X (EA 3454)
}


\seealso{
\code{\link{getCriteriaNames}}, \code{\link{intCriteria}}.
}


\examples{
# Create some spheric data around three distinct centers
x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 2, sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 4, sd = 0.5), ncol = 2))
vals <- vector()
for (k in 2:6) {
    # Perform the kmeans algorithm
    cl <- kmeans(x, k)
    # Compute the Calinski_Harabasz index
    vals <- c(vals,as.numeric(intCriteria(x,cl$cluster,"Calinski_Harabasz")))
}
idx <- bestCriterion(vals,"Calinski_Harabasz")
cat("Best index value is",vals[idx])
}


