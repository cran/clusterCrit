\name{intCriteria}
\alias{intCriteria}

\title{Compute internal clustering criteria}

\description{
  \code{intCriteria} calculates various internal clustering validation or
  quality criteria.
}

\usage{
  intCriteria(traj, part, crit)
}

\arguments{
  \item{traj}{\code{[matrix]} : the matrix of observations (trajectories).}

  \item{part}{\code{[vector]} : the partition vector.}

  \item{crit}{\code{[vector]} : a vector containing the names of the
  indices to compute.}

}

\details{

  The function \code{intCriteria} calculates internal clustering indices.
  The list of all the supported criteria can be obtained with the
  \code{\link{getCriteriaNames}} function.
  
  The currently available indices are :
  \itemize{
	  \item \code{"Ball_Hall"}
	  \item \code{"Banfeld_Raftery"}
	  \item \code{"C_index"}
	  \item \code{"Calinski_Harabasz"}
	  \item \code{"Davies_Bouldin"}
	  \item \code{"Det_Ratio"}
	  \item \code{"Dunn"}
	  \item \code{"Gamma"}
	  \item \code{"G_plus"}
	  \item \code{"GDI11"}
	  \item \code{"GDI12"}
	  \item \code{"GDI13"}
	  \item \code{"GDI21"}
	  \item \code{"GDI22"}
	  \item \code{"GDI23"}
	  \item \code{"GDI31"}
	  \item \code{"GDI32"}
	  \item \code{"GDI33"}
	  \item \code{"GDI41"}
	  \item \code{"GDI42"}
	  \item \code{"GDI43"}
	  \item \code{"GDI51"}
	  \item \code{"GDI52"}
	  \item \code{"GDI53"}
	  \item \code{"Ksq_DetW"}
	  \item \code{"Log_Det_Ratio"}
	  \item \code{"Log_SS_Ratio"}
	  \item \code{"McClain_Rao"}
	  \item \code{"PBM"}
	  \item \code{"Point_Biserial"}
	  \item \code{"Ray_Turi"}
	  \item \code{"Ratkowsky_Lance"}
	  \item \code{"Scott_Symons"}
	  \item \code{"SD_Scat"}
	  \item \code{"SD_Dis"}
	  \item \code{"S_Dbw"}
	  \item \code{"Silhouette"}
	  \item \code{"Tau"}
	  \item \code{"Trace_W"}
	  \item \code{"Trace_WiB"}
	  \item \code{"Wemmert_Gancarski"}
	  \item \code{"Xie_Beni"}
	}

				
   All the names are case insensitive and can be abbreviated. The keyword
   \code{"all"} can also be used as a shortcut to calculate all the
   internal indices.
   
   The \acronym{GDI} (\emph{Generalized Dunn Indices}) are designated by
   the following convention: GDI\emph{mn}, where the integers \emph{m}
   (\emph{1<=m<=5}) and \emph{n} (\emph{1<=n<=3}) correspond to the
   between-group and within-group distances respectively. See the vignette
   for a comprehensive definition of the various distances. \code{GDI}
   alone is synonym of \code{GDI11} and is the genuine Dunn's index.
   
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
\code{\link{getCriteriaNames}}, \code{\link{extCriteria}},
\code{\link{bestCriterion}}.
}


\examples{
# Create some data
x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 2, sd = 0.5), ncol = 2))
# Perform the kmeans algorithm
cl <- kmeans(x, 3)
# Compute all the internal indices
intCriteria(x,cl$cluster,"all")
# Compute some of them
intCriteria(x,cl$cluster,c("C_index","Calinski_Harabasz","Dunn"))
# The names are case insensitive and can be abbreviated
intCriteria(x,cl$cluster,c("det","cal","dav"))
}


