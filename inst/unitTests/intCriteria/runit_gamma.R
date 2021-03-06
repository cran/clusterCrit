# ===========================================================================
# File: "runit_gamma.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2016-05-27 11:04:52
# Author: Bernard Desgraupes
# e-mail: <bernard.desgraupes@u-paris10.fr>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.gamma <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("Gamma"))
	cat(paste("\nFound idx =",idx))
	val <- 0.99999988079071
	cat(paste("\nShould be =",val,"\n"))
	checkEqualsNumeric(idx[[1]],val)
}


