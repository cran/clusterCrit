# ===========================================================================
# File: "runit_sokal_sneath1.R"
#                        Created: 2012-11-06 20:02:30
#              Last modification: 2012-11-06 20:02:30
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.sokal_sneath1 <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsExternal100.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- extCriteria(clus_p2, clus_p3, c("Sokal_Sneath1"))
	cat(paste("\nShould be =",0.11605548688367,"\n"))
	cat(paste("\nFound idx =",idx))
	checkEqualsNumeric(idx[[1]],0.11605548688367)
}


