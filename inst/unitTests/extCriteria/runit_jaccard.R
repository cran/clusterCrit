# ===========================================================================
# File: "runit_jaccard.R"
#                        Created: 2012-11-06 20:02:30
#              Last modification: 2012-11-06 20:02:30
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.jaccard <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsExternal100.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- extCriteria(clus_p2, clus_p3, c("Jaccard"))
	cat(paste("\nShould be =",0.25920245051384,"\n"))
	cat(paste("\nFound idx =",idx))
	checkEqualsNumeric(idx[[1]],0.25920245051384)
}


