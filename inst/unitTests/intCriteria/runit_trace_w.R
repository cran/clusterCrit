# ===========================================================================
# File: "runit_trace_w.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.trace_w <- function() {
	dataPath <- file.path(.path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("Trace_W"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",105.942129943902,"\n"))
	checkEqualsNumeric(idx[[1]],105.942129943902)
}


