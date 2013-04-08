# ===========================================================================
# File: "runit_log_det_ratio.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.log_det_ratio <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("Log_Det_Ratio"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",2748.52288830593,"\n"))
	checkEqualsNumeric(idx[[1]],2748.52288830593)
}


