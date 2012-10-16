# ===========================================================================
# File: "runit_log_ss_ratio.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.log_ss_ratio <- function() {
	dataPath <- file.path(.path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("Log_SS_Ratio"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",3.40114842491597,"\n"))
	checkEqualsNumeric(idx[[1]],3.40114842491597)
}


