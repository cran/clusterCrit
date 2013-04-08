# ===========================================================================
# File: "runit_sd_dis.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.sd_dis <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("SD_Dis"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",0.424825246347848,"\n"))
	checkEqualsNumeric(idx[[1]],0.424825246347848)
}


