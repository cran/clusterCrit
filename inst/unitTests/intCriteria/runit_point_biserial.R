# ===========================================================================
# File: "runit_point_biserial.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.point_biserial <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("Point_Biserial"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",-1.6928719863069,"\n"))
	checkEqualsNumeric(idx[[1]],-1.6928719863069)
}


