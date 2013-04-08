# ===========================================================================
# File: "runit_gdi42.R"
#                        Created: 2012-11-13 11:28:57
#              Last modification: 2012-11-13 11:28:57
# Author: Bernard Desgraupes
# e-mail: <>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.gdi42 <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsInternal_400_4.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- intCriteria(traj_400_4, part_400_4[[4]], c("GDI42"))
	cat(paste("\nFound idx =",idx))
	cat(paste("\nShould be =",11.0668151243083,"\n"))
	checkEqualsNumeric(idx[[1]],11.0668151243083)
}


