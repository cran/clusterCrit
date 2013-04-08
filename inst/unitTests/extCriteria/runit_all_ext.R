# ===========================================================================
# File: "runit_all_ext.R"
#                        Created: 2012-11-06 20:02:30
#              Last modification: 2012-11-06 20:02:30
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# Unit test file for the R package clusterCrit.
# ===========================================================================



test.all_external_criteria <- function() {
	dataPath <- file.path(path.package(package="clusterCrit"),"unitTests","data","testsExternal100.Rdata")
	load(file=dataPath, envir=.GlobalEnv)
	idx <- extCriteria(clus_p2, clus_p3, "all")
	
	cnames <- tolower(getCriteriaNames(FALSE))
	values <- c(0.411693066358566,
				0.419803321361542,
				0.022714141794819,
				0.25920245051384,
				0.428073406219482,
				17.6941604614258,
				3.93253019506888e-09,
				0.511811017990112,
				0.512121200561523,
				0.34433576464653,
				0.282577193178018,
				0.170707076787949,
				0.11605548688367,
				0.611727799227799
		    )
	
	result <- as.list(values)
	names(result) <- cnames
	
	checkEquals(idx,result)
}


