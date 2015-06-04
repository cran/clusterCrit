# ===========================================================================
# File: "Makefile"
#                        Created: 2012-11-01 18:37:16
#              Last modification: 2012-11-13 14:35:53
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# This is part of the R package clusterCrit.
# ===========================================================================


# $(info PKG_DIR: ${PKG_DIR})
# $(info TOP_DIR: ${TOP_DIR})
# $(info PKG_DIRNAME: ${PKG_DIRNAME})

SINGLE_CRIT_BENCHNAME=benchmarkOne.R
ALL_CRIT_BENCHNAME=benchmarkAll.R


all: benchs


benchs:
	@echo "Running all benchmarks..."
	@R --vanilla --slave < ${ALL_CRIT_BENCHNAME}


bench-%:
	@echo "Running benchmark for '$*'..."
	@echo "source(\"${BENCH_DIR}/${SINGLE_CRIT_BENCHNAME}\")" | R --vanilla --slave --args $*


