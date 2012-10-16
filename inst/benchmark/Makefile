# ===========================================================================
# File: "Makefile"
#                        Created: 2012-11-01 18:37:16
#              Last modification: 2012-11-13 14:35:53
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# This is part of the R package clusterCrit.
# ===========================================================================

BENCH_DIR=$(shell pwd)
INST_DIR=$(shell dirname ${BENCH_DIR})
PKG_DIR=$(shell dirname ${INST_DIR})
TOP_DIR=$(shell dirname ${PKG_DIR})
PKG_DIRNAME=$(shell basename ${PKG_DIR})

# $(info PKG_DIR: ${PKG_DIR})
# $(info TOP_DIR: ${TOP_DIR})
# $(info PKG_DIRNAME: ${PKG_DIRNAME})

SINGLE_CRIT_BENCHNAME=benchmarkOne.R
ALL_CRIT_BENCHNAME=benchmarkAll.R


all: install benchs


install:
	cd ${TOP_DIR}; \
	R64 CMD INSTALL --clean ${PKG_DIRNAME} && \
	cd ${BENCH_DIR}


benchs:
	@echo "Running all benchmarks..."
	@R64 --vanilla --slave < ${ALL_CRIT_BENCHNAME}


bench-%:
	@echo "Running benchmark for '$*'..."
	@echo "source(\"${BENCH_DIR}/${SINGLE_CRIT_BENCHNAME}\")" | R64 --vanilla --slave --args $*


