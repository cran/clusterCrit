# ===========================================================================
# File: "Makefile"
#                        Created: 2012-11-01 18:37:16
#              Last modification: 2012-11-02 16:09:22
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# This is part of the R package clusterCrit.
# ===========================================================================

TEST_DIR=$(shell pwd)
INST_DIR=$(shell dirname ${TEST_DIR})
PKG_DIR=$(shell dirname ${INST_DIR})
TOP_DIR=$(shell dirname ${PKG_DIR})
PKG_DIRNAME=$(shell basename ${PKG_DIR})

# $(info PKG_DIR: ${PKG_DIR})
# $(info TOP_DIR: ${TOP_DIR})
# $(info PKG_DIRNAME: ${PKG_DIRNAME})

SINGLE_CRIT_TESTNAME=runOneTest.R
ALL_CRIT_TESTNAME=runAllTests.R


all: install tests


install:
	cd ${TOP_DIR}; \
	R64 CMD INSTALL --clean ${PKG_DIRNAME} && \
	cd ${TEST_DIR}


tests:
	@echo "Running all tests"
	R64 --vanilla --slave < ${ALL_CRIT_TESTNAME}


test-%:
	@echo "Running test for '$*'"
	echo "source(\"${TEST_DIR}/${SINGLE_CRIT_TESTNAME}\")" | R64 --vanilla --args $*


