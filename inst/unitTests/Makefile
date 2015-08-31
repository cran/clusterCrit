# ===========================================================================
# File: "Makefile"
#                        Created: 2012-11-01 18:37:16
#              Last modification: 2013-04-19 15:00:52
# Author: Bernard Desgraupes
# e-mail: <bdesgraupes@users.sourceforge.net>
# This is part of the R package clusterCrit.
# ===========================================================================
# Examples
#     make tests
#     make crit-calinski
#     make test-bestCriterion


# $(info PKG_DIR: ${PKG_DIR})
# $(info TOP_DIR: ${TOP_DIR})
# $(info PKG_DIRNAME: ${PKG_DIRNAME})

SINGLE_CRIT_NAME=runOneCrit.R
SINGLE_TEST_NAME=runOneTest.R
ALL_CRIT_TESTNAME=runAllTests.R
TEST_DIR=.

tests:
	@echo "Running all tests"
	@R --vanilla --slave < ${ALL_CRIT_TESTNAME}


test-%:
	@echo "Running test for '$*'"
	@echo "source(\"${TEST_DIR}/${SINGLE_TEST_NAME}\")" | R --vanilla --slave --args $*


crit-%:
	@echo "Running test for criterion '$*'"
	@echo "source(\"${TEST_DIR}/${SINGLE_CRIT_NAME}\")" | R --vanilla --slave --args $*


all: tests


help:
	@echo "Examples:"
	@echo "  make tests"
	@echo "  make crit-calinski"
	@echo "  make test-bestCriterion"
