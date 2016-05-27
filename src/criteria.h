// ===========================================================================
// File: "criteria.h"
//                        Created: 2010-04-26 08:31:04
//              Last modification: 2016-05-26 19:00:45
// Author: Bernard Desgraupes
// e-mail: <bernard.desgraupes@u-paris10.fr>
// This is part of the R package 'clusterCrit'.
// ===========================================================================


#ifndef	CRITERIA_H
#define CRITERIA_H
#pragma once



// Prototypes
// ----------

SEXP	cluc_calculateInternalCriteria(SEXP inTraj, SEXP inPart, SEXP inCrit);

SEXP	cluc_calculateExternalCriteria(SEXP inPart1, SEXP inPart2, SEXP inCrit);

SEXP	cluc_calculateConcordances(SEXP inPart1, SEXP inPart2);

void	cluc_errorMsg(int inErr);

int		cluc_getIndexFromName(const char * inName, const char **tablePtr);


// Fortran routines
// ----------------
void F77_NAME(cluc_vector_trace)(double* a, int* n, double* result);
void F77_NAME(cluc_matrix_trace)(double* a, int* n, double* result);

void F77_NAME(cluc_calc_int_start)(int* inRows, int* inCols, int* inPart);
void F77_NAME(cluc_calc_int_end)();

void F77_NAME(cluc_calc_ext_start)(int* inRows, int* inNbClust1, int* inNbClust2);
void F77_NAME(cluc_calc_ext_end)();

void F77_NAME(cluc_count_clusters)(int* a, int* n, int* result);
void F77_NAME(cluc_int_set_flags)(int* inIndex);
void F77_NAME(cluc_int_precalc)(double* inTraj, int* inPart, int* outErr);

void F77_NAME(cluc_calc_int_criterion)(double* inTraj, int* inPart, int* inIndex, int* outErr, double* outRes);
void F77_NAME(cluc_calc_ext_criterion)(int* inPart1, int* inPart2, int* inIndex, int* outErr, double* outRes);

void F77_NAME(cluc_calc_concordance)(int* inPart1, int* inPart2, int* inRows, int confMat[2][2]);



#endif  // CRITERIA_H
