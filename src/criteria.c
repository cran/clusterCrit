/* 
 * ===========================================================================
 * File: "criteria.c"
 *                        Created: 2010-04-26 08:31:04
 *              Last modification: 2012-11-20 11:36:59
 * Author: Bernard Desgraupes
 * e-mail: <bernard.desgraupes@u-paris10.fr>
 * This is part of the R package 'clusterCrit'.
 * ===========================================================================
 */

#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>

#include <stdio.h>
#include <stdlib.h>
#include <float.h>

#include "criteria.h"


SEXP cluc_calculateInternalCriteria(SEXP inTraj, SEXP inPart, SEXP inCrit)
{
	int				i, critLen, err = 0;
	int				nbRows, nbCols, nbClust, nbCrit;
	int				*part, *dims;
	double			critVal;
	double			*traj;
	SEXP			result;
	const char * 	critName;
	
	PROTECT(inTraj);
	PROTECT(inPart);
	PROTECT(inCrit);
	
	/* Check the arguments */
	if (!isMatrix(inTraj)) {
		UNPROTECT(3);
		Rf_error("arg 'traj' must be a numeric matrix");
	}
	if (!( isVector(inPart) && isInteger(inPart) )) {
		UNPROTECT(3);
		Rf_error("argument 'part' must be an integer vector");
	}	
	if (TYPEOF(inCrit) != STRSXP) {
		UNPROTECT(3);
		Rf_error("argument 'crit' must be a character vector");
	}   
		
	/* Retrieve the size of the objects */
	dims = INTEGER(coerceVector(getAttrib(inTraj, R_DimSymbol), INTSXP));
	nbRows = dims[0];
	nbCols = dims[1];
	nbCrit = length(inCrit);

	/* Get the trajectories array */
	traj = REAL(inTraj);
	
	/* Get the partition array and the number of clusters */
	part = INTEGER(inPart);
	F77_CALL(cluc_count_clusters)(part, &nbRows, &nbClust);

	/* Allocate a vector for the results */
	result = PROTECT(allocVector(VECSXP, nbCrit));
	setAttrib( result, R_NamesSymbol, inCrit );	
	
	/* Initialize the statics */
	F77_CALL(cluc_calc_int_start)(&nbRows, &nbCols, &nbClust);

	/* First pass to set the flags */
	for (i = 0; i < nbCrit; i++) {
		if (STRING_ELT(inCrit, i) != NA_STRING) {
			critName = CHAR(STRING_ELT(inCrit, i));
			critLen = strlen(critName);			
			F77_CALL(cluc_int_set_flags)(critName, &critLen);
		}		
	}
	
	/* Precalculate required quantities */
	F77_CALL(cluc_int_precalc)(traj, part, &err);

	if (err == 0) {
		/* Second pass to calculate the criteria */
		for (i = 0; i < nbCrit; i++) {
			critVal = R_NaReal;
			if (STRING_ELT(inCrit, i) != NA_STRING) {
				critName = CHAR(STRING_ELT(inCrit, i));
				critLen = strlen(critName);			
				F77_CALL(cluc_calc_int_criterion)(traj, part, critName, &critLen, &err, &critVal);
				if (err != 0) {
					//Rf_error("cluscrit: error calculating '%s' criterion\n", critName);
					break;
				} 
			}		
			SET_VECTOR_ELT(result, i, ScalarReal(critVal));
		}
	} 
	
	/* Release the allocated statics */
	F77_CALL(cluc_calc_int_end)();

	UNPROTECT(4);
	if (err != 0) {
		cluc_errorMsg(err);
	} 
	return result;
}


SEXP cluc_calculateExternalCriteria(SEXP inPart1, SEXP inPart2, SEXP inCrit)
{
	int				i, critLen, err = 0;
	int				nbElem, nbClust1, nbClust2, nbCrit;
	int				*part1, *part2;
	double			critVal;
	SEXP			result;
	const char * 	critName;
	
	PROTECT(inPart1);
	PROTECT(inPart2);
	PROTECT(inCrit);
	
	/* Check the arguments */
	if (!( isVector(inPart1) && isInteger(inPart1) )) {
		UNPROTECT(3);
		Rf_error("argument 'part1' must be an integer vector");
	}	
	if (!( isVector(inPart2) && isInteger(inPart2) )) {
		UNPROTECT(3);
		Rf_error("argument 'part2' must be an integer vector");
	}	
	if (TYPEOF(inCrit) != STRSXP) {
		UNPROTECT(3);
		Rf_error("argument 'crit' must be a character vector");
	}   
	if (length(inPart1) != length(inPart2)) {
		UNPROTECT(3);
		Rf_error("'part1' and 'part2' must have the same length");
	}	
		
	/* Retrieve the size of the objects */
	nbElem = length(inPart1);
	nbCrit = length(inCrit);
	
	/* Get the partition array and the number of clusters */
	part1 = INTEGER(inPart1);
	F77_CALL(cluc_count_clusters)(part1, &nbElem, &nbClust1);
	part2 = INTEGER(inPart2);
	F77_CALL(cluc_count_clusters)(part2, &nbElem, &nbClust2);

	/* Allocate a vector for the results */
	result = PROTECT(allocVector(VECSXP, nbCrit));
	setAttrib( result, R_NamesSymbol, inCrit );	
	
	/* Initialize the statics */
	F77_CALL(cluc_calc_ext_start)(&nbElem, &nbClust1, &nbClust2);

	for (i = 0; i < nbCrit; i++) {
		critVal = R_NaReal;
		if (STRING_ELT(inCrit, i) != NA_STRING) {
			critName = CHAR(STRING_ELT(inCrit, i));
			critLen = strlen(critName);			
			F77_CALL(cluc_calc_ext_criterion)(part1, part2, critName, &critLen, &err, &critVal);
			if (err != 0) {
				//Rf_error("cluscrit: error calculating '%s' criterion\n", critName);
				break;
			} 
		}		
		SET_VECTOR_ELT(result, i, ScalarReal(critVal));
	}
	
	/* Release the allocated statics */
	F77_CALL(cluc_calc_ext_end)();

	UNPROTECT(4);
	if (err != 0) {
		cluc_errorMsg(err);
	} 
	
	return result;
}


SEXP cluc_calculateConcordances(SEXP inPart1, SEXP inPart2)
{
	int				nbElem;
	int				*part1, *part2;
	int				concMat[2][2];
	SEXP			result, names, dmns;
	
	PROTECT(inPart1);
	PROTECT(inPart2);
	
	/* Check the arguments */
	if (!( isVector(inPart1) && isInteger(inPart1) )) {
		UNPROTECT(2);
		Rf_error("argument 'part1' must be an integer vector");
	}	
	if (!( isVector(inPart2) && isInteger(inPart2) )) {
		UNPROTECT(2);
		Rf_error("argument 'part2' must be an integer vector");
	}	
	if (length(inPart1) != length(inPart2)) {
		UNPROTECT(2);
		Rf_error("'part1' and 'part2' must have the same length");
	}	
		
	/* Retrieve the size of the objects */
	nbElem = length(inPart1);
	
	/* Get the partition array and the number of clusters */
	part1 = INTEGER(inPart1);
	part2 = INTEGER(inPart2);

	/* Allocate a vector for the results */
	PROTECT(result = allocMatrix(INTSXP, 2, 2));
    PROTECT(names = allocVector(STRSXP, 2));
    PROTECT(dmns = allocVector(VECSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("y"));
    SET_STRING_ELT(names, 1, mkChar("n"));
	SET_VECTOR_ELT(dmns, 0, names);
	SET_VECTOR_ELT(dmns, 1, names);
    setAttrib(result, R_DimNamesSymbol, dmns);
	
	F77_CALL(cluc_calc_concordance)(part1, part2, &nbElem, concMat);
	INTEGER(result)[0] = concMat[0][0];
	INTEGER(result)[1] = concMat[0][1];
	INTEGER(result)[2] = concMat[1][0];
	INTEGER(result)[3] = concMat[1][1];

	/* Release allocated arrays */
	F77_CALL(cluc_calc_ext_end)();

	UNPROTECT(5);
	
	return result;
}


void cluc_errorMsg(int inErr) {
	char	msg[128];
	char *	errStr;
	
	switch (inErr) {
		case 1:
		errStr = "unknown criterion";
		break;
		
		case 2:
		errStr = "invalid parameter";
		break;
		
		case 3:
		errStr = "memory alloc failed";
		break;
		
		case 4:
		errStr = "empty partition subset";
		break;
		
		case 5:
		errStr = "criterion not implemented";
		break;
		
		default:
		errStr = "unknown error";
		break;
	}
	
	sprintf(msg,"cluscrit: error (%d) -> %s\n", inErr, errStr);
	Rf_error(msg);
}
