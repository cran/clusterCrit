/* 
 * ===========================================================================
 * File: "criteria.c"
 *                        Created: 2010-04-26 08:31:04
 *              Last modification: 2018-07-26 15:17:52
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

#define CRIT_MAX_LENGTH 32

	// DO NOT MODIFY THE ORDER OF THIS STRINGS, cluc_calc_int_criterion AND
	// cluc_int_set_flags RELY ON IT.
	// FUTURE NEW INDICES SHOULD BE APPENDED.
	static const char * sIntCritNames[] = {
		"ball_hall",        	"banfeld_raftery",	"c_index",        	"calinski_harabasz",
		"davies_bouldin",   	"det_ratio",      	"dunn",           	"g_plus",
		"gamma",            	"gdi11",          	"gdi12",          	"gdi13",
		"gdi21",            	"gdi22",          	"gdi23",          	"gdi31",
		"gdi32",            	"gdi33",          	"gdi41",          	"gdi42",
		"gdi43",            	"gdi51",          	"gdi52",          	"gdi53",
		"ksq_detw",         	"log_det_ratio",  	"log_ss_ratio",   	"mcclain_rao",
		"pbm",              	"point_biserial", 	"ratkowsky_lance",	"ray_turi",
		"s_dbw",            	"scott_symons",   	"sd_dis",         	"sd_scat",
		"silhouette",       	"tau",            	"trace_w",        	"trace_wib",
		"wemmert_gancarski",	"xie_beni",       	(char *) NULL      
	};

	enum {
		INTCRIT_BALL_HALL,        	INTCRIT_BANFELD_RAFTERY,	INTCRIT_C_INDEX,        	INTCRIT_CALINSKI_HARABASZ,
		INTCRIT_DAVIES_BOULDIN,   	INTCRIT_DET_RATIO,      	INTCRIT_DUNN,           	INTCRIT_G_PLUS,           
		INTCRIT_GAMMA,            	INTCRIT_GDI11,          	INTCRIT_GDI12,          	INTCRIT_GDI13,            
		INTCRIT_GDI21,            	INTCRIT_GDI22,          	INTCRIT_GDI23,          	INTCRIT_GDI31,            
		INTCRIT_GDI32,            	INTCRIT_GDI33,          	INTCRIT_GDI41,          	INTCRIT_GDI42,            
		INTCRIT_GDI43,            	INTCRIT_GDI51,          	INTCRIT_GDI52,          	INTCRIT_GDI53,            
		INTCRIT_KSQ_DETW,         	INTCRIT_LOG_DET_RATIO,  	INTCRIT_LOG_SS_RATIO,   	INTCRIT_MCCLAIN_RAO,      
		INTCRIT_PBM,              	INTCRIT_POINT_BISERIAL, 	INTCRIT_RATKOWSKY_LANCE,	INTCRIT_RAY_TURI,         
		INTCRIT_S_DBW,            	INTCRIT_SCOTT_SYMONS,   	INTCRIT_SD_DIS,         	INTCRIT_SD_SCAT,          
		INTCRIT_SILHOUETTE,       	INTCRIT_TAU,            	INTCRIT_TRACE_W,        	INTCRIT_TRACE_WIB,        
		INTCRIT_WEMMERT_GANCARSKI,	INTCRIT_XIE_BENI        
	};


	static const char * sExtCritNames[] = {
		"czekanowski_dice",	"folkes_mallows",	"hubert",         	"jaccard",
		"kulczynski",      	"mcnemar",       	"phi",            	"precision",
		"rand",            	"recall",        	"rogers_tanimoto",	"russel_rao",
		"sokal_sneath1",   	"sokal_sneath2", 	(char *) NULL      
	};

	enum {
		EXTCRIT_CZEKANOWSKI_DICE,	EXTCRIT_FOLKES_MALLOWS,	EXTCRIT_HUBERT,         	EXTCRIT_JACCARD,   
		EXTCRIT_KULCZYNSKI,      	EXTCRIT_MCNEMAR,       	EXTCRIT_PHI,            	EXTCRIT_PRECISION, 
		EXTCRIT_RAND,            	EXTCRIT_RECALL,        	EXTCRIT_ROGERS_TANIMOTO,	EXTCRIT_RUSSEL_RAO,
		EXTCRIT_SOKAL_SNEATH1,   	EXTCRIT_SOKAL_SNEATH2  
	};


static const R_CallMethodDef CallEntries[] = {
	{"cluc_calculateInternalCriteria", (DL_FUNC) &cluc_calculateInternalCriteria, 3},
	{"cluc_calculateExternalCriteria", (DL_FUNC) &cluc_calculateExternalCriteria, 3},
	{"cluc_calculateConcordances", (DL_FUNC) &cluc_calculateConcordances, 2},
    {NULL, NULL, 0}
};


/* 
 * ------------------------------------------------------------------------
 * 
 * "R_init_clusterCrit()" --
 * 
 * This function is automatically called by R when the package is loaded.
 * It ensures efficient dispatch of calls to C functions (see Writing R
 * extensions, § 5.4 Registering native routines).
 * 
 * ------------------------------------------------------------------------
 */
void
R_init_clusterCrit(DllInfo * info)
{
    R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}


/* 
 * ------------------------------------------------------------------------
 * 
 * "SEXP cluc_calculateInternalCriteria(SEXP inTraj, SEXP inPart, SEXP inCrit)" --
 * 
 * The calling R proc has already checked that 
 *    1- the 'traj' argument is a matrix
 *    2- the 'part' argument is an integer vector with values ranging
 *       sequentially from 1
 * 
 * ------------------------------------------------------------------------
 */
SEXP cluc_calculateInternalCriteria(SEXP inTraj, SEXP inPart, SEXP inCrit)
{
	int				i, idx, err = 0;
	int				nbRows, nbCols, nbClust, nbCrit;
	int				*part, *dims;
	double			critVal;
	double			*traj;
	SEXP			result;
	const char * 	str;
	
	PROTECT(inTraj);
	PROTECT(inPart);
	PROTECT(inCrit);
	
	/* Check the arguments */
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
			str = CHAR(STRING_ELT(inCrit, i));
			idx = cluc_getIndexFromName(str, sIntCritNames);
			if (idx == -1) {
				UNPROTECT(4);
				cluc_errorMsg(1);
			} 
			F77_CALL(cluc_int_set_flags)(&idx);			
		}		
	}
	
	/* Precalculate required quantities */
	F77_CALL(cluc_int_precalc)(traj, part, &err);

	if (err == 0) {
		/* Second pass to calculate the criteria */
		for (i = 0; i < nbCrit; i++) {
			critVal = R_NaReal;
			if (STRING_ELT(inCrit, i) != NA_STRING) {
				str = CHAR(STRING_ELT(inCrit, i));
				idx = cluc_getIndexFromName(str, sIntCritNames);
				if (idx == -1) {
					err = 1;
					break;
				} 
				F77_CALL(cluc_calc_int_criterion)(traj, part, &idx, &err, &critVal);
				if (err != 0) {
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


/* 
 * ------------------------------------------------------------------------
 * 
 * "SEXP cluc_calculateExternalCriteria(SEXP inPart1, SEXP inPart2, SEXP inCrit)" --
 * 
 * The calling R proc has already checked that the 'part1' and 'part2'
 * arguments are integer vectors with values ranging sequentially from 1
 * and that they have the same length.
 * 
 * ------------------------------------------------------------------------
 */
SEXP cluc_calculateExternalCriteria(SEXP inPart1, SEXP inPart2, SEXP inCrit)
{
	int				i, idx, err = 0;
	int				nbElem, nbClust1, nbClust2, nbCrit;
	int				*part1, *part2;
	double			critVal;
	SEXP			result;
	const char * 	str;
	
	PROTECT(inPart1);
	PROTECT(inPart2);
	PROTECT(inCrit);
	
	/* Check the arguments */
	if (TYPEOF(inCrit) != STRSXP) {
		UNPROTECT(3);
		Rf_error("argument 'crit' must be a character vector");
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
			str = CHAR(STRING_ELT(inCrit, i));
			idx = cluc_getIndexFromName(str, sExtCritNames);
			if (idx == -1) {
				err = 1;
				break;
			} 
			F77_CALL(cluc_calc_ext_criterion)(part1, part2, &idx, &err, &critVal);
			if (err != 0) {
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
	
	Rf_error("cluscrit: error (%d) -> %s\n", inErr, errStr);
}


int
cluc_getIndexFromName(const char * inName, const char *tablePtr[])
{
	int		idx = -1, i = 0;
	char *	p = (char*)tablePtr[0];

	while (p != NULL) {
		if (strcmp(inName, p) == 0) {
			idx = i;
			break;
		} 
		p = (char*)tablePtr[++i];
	}
	
	return idx;
}





