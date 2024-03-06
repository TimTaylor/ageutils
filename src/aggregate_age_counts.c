#include <R.h>
#include <Rinternals.h>
#include "aggregate_age_counts.h"

#define IS_NUMERIC(x) (isReal(x) || isInteger(x))

SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP breaks) {

    // PROTECT counter
    int protected = 0;

    // ensure numeric input
    if (!IS_NUMERIC(counts) || LENGTH(counts) == 0)
        error("`counts` must be numeric and of length >= 1.");
    if (!IS_NUMERIC(ages) || LENGTH(ages) == 0)
        error("`ages` must be numeric and of length >= 1.");
    if (!IS_NUMERIC(breaks) || LENGTH(breaks) == 0)
        error("`breaks` must be numeric and of length >= 1.");

    // ensure ages and counts are the same length
    if (LENGTH(ages) != LENGTH(counts))
        error("`ages` and `counts` must be the same length.");

    // coerce ages to integer
    ages = PROTECT(coerceVector(ages, INTSXP)); protected++;

    // coerce counts to double
    counts = PROTECT(coerceVector(counts, REALSXP)); protected++;

    // coerce breaks to integer
    breaks = PROTECT(coerceVector(breaks, INTSXP)); protected++;

    // Ensure breaks are not NA and are in strictly increasing order
    int* p_breaks = INTEGER(breaks);
    int brk = p_breaks[0];
    if (brk == NA_INTEGER)
        error("`breaks` must be non-missing (not NA) and coercible to integer.");
    for (int i = 0; i < LENGTH(breaks) - 1; i++) {

        int next_brk = p_breaks[i + 1];

        if (next_brk == NA_INTEGER)
            error("`breaks` must be non-missing (not NA) and coercible to integer.");

        if (next_brk <= brk)
            error("`breaks` must be in strictly increasing order.");

        brk = next_brk;
    }

    // check ages are >= the first break (or NA)
    // if NA present we set a bool flag and use this later for allocating rows
    int* p_ages = INTEGER(ages);
    int na_flag = 0;
    for (int i = 0; i < LENGTH(ages); i++) {
        int age = p_ages[i];
        if (age == NA_INTEGER) {
            na_flag = 1;
        } else if  (age != NA_INTEGER && age < p_breaks[0]) {
            error("`ages` must greater than or equal to the minimum value of `breaks`.");
        }
    }

    // order by age
    // ind = order(x, nalast = TRUE, decreasing = FALSE)
    // R_orderVector1(result, length, input to sort, nalast, decreasing)
    int* ind;
    ind = (int *) R_alloc(LENGTH(ages), sizeof(int));

    int* out_ages;
    out_ages = (int *) R_alloc(LENGTH(ages), sizeof(int));

    double* p_counts = REAL(counts);
    double* out_counts;
    out_counts = (double *) R_alloc(LENGTH(ages), sizeof(double));
    R_orderVector1(ind, LENGTH(ages), ages, TRUE, FALSE);
    for (int i = 0; i < LENGTH(ages); i++) {
        out_ages[i] = p_ages[ind[i]];
        out_counts[i] = p_counts[ind[i]];
    }

    // number of breaks and groups (groups allows for optional na)
    int n_breaks = LENGTH(breaks);
    int n_groups = n_breaks + na_flag;

    // allocate output and initialise to 0
    SEXP group_counts = PROTECT(allocVector(REALSXP, n_groups)); protected++;
    double* p_groups = REAL(group_counts);
    Memzero(p_groups, n_groups);

    // calculate the other counts
    int group_index = 0;
    for (int i = 0; i < LENGTH(ages); ++i) {
        int current_age = out_ages[i];
        double tmp = out_counts[i];
        if (current_age == NA_INTEGER) {
            p_groups[n_groups - 1] += tmp;
        } else {
            while(group_index < n_breaks -1 && current_age >= p_breaks[group_index + 1])
                ++group_index;
            p_groups[group_index] += tmp;
        }
    }

    // generate the corresponding intervals
    SEXP start = PROTECT(allocVector(REALSXP, n_groups)); protected++;
    SEXP end = PROTECT(allocVector(REALSXP, n_groups)); protected++;
    double* p_start = REAL(start);
    double* p_end = REAL(end);

    SEXP factor = PROTECT(allocVector(INTSXP, n_groups)); protected++;
    SEXP lvls = PROTECT(allocVector(STRSXP, n_groups - na_flag)); protected++; // No NA level
    int* p_factor = INTEGER(factor);

    // create all but the last names for the intervals, "[%d,%d)"
    for (int i = 0; i < n_breaks - 1; ++i) {
        p_factor[i] = i + 1;
        p_start[i] = (double) p_breaks[i];
        p_end[i] = (double) p_breaks[i+1];
        // names "[%d,%d)"
        int bufsz = snprintf(NULL, 0, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    // create last name "[%d,Inf)"
    p_factor[n_breaks - 1] = n_breaks;
    p_start[n_breaks - 1] = (double) p_breaks[n_breaks - 1];
    p_end[n_breaks - 1] = R_PosInf;

    int bufsz = snprintf(NULL, 0, "[%d, Inf)", p_breaks[n_breaks - 1]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d, Inf)", p_breaks[n_breaks - 1]);
    SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
    R_Free(buf);

    if (na_flag) {
        p_start[n_groups - 1] = NA_REAL;
        p_end[n_groups - 1] = NA_REAL;
        p_factor[n_groups - 1] = NA_INTEGER;
    }

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2)); protected++;
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // array of names; note the null string
    const char *names[] = {"interval" ,"lower_bound", "upper_bound", "count", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names)); protected++;
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, start);
    SET_VECTOR_ELT(out, 2, end);
    SET_VECTOR_ELT(out, 3, group_counts);

    // add class
    SEXP class = PROTECT(allocVector(STRSXP, 1)); protected++;
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    SEXP rnms = PROTECT(allocVector(INTSXP, 2)); protected++;
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_groups;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(protected);

    return out;
}
