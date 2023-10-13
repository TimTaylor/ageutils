#include <R.h>
#include <Rinternals.h>
#include "aggregate_age_counts.h"

SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP breaks) {

    int protected = 0;

    int n_ages = LENGTH(ages);
    int* p_ages = INTEGER(ages);
    int n_breaks = LENGTH(breaks);
    int* p_breaks = INTEGER(breaks);

    // order by age
    // ind = order(x, nalast = TRUE, decreasing = FALSE)
    // R_orderVector1(result, length, input to sort, nalast, decreasing)
    int* ind;
    ind = (int *) R_alloc(n_ages, sizeof(int));

    int* out_ages;
    out_ages = (int *) R_alloc(n_ages, sizeof(int));

    double* p_counts = REAL(counts);
    double* out_counts;
    out_counts = (double *) R_alloc(n_ages, sizeof(double));
    R_orderVector1(ind, n_ages, ages, TRUE, FALSE);
    for (int i = 0; i < n_ages; i++) {
        out_ages[i] = p_ages[ind[i]];
        out_counts[i] = p_counts[ind[i]];
    }

    // number of groups (allowing for an NA group)
    int n_groups = n_breaks + 1;

    // allocate output and initialise to 0
    SEXP group_counts = PROTECT(allocVector(REALSXP, n_groups)); protected++;
    double* p_groups = REAL(group_counts);
    Memzero(p_groups, n_groups);

    // Calculate the NA values when ages are below the first break
    int j = 0;
    int a = out_ages[j];
    while (a < p_breaks[0]) {
        double tmp = out_counts[j];
        p_groups[n_breaks] += tmp;
        j++;
        if (j >= n_ages)
            break;
        a = out_ages[j];
    }

    // calculate the other counts
    int group_index = 0;
    for (int i = j; i < n_ages; ++i) {
        int current_age = out_ages[i];
        double tmp = out_counts[i];
        if (current_age == NA_INTEGER) {
            p_groups[n_breaks] += tmp;
        } else {
            while(group_index < n_groups - 2 && current_age >= p_breaks[group_index + 1])
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
    SEXP lvls = PROTECT(allocVector(STRSXP, n_groups - 1)); protected++; // No NA level
    int* p_factor = INTEGER(factor);

    // create all but the last names for the intervals, "[%d,%d)"
    for (int i=0; i < n_groups - 1; ++i) {
        p_factor[i] = i+1;
        p_start[i] = p_breaks[i];
        p_end[i] = p_breaks[i+1];
        // names "[%d,%d)"
        int bufsz = snprintf(NULL, 0, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    p_end[n_groups - 2] = R_PosInf;
    p_start[n_groups - 1] = NA_REAL;
    p_end[n_groups - 1] = NA_REAL;
    p_factor[n_groups - 1] = NA_INTEGER;

    // create last name "[%d,Inf)"
    int bufsz = snprintf(NULL, 0, "[%d, Inf)", p_breaks[n_groups-2]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d, Inf)", p_breaks[n_groups-2]);
    SET_STRING_ELT(lvls, n_groups-2, mkChar(buf));
    R_Free(buf);

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
