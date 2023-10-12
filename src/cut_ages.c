#include <R.h>
#include <Rinternals.h>

SEXP cut_ages(SEXP ages, SEXP breaks, SEXP max_upper) {

    // protection counter
    int protected = 0;

    // setup pointers to ages, breaks and store lengths
    int* p_ages   = INTEGER(ages);
    int* p_breaks = INTEGER(breaks);
    int  n_ages   = LENGTH(ages);
    int  n_breaks = LENGTH(breaks);

    // round max upper
    double max_upper_bound = round(asReal(max_upper));

    // calculate the maximum size we need to allocate for indexing
    int max = p_breaks[n_breaks - 1];
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age > max)
            max = age;
    }
    max++;

    // create vector of lower and upper bounds by looping over breaks using
    // index as a pointer that maps an age to a corresponding bound
    int* index;
    index = (int *) R_alloc(max, sizeof(int));

    double* lower;
    lower = (double *) R_alloc(n_breaks, sizeof(double));

    double* upper;
    upper = (double *) R_alloc(n_breaks, sizeof(double));

    int first_break = p_breaks[0];
    lower[0] = first_break;
    for (int i = 0; i < n_breaks - 1; ++i) {
        int tmp = p_breaks[i + 1];
        lower[i + 1] = tmp;
        upper[i] = tmp;
        for (int j = lower[i]; j < upper[i]; j++)
            index[j] = i;
    }

    // set the last index pointers
    for (int j = lower[n_breaks - 1]; j < max; j++)
        index[j] = n_breaks - 1;

    // set the upper bound to max_upper_bound
    upper[n_breaks - 1] = max_upper_bound;

    // create factors and output bounds corresponding to ages
    SEXP factor = PROTECT(allocVector(INTSXP, n_ages)); protected++;
    int* p_factor = INTEGER(factor);

    SEXP lower_bound = PROTECT(allocVector(REALSXP, n_ages)); protected++;
    double* p_lower_bound = REAL(lower_bound);

    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_ages)); protected++;
    double* p_upper_bound = REAL(upper_bound);

    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age == NA_INTEGER || age < first_break || age >= max_upper_bound) {
            p_factor[i] = NA_INTEGER;
            p_lower_bound[i] = NA_REAL;
            p_upper_bound[i] = NA_REAL;
        } else {
            int tmp = index[age];
            p_lower_bound[i] = lower[tmp];
            p_upper_bound[i] = upper[tmp];
            p_factor[i] = tmp + 1;
        }
    }

    // create levels
    SEXP lvls = PROTECT(allocVector(STRSXP, n_breaks)); protected++;

    // create all but the last names for the levels, "[%d,%d)"
    for (int i = 0; i < n_breaks - 1; ++i) {
        int bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_breaks[i], (int) p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_breaks[i], (int) p_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    // create last level name allowing for "[%d, Inf)"
    if (!R_FINITE(max_upper_bound)) {
        int bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    } else {
        int bufsz = snprintf(NULL, 0, "[%d, %.f)", (int) p_breaks[n_breaks - 1], max_upper_bound);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %.f)", (int) p_breaks[n_breaks - 1], max_upper_bound);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    }

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2)); protected++;
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // create list with lower and upper bound entries
    const char *names[] = {"interval", "lower_bound", "upper_bound", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names)); protected++;
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, lower_bound);
    SET_VECTOR_ELT(out, 2, upper_bound);

    // add the data frame class
    SEXP class = PROTECT(allocVector(STRSXP, 1)); protected++;
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    // this format can be seen in the R function .set_row_names()
    SEXP rnms = PROTECT(allocVector(INTSXP, 2)); protected++;
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_ages;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(protected);
    return out;
}
