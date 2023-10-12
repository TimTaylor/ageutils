#include <R.h>
#include <Rinternals.h>

SEXP cut_ages(SEXP ages, SEXP breaks, SEXP max_upper, SEXP max_bound) {


    int n_breaks = LENGTH(breaks);
    int n_ages = LENGTH(ages);
    int max = INTEGER(max_bound)[0];
    int* p_ages = INTEGER(ages);
    int* p_breaks= INTEGER(breaks);
    int first_break = p_breaks[0];

    // create vector of lower and upper bounds by looping over breaks using
    // index as a pointer that maps an age to a corresponding bound
    int* index;
    index = (int *) R_alloc(INTEGER(max_bound)[0], sizeof(int));

    double* lower;
    lower = (double *) R_alloc(n_breaks, sizeof(double));

    double* upper;
    upper = (double *) R_alloc(n_breaks, sizeof(double));

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

    // set the upper bound to max_upper
    upper[n_breaks - 1] = asReal(max_upper);

    // create factors and output bounds corresponding to ages
    SEXP factor = PROTECT(allocVector(INTSXP, n_ages));
    int* p_factor = INTEGER(factor);

    SEXP lower_bound = PROTECT(allocVector(REALSXP, n_ages));
    double* p_lower_bound = REAL(lower_bound);

    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_ages));
    double* p_upper_bound = REAL(upper_bound);

    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age == NA_INTEGER || age < first_break || age >= max) {
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
    SEXP lvls = PROTECT(allocVector(STRSXP, n_breaks));

    // create all but the last names for the levels, "[%d,%d)"
    for (int i = 0; i < n_breaks - 1; ++i) {
        int bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_breaks[i], (int) p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_breaks[i], (int) p_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    // create last level name allowing for "[%d, Inf)"
    double max_tmp = asReal(max_upper);
    if (!R_FINITE(max_tmp)) {
        int bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    } else {
        int bufsz = snprintf(NULL, 0, "[%d, %.f)", (int) p_breaks[n_breaks - 1], max_tmp);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %.f)", (int) p_breaks[n_breaks - 1], max_tmp);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    }

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // create list with lower and upper bound entries
    const char *names[] = {"interval", "lower_bound", "upper_bound", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, lower_bound);
    SET_VECTOR_ELT(out, 2, upper_bound);

    // add the data frame class
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    // this format can be seen in the R function .set_row_names()
    SEXP rnms = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_ages;
    setAttrib(out, R_RowNamesSymbol, rnms);


    UNPROTECT(8);
    return out;
}
