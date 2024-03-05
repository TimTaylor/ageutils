#include <R.h>
#include <Rinternals.h>

#define IS_NUMERIC(x) (isReal(x) || isInteger(x))


SEXP breaks_to_interval(SEXP breaks, SEXP max_upper) {

    // PROTECT counter
    int protected = 0;

    // ensure max_upper is numeric scalar
    if(!IS_NUMERIC(max_upper) || LENGTH(max_upper) != 1)
        error("`max_upper` must be scalar numeric and not NA.");

    // Ensure max_upper is not NA/NaN
    double max_upper_bound = asReal(max_upper);
    if(ISNA(max_upper_bound) || ISNAN(max_upper_bound))
        error("`max_upper` must be scalar numeric and not NA.");

    // ensure numeric breaks
    if (!IS_NUMERIC(breaks) || LENGTH(breaks) == 0)
        error("`breaks` must be numeric and of length >= 1.");

    // coerce breaks to integer
    breaks = PROTECT(coerceVector(breaks, INTSXP)); protected++;

    // Ensure breaks are not NA and are in strictly increasing order and less than max_upper
    int* p_breaks = INTEGER(breaks);
    int brk = p_breaks[0];
    if (brk == NA_INTEGER)
        error("`breaks` must be non-missing (not NA) and coercible to integer.");

    if ((double) brk >= max_upper_bound)
        error("`max_upper` must be greater than all `breaks`.");

    for (int i = 0; i < LENGTH(breaks) - 1; i++) {

        int next_brk = p_breaks[i + 1];

        if (next_brk == NA_INTEGER)
            error("`breaks` must be non-missing (not NA) and coercible to integer.");

        if (next_brk <= brk)
            error("`breaks` must be in strictly increasing order.");

        brk = next_brk;

        if ((double) brk >= max_upper_bound)
            error("`max_upper` must be greater than all `breaks`.");
    }


    // create pointer to breaks and calculate length
    int n_breaks = LENGTH(breaks);

    // make upper bound integerish
    max_upper_bound = round(max_upper_bound);

    // create numeric lower bounds
    SEXP lower_bound = PROTECT(coerceVector(breaks, REALSXP)); protected++;
    double* p_lower_bound = REAL(lower_bound);

    // create numeric upper bounds
    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_breaks)); protected++;
    double* p_upper_bound = REAL(upper_bound);
    for (int i = 0; i < n_breaks - 1; ++i) {
        p_upper_bound[i] = p_lower_bound[i + 1];
    }
    p_upper_bound[n_breaks - 1] = max_upper_bound;

    // create underlying integers for interval factors
    SEXP interval = PROTECT(allocVector(INTSXP, n_breaks)); protected++;
    int* p_interval = INTEGER(interval);
    for (int i = 0; i < n_breaks; i++) {
        p_interval[i] = i + 1;
    }

    // create levels
    SEXP lvls = PROTECT(allocVector(STRSXP, n_breaks)); protected++;

    // create all but the last names for the levels, "[%d,%d)"
    for (int i = 0; i < n_breaks - 1; ++i) {
        int bufsz = snprintf(NULL, 0, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
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

    // add levels and class to intervals
    setAttrib(interval, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2)); protected++;
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(interval, fclass);

    // create list with lower and upper bound entries
    const char *names[] = {"interval", "lower_bound", "upper_bound", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names)); protected++;
    SET_VECTOR_ELT(out, 0, interval);
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
    INTEGER(rnms)[1] = -n_breaks;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(protected);
    return out;

}
