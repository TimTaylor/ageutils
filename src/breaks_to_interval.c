#include <R.h>
#include <Rinternals.h>

SEXP breaks_to_interval(SEXP breaks, SEXP max_upper) {

    // check breaks are numeric
    if (!isNumeric(breaks))
        error("`breaks` must be numeric.");

    // coerce breaks to integer
    // this is mainly for the warning messages
    breaks = PROTECT(coerceVector(breaks, INTSXP));

    // loop over breaks to check valid
    int n_breaks = LENGTH(breaks);
    int* p_breaks = INTEGER(breaks);
    if (p_breaks[n_breaks - 1] == NA_INTEGER)
        error("`breaks` must be non-missing, finite, and, coercible to integer.");


    for (int i = 0; i < n_breaks - 1; ++i) {
        int lower = p_breaks[i];
        if (lower == NA_INTEGER) {
            error("`breaks` must be non-missing, finite, and, coercible to integer.");
        }
        int upper = p_breaks[i + 1];
        if (upper <= lower) {
            error("`breaks` must be in strictly increasing order.");
        }
    }

    // check max_upper is scalar and appropriately bounded
    int n_max_upper = LENGTH(max_upper);
    if (n_max_upper != 1)
        error("`max_upper` must be a numeric scalar.");
    double max = asReal(max_upper);
    if (ISNA(max))
        error("`max_upper` must be a numeric scalar.");

    // create numeric lower bounds
    SEXP lower_bound = PROTECT(coerceVector(breaks, REALSXP));
    double* p_lower_bound = REAL(lower_bound);

    // check breaks < max_upper
    if (max <= p_lower_bound[n_breaks - 1])
        error("`max_upper` must be greater than all `breaks`");

    // create numeric upper bounds
    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_breaks));
    double* p_upper_bound = REAL(upper_bound);
    for (int i = 0; i < n_breaks - 1; ++i) {
        p_upper_bound[i] = p_lower_bound[i + 1];
    }
    p_upper_bound[n_breaks - 1] = max;


    // create underlying integers for interval factors
    SEXP interval = PROTECT(allocVector(INTSXP, n_breaks));
    int* p_interval = INTEGER(interval);
    for (int i = 0; i < n_breaks; i++) {
        p_interval[i] = i + 1;
    }

    // create levels
    SEXP lvls = PROTECT(allocVector(STRSXP, n_breaks));

    // create all but the last names for the levels, "[%d,%d)"
    for (int i = 0; i < n_breaks - 1; ++i) {
        int bufsz = snprintf(NULL, 0, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", p_breaks[i], p_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    // create last level name allowing for "[%d, Inf)"
    if (!R_FINITE(max)) {
        int bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_breaks[n_breaks - 1]);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    } else {
        int bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_breaks[n_breaks - 1], (int) max);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_breaks[n_breaks - 1], (int) max);
        SET_STRING_ELT(lvls, n_breaks - 1, mkChar(buf));
        R_Free(buf);
    }

    // add levels and class to intervals
    setAttrib(interval, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(interval, fclass);

    // create list with lower and upper bound entries
    const char *names[] = {"interval", "lower_bound", "upper_bound", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, interval);
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
    INTEGER(rnms)[1] = -n_breaks;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(9);
    return out;
}
