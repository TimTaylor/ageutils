#include <R.h>
#include <Rinternals.h>

#define IS_NUMERIC(x) (isReal(x) || isInteger(x))

SEXP cut_ages(SEXP ages, SEXP breaks, SEXP max_upper) {

    // protection counter
    int protected = 0;

    // ensure max_upper is numeric scalar
    if(!IS_NUMERIC(max_upper) || LENGTH(max_upper) != 1)
        error("`max_upper` must be scalar numeric and not NA.");

    // Ensure max_upper is not NA/NaN
    double max_upper_bound = asReal(max_upper);
    if(ISNA(max_upper_bound) || ISNAN(max_upper_bound))
        error("`max_upper` must be scalar numeric and not NA.");

    // Ensure max_upper > 0
    if (max_upper_bound <= 0)
        error("`max_upper` must be positive.");

    // ensure numeric breaks
    if (!IS_NUMERIC(breaks) || LENGTH(breaks) == 0)
        error("`breaks` must be numeric and of length >= 1.");

    // coerce breaks to integer and store length
    breaks = PROTECT(coerceVector(breaks, INTSXP)); protected++;
    int  n_breaks = LENGTH(breaks);

    // Ensure breaks are not NA and are in strictly increasing order
    int* p_breaks = INTEGER(breaks);
    int brk = p_breaks[0];
    if (brk == NA_INTEGER || brk < 0)
        error("`breaks` must be non-negative and coercible to integer.");

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

    // ensure numeric ages
    if (!IS_NUMERIC(ages) || LENGTH(ages) == 0)
        error("`ages` must be numeric and of length >= 1.");

    // coerce ages to integer
    ages = PROTECT(coerceVector(ages, INTSXP)); protected++;

    // check ages are >= the first break and not NA
    int* p_ages = INTEGER(ages);
    int  n_ages   = LENGTH(ages);
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age == NA_INTEGER)
            error("`ages` must be non-missing (not NA) and coercible to integer.");
        if (age < p_breaks[0])
            error("`ages` must greater than or equal to the minimum value of `breaks`.");

    }

    // round max upper
    max_upper_bound = round(max_upper_bound);

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
        if (age == NA_INTEGER || age >= max_upper_bound) {
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
