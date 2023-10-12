#include <R.h>
#include <Rinternals.h>

SEXP cut_ages(SEXP ages, SEXP breaks, SEXP max_upper) {

    // protection counter
    int protected = 0;

    // check ages and breaks are numeric
    // note - isNumeric allows logical hence the longer form used below
    if (!(isReal(ages) || isInteger(ages)))
        error("`ages` must be numeric.");

    if (!(isReal(breaks) || isInteger(breaks)))
        error("`breaks` must be numeric.");

    if (!(isReal(max_upper) || isInteger(max_upper)))
        error("`max_upper` must be numeric.");

    // coerce ages to integer and check they are appropriately bounded or NA
    ages = PROTECT(coerceVector(ages, INTSXP)); protected++;
    int n_ages  = LENGTH(ages);
    int* p_ages = INTEGER(ages);
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age != NA_INTEGER && age < 0)
            error("`ages` must be non-negative or NA.");

    }

    // check max_upper is scalar and appropriately bounded
    if (LENGTH(max_upper) != 1)
        error("`max_upper` must be a numeric scalar.");
    double max = round(asReal(max_upper));
    if (ISNA(max))
        error("`max_upper` must be a numeric scalar.");
    if (max <= 0)
        error("`max_upper` must be positive");

    // check breaks
    breaks = PROTECT(coerceVector(breaks, INTSXP)); protected++;
    int n_breaks = LENGTH(breaks);
    int* p_breaks = INTEGER(breaks);

    int first_break = p_breaks[0];
    if (first_break == NA_INTEGER || first_break < 0)
        error("`breaks` must be non-negative and coercible to integer.");

    for (int i = 1; i < n_breaks; i++) {
        int brk = p_breaks[i];
        if (brk == NA_INTEGER || brk < 0)
            error("`breaks` must be non-negative and coercible to integer.");
        if (brk <= p_breaks[i - 1])
            error("`breaks` must be in strictly increasing order and not NA.");
    }

    // create vector of lower and upper bounds by looping over breaks using
    // index as a pointer that maps an age to a corresponding bound
    int max_bound = p_breaks[n_breaks - 1];
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age > max_bound)
            max_bound = age;
    }
    max_bound++;

    int* index;
    index = (int *) R_alloc(max_bound, sizeof(int));

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
    for (int j = lower[n_breaks - 1]; j < max_bound; j++)
        index[j] = n_breaks - 1;

    // set the upper bound to max_upper
    upper[n_breaks - 1] = max;

    // create factors and output bounds corresponding to ages
    SEXP factor = PROTECT(allocVector(INTSXP, n_ages)); protected++;
    int* p_factor = INTEGER(factor);

    SEXP lower_bound = PROTECT(allocVector(REALSXP, n_ages)); protected++;
    double* p_lower_bound = REAL(lower_bound);

    SEXP upper_bound = PROTECT(allocVector(REALSXP, n_ages)); protected++;
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
