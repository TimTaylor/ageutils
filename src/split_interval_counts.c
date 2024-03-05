#include <R.h>
#include <Rinternals.h>
#include "split_interval_counts.h"

#define IS_NUMERIC(x) (isReal(x) || isInteger(x))

SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights) {

    // PROTECT counter
    int protected = 0;

    // ensure numeric bounds and counts are of correct length
    if (!IS_NUMERIC(lower_bounds) || LENGTH(lower_bounds) == 0)
        error("`lower_bounds` must be numeric and of length >= 1.");
    if (!IS_NUMERIC(upper_bounds) || LENGTH(upper_bounds) == 0)
        error("`upper_bounds` must be numeric and of length >= 1.");
    if (!IS_NUMERIC(counts) || LENGTH(counts) == 0)
        error("`counts` must be numeric and of length >= 1.");

    // ensure counts are doubles
    counts = PROTECT(coerceVector(counts, REALSXP)); protected++;

    // ensure compatible lengths for bounds and counts
    if (LENGTH(lower_bounds) != LENGTH(upper_bounds))
        error("`lower_bounds` and `upper_bounds` must be the same length.");

    if (LENGTH(lower_bounds) != LENGTH(counts))
        error("`bounds` and `counts` must be the same length.");

    // ensure max_upper is numeric scalar
    if(!IS_NUMERIC(max_upper) || LENGTH(max_upper) != 1)
        error("`max_upper` must be scalar numeric and not NA.");

    // Ensure max_upper is not NA/NaN
    int max_upper_bound = asInteger(max_upper);
    if(max_upper_bound == NA_INTEGER)
        error("`max_upper` must be non-missing (not NA) and coercible to integer.");

    // upper bounds to double
    upper_bounds = PROTECT(coerceVector(upper_bounds, REALSXP)); protected++;

    // Replace upper bounds greater than max_upper_bound
    double* p_upper = REAL(upper_bounds);
    for (int i = 0; i < LENGTH(upper_bounds); i++) {

        if (ISNA(p_upper[i]) || ISNAN(p_upper[i]))
            error("`upper_bounds` must be non-missing (not NA).");

        if (p_upper[i] > (double) max_upper_bound) {
            p_upper[i] = (double) max_upper_bound;
            warning(
                "`upper_bounds` greater than `max_upper` (%d) have been replaced prior to splitting.",
                max_upper_bound
            );
        }
    }

    // coerce lower and upper bounds to integer
    lower_bounds = PROTECT(coerceVector(lower_bounds, INTSXP)); protected++;
    upper_bounds = PROTECT(coerceVector(upper_bounds, INTSXP)); protected++;
    int* p_lower = INTEGER(lower_bounds);
    int* p_int_upper = INTEGER(upper_bounds);

    for (int i = 0; i < LENGTH(lower_bounds); i++) {

        if (p_lower[i] == NA_INTEGER || p_int_upper[i] == NA_INTEGER)
            error("`bounds` must be finite, non-missing (not NA) and coercible to integer.");

        if (p_lower[i] >= p_int_upper[i])
            error("`lower_bounds` must be less than `upper_bounds`.");
    }


    // pointers to bounds, counts and weights
    double* p_counts = REAL(counts);
    double* p_weights;

    // create weights if NULL
    if (TYPEOF(weights) == NILSXP) {
        double value = 1.0 / max_upper_bound;
        p_weights = (double *) R_alloc(max_upper_bound, sizeof(double));
        for (int i = 0; i < max_upper_bound; i++)
            p_weights[i] = value;
    } else {
        weights = PROTECT(coerceVector(weights, REALSXP)); protected++;
        p_weights = REAL(weights);
        if (LENGTH(weights) != max_upper_bound)
            error(
                "`weights` must be a vector of length %d (`max_upper`) representing ages 0:%d",
                max_upper_bound,
                max_upper_bound - 1
            );
        for (int i = 0; i < LENGTH(weights); i++) {
            if (ISNA(p_weights[i]) || ISNAN(p_weights[i]) || p_weights[i] < 0)
                error("`weights` must be non-negative and not NA.");
        }
    }

    // calculate length of output
    int n_bounds = LENGTH(lower_bounds);
    int total = 0;
    for (int i = 0; i < n_bounds; ++i)
        total += (p_upper[i] - p_lower[i]);

    // allocate space for ages
    SEXP age = PROTECT(allocVector(INTSXP, total)); protected++;
    int* p_age = INTEGER(age);

    // allocate space for count
    SEXP count = PROTECT(allocVector(REALSXP, total));  protected++;
    double* p_count = REAL(count);

    // loop over inputs
    int index = 0;
    for (int i = 0; i < n_bounds; ++i) {
        int interval_start = p_lower[i];
        int interval_end = p_upper[i];
        double ct = p_counts[i];
        double sum = 0;
        int new_index = index;
        for (int j = interval_start; j < interval_end; j++) {
            sum += p_weights[j];
            p_age[new_index] = j;
            new_index++;
        }
        new_index = index;
        if (fabs(sum) > sqrt(DBL_EPSILON)) {
            for (int j = interval_start; j < interval_end; j++) {
                p_count[new_index] = ct * p_weights[j] / sum;
                new_index++;
            }
        } else {
            for (int j = interval_start; j < interval_end; j++) {
                p_count[new_index] = ct * p_weights[j];
                new_index++;
            }
        }
        index = new_index;
    }

    // create list with age and count entries
    const char *names[] = {"age", "count", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names)); protected++;
    SET_VECTOR_ELT(out, 0, age);
    SET_VECTOR_ELT(out, 1, count);

    // add the data frame class
    SEXP class = PROTECT(allocVector(STRSXP, 1)); protected++;
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    // this format can be seen in the R function .set_row_names()
    SEXP rnms = PROTECT(allocVector(INTSXP, 2)); protected++;
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -total;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(protected);
    return out;
}
