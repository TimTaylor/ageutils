#include <R.h>
#include <Rinternals.h>
#include "split_interval_counts.h"

SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights) {

    int protected = 0;

    // pointers to bounds, counts and weights
    int* p_lower = INTEGER(lower_bounds);
    int* p_upper = INTEGER(upper_bounds);
    double* p_counts = REAL(counts);
    double* p_weights;

    // get the maximum upper bound
    int max_upper_bound = asInteger(max_upper);

    // create weights if NULL
    if (TYPEOF(weights) == NILSXP) {
        double value = 1.0 / max_upper_bound;
        p_weights = (double *) R_alloc(max_upper_bound, sizeof(double));
        for (int i = 0; i < max_upper_bound; i++)
            p_weights[i] = value;
    } else {
        p_weights = REAL(weights);
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
