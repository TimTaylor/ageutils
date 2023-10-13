#include <Rinternals.h>
#include "aggregate_age_counts.h"
#include "split_interval_counts.h"

SEXP reaggregate_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP breaks, SEXP max_upper, SEXP weights) {
    SEXP split = PROTECT(split_interval_counts(lower_bounds, upper_bounds, counts, max_upper, weights));
    SEXP out = PROTECT(aggregate_age_counts(VECTOR_ELT(split, 1), VECTOR_ELT(split, 0), breaks));
    UNPROTECT(2);
    return out;
}
