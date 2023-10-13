#ifndef SPLIT_INTERVAL_COUNTS_H
#define SPLIT_INTERVAL_COUNTS_H

#include "Rinternals.h"

SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights);

#endif
