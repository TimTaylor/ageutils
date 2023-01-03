#include <R.h>
#include <Rinternals.h>

#ifndef _AGEUTILS
#define _AGEUTILS

// convert breaks to interval
// Assumes: breaks are finite, coercible to integer, not NA, and strictly sorted (increasingly).
SEXP breaks_to_interval(SEXP breaks);

// convert ages to interval
// Assumes: ages are integer with 200 > age >= 0 (NA allowed)
//          not all ages are NA
//          breaks are integer, unique, sorted (increasingly) and not NA
SEXP cut_ages(SEXP ages, SEXP breaks);

// calculate the aggregated age counts
// Assumes: integer bounds
//          bounds equal length
//          no missing (NA) bounds
//          lower bounds less than upper bounds
//          weights >= 0 and of length max(upper_bounds) corresponding to ages 0:(max - 1)
SEXP split_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP max_upper, SEXP weights);

// calculate the aggregated age counts
// Assumes: ages are integer and non-negative
//          ages is the same length as counts
//          counts are numeric
//          breaks are integer, unique, sorted (increasingly) and not NA
SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP breaks);

// calculate the aggregated age counts
// Assumes: integer bounds of equal length
//          lower bounds less than upper bounds
//          counts are numeric
//          breaks are integer, unique, sorted (increasingly) and not NA
//          weights >= 0 and of length max(upper_bounds) corresponding to ages 0:(max - 1)
SEXP reaggregate_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP breaks, SEXP max_upper, SEXP weights);

#endif
