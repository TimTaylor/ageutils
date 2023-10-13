#include <R.h>
#include <Rinternals.h>

#define MAXBOUND 2000

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


SEXP aggregate_age_counts(SEXP counts, SEXP ages, SEXP breaks) {

    // ensure numeric input
    if (!isNumeric(counts))
        error("`counts` must be numeric.");
    if (!isNumeric(ages))
        error("`ages` must be numeric.");
    if (!isNumeric(breaks))
        error("`breaks` must be numeric.");

    // coerce counts to double and ages/breaks to integer
    counts = PROTECT(coerceVector(counts, REALSXP));
    ages = PROTECT(coerceVector(ages, INTSXP));
    breaks = PROTECT(coerceVector(breaks, INTSXP));

    // check ages are appropriately bounded or NA
    int n_ages = LENGTH(ages);
    int* p_ages = INTEGER(ages);
    for (int i = 0; i < n_ages; i++) {
        int age = p_ages[i];
        if (age != NA_INTEGER) {
            if (age < 0 || age >= MAXBOUND)
                error("`ages` must be in the interval `[0, %d)` or NA.", MAXBOUND);
        }
    }

    // check ages and counts are of equal length
    if (n_ages != LENGTH(counts))
        error("`ages` and `counts` must be the same length.");

    // check breaks and add MAXBOUND
    int n_breaks = LENGTH(breaks);
    int* p_breaks = INTEGER(breaks);

    SEXP new_breaks = PROTECT(allocVector(REALSXP, n_breaks + 1));
    double* p_new_breaks = REAL(new_breaks);

    int first_break = p_breaks[0];
    if (first_break == NA_INTEGER || first_break < 0)
        error("`breaks` must be non-negative and coercible to integer.");
    p_new_breaks[0] = first_break;
    for (int i = 0; i < n_breaks - 1; ++i) {
        int brk = p_breaks[i + 1];
        if (brk == NA_INTEGER || brk <= p_new_breaks[i])
            error("`breaks` must be non-negative and in strictly increasing order.");
        p_new_breaks[i + 1] = brk;
    }
    p_new_breaks[n_breaks] = MAXBOUND;

    // order by age
    // ind = order(x, nalast = TRUE, decreasing = FALSE)
    // R_orderVector1(result, length, input to sort, nalast, decreasing)
    int* ind;
    ind = (int *) R_alloc(n_ages, sizeof(int));

    int* out_ages;
    out_ages = (int *) R_alloc(n_ages, sizeof(int));

    double* p_counts = REAL(counts);
    double* out_counts;
    out_counts = (double *) R_alloc(n_ages, sizeof(double));
    R_orderVector1(ind, n_ages, ages, TRUE, FALSE);
    for (int i = 0; i < n_ages; i++) {
        out_ages[i] = p_ages[ind[i]];
        out_counts[i] = p_counts[ind[i]];
    }

    // number of groups (allowing for an NA group)
    int n_groups = n_breaks + 1;

    // allocate output and initialise to 0
    SEXP group_counts = PROTECT(allocVector(REALSXP, n_groups));
    double* p_groups = REAL(group_counts);
    Memzero(p_groups, n_groups);

    // Calculate the NA values when ages are below the first break
    int j = 0;
    int a = out_ages[j];
    while (a < first_break) {
        double tmp = out_counts[j];
        p_groups[n_breaks] += tmp;
        j++;
        if (j < n_ages) {
            a = out_ages[j];
        } else {
            break;
        }
    }

    // calculate the other counts
    int group_index = 0;
    for (int i = j; i < n_ages; ++i) {
        int current_age = out_ages[i];
        double tmp = out_counts[i];
        if (current_age == NA_INTEGER) {
            p_groups[n_breaks] += tmp;
        } else {
            while(group_index < n_groups - 2 && current_age >= p_breaks[group_index + 1])
                ++group_index;
            p_groups[group_index] += tmp;
        }
    }

    // generate the corresponding intervals
    SEXP start = PROTECT(allocVector(REALSXP, n_groups));
    SEXP end = PROTECT(allocVector(REALSXP, n_groups));
    double* p_start = REAL(start);
    double* p_end = REAL(end);

    SEXP factor = PROTECT(allocVector(INTSXP, n_groups));
    SEXP lvls = PROTECT(allocVector(STRSXP, n_groups - 1)); // No NA level
    int* p_factor = INTEGER(factor);

    // create all but the last names for the intervals, "[%d,%d)"
    for (int i=0; i < n_groups - 1; ++i) {
        p_factor[i] = i+1;
        p_start[i] = p_new_breaks[i];
        p_end[i] = p_new_breaks[i+1];
        // names "[%d,%d)"
        int bufsz = snprintf(NULL, 0, "[%d, %d)", (int) p_new_breaks[i], (int) p_new_breaks[i+1]);
        char* buf = R_Calloc(bufsz + 1, char);
        snprintf(buf, bufsz + 1, "[%d, %d)", (int) p_new_breaks[i], (int) p_new_breaks[i+1]);
        SET_STRING_ELT(lvls, i, mkChar(buf));
        R_Free(buf);
    }

    p_end[n_groups - 2] = R_PosInf;
    p_start[n_groups - 1] = NA_REAL;
    p_end[n_groups - 1] = NA_REAL;
    p_factor[n_groups - 1] = NA_INTEGER;

    // create last name "[%d,Inf)"
    int bufsz = snprintf(NULL, 0, "[%d, Inf)", (int) p_new_breaks[n_groups-2]);
    char* buf = R_Calloc(bufsz + 1, char);
    snprintf(buf, bufsz + 1, "[%d, Inf)", (int) p_new_breaks[n_groups-2]);
    SET_STRING_ELT(lvls, n_groups-2, mkChar(buf));
    R_Free(buf);

    // add levels and class to factor
    setAttrib(factor, R_LevelsSymbol, lvls);
    SEXP fclass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(fclass, 0, mkChar("ordered"));
    SET_STRING_ELT(fclass, 1, mkChar("factor"));
    classgets(factor, fclass);

    // array of names; note the null string
    const char *names[] = {"interval" ,"lower_bound", "upper_bound", "count", ""};
    SEXP out = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(out, 0, factor);
    SET_VECTOR_ELT(out, 1, start);
    SET_VECTOR_ELT(out, 2, end);
    SET_VECTOR_ELT(out, 3, group_counts);

    // add class
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("data.frame"));
    classgets(out, class);

    // add row names in short form
    SEXP rnms = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rnms)[0] = NA_INTEGER;
    INTEGER(rnms)[1] = -n_groups;
    setAttrib(out, R_RowNamesSymbol, rnms);

    UNPROTECT(13);

    return out;
}

/////////////////////////////////////////////////////////

SEXP reaggregate_interval_counts(SEXP lower_bounds, SEXP upper_bounds, SEXP counts, SEXP breaks, SEXP max_upper, SEXP weights) {
    SEXP split = PROTECT(split_interval_counts(lower_bounds, upper_bounds, counts, max_upper, weights));
    SEXP out = PROTECT(aggregate_age_counts(VECTOR_ELT(split, 1), VECTOR_ELT(split, 0), breaks));
    UNPROTECT(2);
    return out;
}
