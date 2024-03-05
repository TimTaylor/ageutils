# -------------------------------------------------------------------------
#' Split interval counts
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `split_interval_counts()` splits counts of a given age interval in to
#' counts for individual years based on a given weighting. Age intervals are
#' specified by their lower (closed) and upper (open) bounds, i.e. intervals
#' of the form [lower, upper).
#'
# -------------------------------------------------------------------------
#' @param lower_bounds,upper_bounds `[integerish]`.
#'
#' A pair of vectors representing the bounds of the intervals.
#'
#' `lower_bounds` must be strictly less than `upper_bounds` and greater than or
#' equal to zero.
#'
#' Missing (NA) bounds are not permitted.
#'
#' Double vectors will be coerced to integer.
#'
#' @param counts `[numeric]`.
#'
#' Vector of counts to be aggregated.
#'
#' @param max_upper `[integerish]`
#'
#' Represents the maximum upper bounds permitted upon splitting the data.
#'
#' Any upper bound greater than this will be replaced with this value prior to
#' splitting.
#'
#' Double vectors will be coerced to integer.
#'
#' @param weights `[numeric]`
#'
#' Population weightings to apply for individual years.
#'
#' If `NULL` (default) counts will be split evenly based on interval size.
#'
#' If specified, must be of length `max_upper` and represent weights in the
#' range 0:(max_upper - 1).
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with entries `age` (in years) and `count`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30)
#' )
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, Inf),
#'     counts = c(5, 10, 30),
#'     max_upper = 15
#' )
#'
#' split_interval_counts(
#'     lower_bounds = c(0, 5),
#'     upper_bounds = c(5, 10),
#'     counts = c(5, 10),
#'     max_upper =10,
#'     weights = 1:10
#' )
#'
# -------------------------------------------------------------------------
#' @export
split_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        max_upper = 100L,
        weights = NULL
) {
    .Call(C_split_interval_counts, lower_bounds, upper_bounds, counts, max_upper, weights)
}

# -------------------------------------------------------------------------
# R implementation for testing
# -------------------------------------------------------------------------
split_interval_counts_r <- function(
        lower_bounds,
        upper_bounds,
        counts,
        max_upper = 100L,
        weights = NULL
) {
    # ensure numeric bounds, counts and weights
    if (!is.numeric(lower_bounds))
        stop("`lower_bounds` must be numeric.")
    if (!is.numeric(upper_bounds))
        stop("`upper_bounds` must be numeric.")
    if (!is.numeric(counts))
        stop("`counts` must be numeric.")
    if (!(is.numeric(max_upper) && length(max_upper) == 1L))
        stop("`max_upper` must be an integer of length 1.")

    # Ensure max_upper is coercible to integer
    max_upper <- as.integer(max_upper)
    if (anyNA(max_upper))
        stop("`max_upper` must be finite, and, coercible to integer.")

    # Ensure bounds and counts have compatible lengths
    n_bounds <- length(upper_bounds)
    if (length(lower_bounds) != n_bounds)
        stop("`lower_bounds` and `upper_bounds` must be the same length.")
    if (length(counts) != n_bounds)
        stop("`bounds` and `counts` must be the same length.")

    # Ensure lower bounds are coercible to integer
    lower_bounds <- as.integer(lower_bounds)
    if (anyNA(lower_bounds))
        stop("`lower_bounds` must be finite, non-missing (not NA) and coercible to integer.")

    # Replace upper bounds greater than max_upper then ensure coercible to integer
    if (anyNA(upper_bounds))
        stop("`upper_bounds` must be non-missing (not NA).")
    idx <- upper_bounds > max_upper
    if (isTRUE(any(idx))) {
        upper_bounds[idx] <- max_upper
        warningf(
            "`upper_bounds` greater than `max_upper` (%d) have been replaced prior to splitting.",
            max_upper
        )
    }
    upper_bounds <- as.integer(upper_bounds)

    # Ensure lower bounds less than upper bounds
    if (any(lower_bounds >= upper_bounds))
        stop("`lower_bounds` must be less than `upper_bounds`.")

    # Coerce counts to double prior to calling C function
    counts <- as.double(counts)

    # check weights if not NULL
    if (is.null(weights)) {
        weights <- rep.int(1 / max_upper, max_upper)
    } else {
        if (!is.numeric(weights))
            stop("`weights` must be numeric.")
        if (anyNA(weights) || min(weights, na.rm = TRUE) < 0)
            stop("`weights` must be non-negative and not missing (NA).")
        if (length(weights) != max_upper) {
            stopf(
                "`weights` must be a vector of length %d (`max_upper`) representing ages 0:%d",
                max_upper,
                max_upper - 1
            )
        }
        weights <- as.double(weights)
    }

    n <- length(lower_bounds)
    age_lengths <- upper_bounds - lower_bounds
    total <- sum(age_lengths)
    age <-  integer(total)
    count <-  numeric(total)
    index <- 1
    for (i in seq_len(n)) {
        interval_start <- lower_bounds[i]
        interval_end <- upper_bounds[i]
        ct <- counts[i]
        ag <- interval_start:(interval_end - 1)
        wt <- weights[ag+1]
        s <- sum(wt)
        if (abs(s) > sqrt(.Machine$double.eps))
            wt <- wt / s
        new_index <- index+age_lengths[i]
        age[index:(new_index - 1)] <- ag
        count[index:(new_index - 1)] <- ct*wt
        index <- new_index
    }

    list2DF(list(age=age, count=count))
}




























