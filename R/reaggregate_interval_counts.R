# -------------------------------------------------------------------------
#' Reaggregate age intervals
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `reaggregate_interval_counts()` converts counts over one interval range to
#' another. It first splits counts of a given age interval in to counts for
#' individual years based on a given weighting. These are then aggregated to the
#' desired breaks. Functionally this is equivalent to, but more efficient than,
#' a call to `split_interval_counts()` followed by `aggregate_age_counts()`.
#'
# -------------------------------------------------------------------------
#' @param breaks `[numeric]`.
#'
#' 1 or more cut points in increasing (strictly) order.
#'
#' These correspond to the left hand side of the desired intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param counts `[numeric]`.
#'
#' Vector of counts to be aggregated.
#'
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
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and an
#' associated `count`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' reaggregate_interval_counts(
#'     lower_bounds = c(0, 5, 10),
#'     upper_bounds = c(5, 10, 20),
#'     counts = c(5, 10, 30),
#'     breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
# -------------------------------------------------------------------------
#' @export
reaggregate_interval_counts <- function(
        lower_bounds,
        upper_bounds,
        counts,
        breaks,
        max_upper = 100L,
        weights = NULL
) {
    # # ensure numeric bounds, counts and weights
    # .assert_numeric(lower_bounds)
    # .assert_numeric(upper_bounds)
    # .assert_numeric(counts)
    # .assert_scalar_numeric(max_upper)
    #
    # # Ensure max_upper is coercible to integer
    # max_upper <- as.integer(max_upper)
    # if (anyNA(max_upper))
    #     stop("`max_upper` must be finite, and, coercible to integer.")
    #
    # # Ensure bounds and counts have compatible lengths
    # n_bounds <- length(upper_bounds)
    # if (length(lower_bounds) != n_bounds)
    #     stop("`lower_bounds` and `upper_bounds` must be the same length.")
    # if (length(counts) != n_bounds)
    #     stop("`bounds` and `counts` must be the same length.")
    #
    # # Ensure lower bounds are coercible to integer
    # lower_bounds <- as.integer(lower_bounds)
    # if (anyNA(lower_bounds))
    #     stop("`lower_bounds` must be finite, non-missing (not NA) and coercible to integer.")
    #
    # # Replace upper bounds greater than max_upper then ensure coercible to integer
    # if (anyNA(upper_bounds))
    #     stop("`upper_bounds` must be non-missing (not NA).")
    # idx <- upper_bounds > max_upper
    # if (isTRUE(any(idx))) {
    #     upper_bounds[idx] <- max_upper
    #     warningf(
    #         "`upper_bounds` greater than `max_upper` (%d) have been replaced prior to splitting.",
    #         max_upper
    #     )
    # }
    # upper_bounds <- as.integer(upper_bounds)
    #
    # # Ensure lower bounds less than upper bounds
    # if (any(lower_bounds >= upper_bounds))
    #     stop("`lower_bounds` must be less than `upper_bounds`.")
    #
    # # Coerce counts to double prior to calling C function
    # counts <- as.double(counts)
    #
    # # check weights if not NULL
    # if (!is.null(weights)) {
    #     if (!is.numeric(weights))
    #         stop("`weights` must be numeric.")
    #     if (anyNA(weights) || min(weights, na.rm = TRUE) < 0)
    #         stop("`weights` must be non-negative and not missing (NA).")
    #     if (length(weights) != max_upper) {
    #         stopf(
    #             "`weights` must be a vector of length %d (`max_upper`) representing ages 0:%d",
    #             max_upper,
    #             max_upper - 1
    #         )
    #     }
    #     weights <- as.double(weights)
    # }
    #
    # # coerce breaks to integer and ensure not NA
    # breaks <- as.integer(breaks)
    # if (anyNA(breaks))
    #     stop("`breaks` must be non-missing (not NA) and coercible to integer.")
    #
    # # check strictly increasing breaks
    # if (is.unsorted(breaks, strictly = TRUE))
    #     stop("`breaks` must be in strictly increasing order.")

    .Call(C_reaggregate_interval_counts, lower_bounds, upper_bounds, counts, breaks, max_upper, weights)
}
