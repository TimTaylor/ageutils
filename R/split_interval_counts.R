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
#' Used to replace `Inf` upper bounds prior to splitting.
#'
#' If any `upper_bound` is greater than `max_upper` the function will error.
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
