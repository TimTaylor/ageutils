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
#' 1 or more non-negative cut points in increasing (strictly) order.
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
#' A data frame with 4 entries; `interval`, `lower_bound`, `upper_bound` and an
#' associated `count`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # default ages generated if only counts provided (here ages will be 0:64)
#' aggregate_age_counts(counts = 1:65, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))
#' aggregate_age_counts(counts = 1:65, breaks = 50L)
#'
#' # NA ages are handled with their own grouping
#' ages <- 1:65;
#' ages[1:44] <- NA
#' aggregate_age_counts(
#'     counts = 1:65,
#'     ages = ages,
#'     breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' )
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
    .Call(C_reaggregate_interval_counts, lower_bounds, upper_bounds, counts, breaks, max_upper, weights)
}
