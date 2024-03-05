# -------------------------------------------------------------------------
#' Convert breaks to an interval
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `breaks_to_interval()` takes a specified set of breaks representing the left
#' hand limits of a closed open interval, i.e [x, y), and returns the
#' corresponding interval and upper bounds. The resulting intervals span from
#' the minimum break through to a specified `max_upper`.
#'
# -------------------------------------------------------------------------
#' @param breaks `[integerish]`.
#'
#' 1 or more non-negative cut points in increasing (strictly) order.
#'
#' These correspond to the left hand side of the desired intervals (e.g. the
#' closed side of [x, y).
#'
#' Double values are coerced to integer prior to categorisation.
#'
#' @param max_upper `[numeric]`
#'
#' Represents the maximum upper bound splitting the data.
#'
#' Defaults to `Inf`.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with an ordered factor column (`interval`), as well as columns
#' corresponding to the explicit bounds (`lower_bound` and `upper_bound`). Note
#' these bounds are returned as `<numeric>` to allow the maximum upper bound to
#' be `Inf`.
#'
# -------------------------------------------------------------------------
#' @examples
#' brks <- c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' breaks_to_interval(breaks = brks)
#' breaks_to_interval(breaks = brks, max_upper = 100L)
#'
#' @export
breaks_to_interval <- function(breaks, max_upper = Inf) {
    .Call(C_breaks_to_interval, breaks, max_upper)
}

# -------------------------------------------------------------------------
# R implementation for testing
# -------------------------------------------------------------------------
breaks_to_interval_r <- function(breaks, max_upper = Inf) {

    # check breaks are numeric
    .assert_numeric(breaks)

    # coerce breaks to integer
    breaks <- as.integer(breaks)

    # ensure valid
    if (anyNA(breaks))
        stop("`breaks` must be finite, and, coercible to integer.")

    # check strictly increasing breaks
    if (is.unsorted(breaks, strictly = TRUE))
        stop("`breaks` must be in strictly increasing order.")

    # check max_upper
    .assert_scalar_numeric_not_na(max_upper)
    if (max_upper <= max(breaks))
        stop("`max_upper` must be greater than all `breaks`.")

    # convert to double for consistency across bounds
    breaks <- as.double(breaks)

    # generate intervals
    upper <- c(breaks[-1L], round(max_upper))
    intervals <- sprintf("[%.f, %.f)", breaks, upper)
    intervals <- factor(intervals, levels = intervals, ordered = TRUE)

    # return as data frame
    list2DF(
        list(
            interval = intervals,
            lower_bound = breaks,
            upper_bound = upper
        )
    )

}

