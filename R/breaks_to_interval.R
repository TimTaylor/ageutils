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
#' A [tibble][tibble::tbl_df-class] with an ordered factor column (`interval`),
#' as well as columns corresponding to the explicit bounds (`lower_bound` and
#' `upper_bound`). Note that even those these bounds are whole numbers they are
#' returned as `numeric` to allow the maximum upper bound to be given as `Inf`.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' breaks_to_interval(breaks = c(0, 1, 5, 15, 25, 45, 65))
#' breaks_to_interval(
#'     breaks = c(0, 1, 5, 15, 25, 45, 65),
#'     max_upper = 100
#' )
#'
#' @export
breaks_to_interval <- function(breaks, max_upper = Inf) {


    # coerce breaks to integer
    if (!is.numeric(breaks))
        stop("`breaks` must be numeric.")
    breaks <- as.integer(breaks)

    # ensure valid
    if (anyNA(breaks))
        stop("`breaks` must be finite, and, coercible to integer.")

    # check strictly increasing breaks
    if (is.unsorted(breaks, strictly = TRUE))
        stop("`breaks` must be in strictly increasing order.")

    # check max_upper
    if (!is.numeric(max_upper) || length(max_upper) != 1L || is.na(max_upper))
        stop("`max_upper` must be a numeric vector of length 1 and not NA.")
    if (max_upper <= max(breaks))
        stop("`max_upper` must be greater than all `breaks`.")

    # convert to double for consistency across bounds
    breaks <- as.double(breaks)

    # generate intervals
    upper <- c(breaks[-1L], round(max_upper))
    intervals <- sprintf("[%.f, %.f)", breaks, upper)
    intervals <- factor(intervals, levels = intervals, ordered = TRUE)

    # return as tibble
    # NOTE: This is almost as efficient as list2DF but does lack validation. It
    #       should always be ok but just in case we add a validation step in our
    #       tests.
    new_tibble(
        list(
            interval = intervals,
            lower_bound = breaks,
            upper_bound = upper
        )
    )
}
