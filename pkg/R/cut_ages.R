# -------------------------------------------------------------------------
#' Cut integer age vectors
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `cut_ages()` provides categorisation of ages based on specified breaks
#' which represent the left-hand interval limits. The resulting intervals span
#' from the minimum break through to a specified `max_upper` and will always be
#' closed on the left and open on the right. Ages below the minimum break, or
#' above `max_upper` will be returned as NA.
#'
# -------------------------------------------------------------------------
#' @param ages `[numeric]`.
#'
#' Vector of age values.
#'
#' Double values are coerced to integer prior to categorisation / aggregation.
#'
#' Must not be NA.
#'
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
#' Represents the maximum upper bound for the resulting intervals.
#'
#' Double values are rounded up to the nearest (numeric) integer.
#'
#' Defaults to `Inf`.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with an ordered factor column (`interval`), as well as columns
#' corresponding to the explicit bounds (`lower` and `upper`).
#' Internally both bound columns are stored as double but it can be taken as
#' part of the function API that `lower` is coercible to integer without
#' any coercion to `NA_integer_`. Similarly all values of `upper` apart
#' from those corresponding to `max_upper` can be assumed coercible to integer
#' (`max_upper` may or may not depending on the given argument).
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' cut_ages(ages = 0:9, breaks = c(0, 3, 5, 10))
#'
#' cut_ages(ages = 0:9, breaks = c(0, 5))
#'
#' # Note the following is comparable to a call to
#' # cut(ages, right = FALSE, breaks = c(breaks, Inf))
#' ages <- seq.int(from = 0, by = 10, length.out = 10)
#' breaks <- c(0, 1, 10, 30)
#' cut_ages(ages, breaks)
#'
#' # values above max_upper treated as NA
#' cut_ages(ages = 0:10, breaks = c(0,5), max_upper = 7)
#'
# -------------------------------------------------------------------------
#' @export
cut_ages <- function(ages, breaks, max_upper = Inf) {

    # check max_upper
    if(!is.numeric(max_upper) || length(max_upper) != 1L || is.na(max_upper) || max_upper <= 0)
        stop("`max_upper` must be positive, numeric and of length 1.")

    # check breaks
    if (!is.numeric(breaks) || length(breaks) == 0L)
        stop("`breaks` must be numeric and of length >= 1.")

    breaks <- as.integer(breaks)
    min_break <- min(breaks)
    if (is.na(min_break) || min_break < 0)
        stop("`breaks` must be coercible to integer, non-negative and not NA.")

    max_break <- max(breaks)
    if (max_break >= max_upper)
        stop("`max_upper` must be greater than all `breaks`.")

    if (is.unsorted(breaks))
        stop("`breaks` must be in strictly increasing order.")

    # check ages
    if (!is.numeric(ages) || length(ages) == 0L)
        stop("`ages` must be numeric and of length >= 1.")

    ages <- as.integer(ages)
    min_age <- min(ages)
    if (is.na(min_age))
        stop("`ages` must be coercible to integer and not NA.")

    if (min_age < breaks[1L])
        stop("`ages` must greater than or equal to the minimum value of `breaks`.");

    # allow for breaks which do not start at zero
    lower <- c(0L, breaks)

    # calculate the upper bounds using max_age (needs replacing for levels)
    n <- length(lower)
    upper <- c(lower[-1L], max(ages, lower[n]) + 1)

    # create a lookup of intervals from ages. 'key' is ages shifted by 1 (as we
    #   index from 1 but need to allow for age 0)
    index <- rep.int(seq_len(n), times = upper - lower)
    ages[ages >= max_upper] <- NA_integer_
    index <- index[ages + 1L]

    # Now use the correct upper bound after making integer(ish)
    upper[n] <- ceiling(max_upper)

    # lookup bounds and convert to numeric for consistent output (as we allow
    # for an arbitrarily large max_upper)
    lower_bounds <- as.numeric(lower[index])
    upper_bounds <- as.numeric(upper[index])

    # calculate the intervals ignoring the first one we added
    intervals <- sprintf("[%.f, %.f)", lower[-1L], upper[-1L])

    # create an ordered factor
    # NOTE: do levels before class to avoid a warning.
    # NOTE: this is not per the API for factors so technically could break (unlikely)
    index <- index - 1L
    attr(index, "levels") <- intervals
    class(index) <- c("ordered", "factor")

    # output as tibble
    new_tibble(
        list(
            interval = index,
            lower = lower_bounds,
            upper = upper_bounds
        )
    )

}
