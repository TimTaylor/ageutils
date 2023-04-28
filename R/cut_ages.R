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
#' Vector of age in years.
#'
#' Double values are coerced to integer prior to categorisation / aggregation.
#'
#' `ages` >= 200 are not permitted due to the internal implementation and the
#' fact that we have yet to encounter a 200 year old, living, individual.
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
#' Defaults to `Inf`.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with an ordered factor column (`interval`), as well as columns
#' corresponding to the explicit bounds (`lower_bound` and `upper_bound`).
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' cut_ages(ages = 0:9, breaks = c(0L, 3L, 5L, 10L))
#'
#' cut_ages(ages = 0:9, breaks = 5L)
#'
#' # Note the following is comparable to a call to
#' # cut(ages, right = FALSE, breaks = c(breaks, Inf))
#' ages <- seq.int(from = 0, by = 10, length.out = 10)
#' breaks <- c(0, 1, 10, 30)
#' cut_ages(ages, breaks)
#'
#' # values below the minimum break and above max_upper treated as NA
#' cut_ages(ages = 0:10, breaks = 5, max_upper = 7)
#'
# -------------------------------------------------------------------------
#' @export
cut_ages <- function(ages, breaks, max_upper = Inf) {

    .MAXBOUND <- 200

    # ensure numeric ages, breaks and max_upper
    if (!is.numeric(ages))
        stop("`ages` must be numeric.")
    if (!is.numeric(breaks))
        stop("`breaks` must be numeric.")
    if (!is.numeric(max_upper))
        stop("`max_upper` must be numeric.")

    # check ages are appropriately bounded or NA
    na_ages <- is.na(ages)
    if (any((ages < 0 | ages >= .MAXBOUND) & !na_ages))
        stopf("`ages` must be in the interval `[0, %d)` or NA.", .MAXBOUND)

    # check max_upper is appropriately bounded or Inf
    if (!is.numeric(max_upper) || length(max_upper) > 1L || is.na(max_upper))
        stop("`max_upper` must be a numeric scalar.")
    if ((max_upper < 0) ||(max_upper >= .MAXBOUND && is.finite(max_upper)))
        stopf("`max_upper` must be in the interval `[0, %d)` or Inf.", .MAXBOUND)

    # check breaks
    if (anyNA(breaks) || any(breaks < 0 | breaks >= .MAXBOUND))
        stopf("`breaks` must be in the interval [0, %d).", .MAXBOUND)
    if (is.unsorted(breaks, strictly = TRUE))
        stop("`breaks` must be in strictly increasing order.")
    if (breaks[length(breaks)] >= max_upper)
        stop("all `breaks` must be less than `max_upper`.")

    # ensure integer input
    ages <- as.integer(ages)
    breaks <- as.integer(breaks)

    # allow for breaks which do not start at zero
    lower <- c(0L, breaks)
    upper <- c(breaks, .MAXBOUND)

    # create a lookup of intervals from ages. 'key' is ages adapted for invalid
    # values and shifted (as we index from 1 but need to allow for age 0)
    n <- length(lower)
    index <- rep.int(seq_len(n), times = upper - lower)
    idx <- ages
    idx[ages < breaks[1L] | ages >= max_upper] <- NA_integer_
    idx <- index[idx + 1L]

    # set the maximum upper value to users input of max_upper
    upper[n] <- max_upper

    # lookup bounds and convert to numeric for consistent output (as we allow
    # for an arbitrarily large max_upper)
    lower_bounds <- as.numeric(lower[idx])
    upper_bounds <- as.numeric(upper[idx])

    # calculate the intervals ignoring the first one we added
    intervals <- sprintf("[%.f, %.f)", lower[-1L], upper[-1L])

    # create an ordered factor (do levels before class to avoid a warning)
    # note this is not per the API so technically could break (very unlikely though)
    idx <- idx - 1L
    attr(idx, "levels") <- intervals
    class(idx) <- c("ordered", "factor")

    # return as data frame
    list2DF(
        list(
            interval = idx,
            lower_bound = lower_bounds,
            upper_bound = upper_bounds
        )
    )


}



