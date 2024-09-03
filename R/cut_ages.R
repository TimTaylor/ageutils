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
#' Double values are rounded to the nearest (numeric) integer.
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
#' cut_ages(ages = 0:9, breaks = c(0L, 5L))
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
    dat <- .Call(C_cut_ages, ages, breaks, max_upper)
    new_tibble(dat)
}
