# -------------------------------------------------------------------------
#' Aggregate counts across ages
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `aggregate_age_counts()` provides aggregation of counts across ages (in
#' years). It is similar to a `cut()` and `tapply()` pattern but optimised for
#' speed over flexibility. It takes a specified set of breaks representing the
#' left hand limits of a closed open interval, i.e [x, y), and returns the
#' corresponding interval and upper bounds. The resulting intervals span from
#' the minimum break through to the maximum age. Missing values are grouped as
#' NA.
#'
# -------------------------------------------------------------------------
#' @param ages `[numeric]`.
#'
#' Vector of age in years.
#'
#' Double values are coerced to integer prior to categorisation / aggregation.
#'
#' For `aggregate_age_counts()`, these must corresponding to the `counts` entry
#' and will defaults to 0:(N-1) where `N` is the number of counts present.
#'
#' No (non-missing) age can be less than the minimum break.
#'
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
#'
#' # NA ages are handled with their own grouping
#' ages <- 1:65
#' ages[1:44] <- NA
#' aggregate_age_counts(
#'     counts = 1:65,
#'     ages = ages,
#'     breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
#' )
#'
# -------------------------------------------------------------------------
#' @export
aggregate_age_counts <- function(
    counts,
    ages = seq_along(counts) - 1L,
    breaks
) {
    .Call(C_aggregate_age_counts, counts, ages, breaks)
}
