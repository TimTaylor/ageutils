#' Argument .assertions
#'
# -------------------------------------------------------------------------
#' .assertions for function arguments. Motivated by `vctrs::vec_.assert()` but
#' with lower overhead at a cost of less informative error messages. Designed to
#' make it easy to identify the top level calling function whether used within a
#' user facing function or internally.
#'
# -------------------------------------------------------------------------
#' @param x
#'
#' Argument to check.
#'
#' @param arg `[character]`
#'
#' Name of argument being checked (used in error message).
#'
#' @param call `[call]`
#'
#' Call to use in error message.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' NULL if the .assertion succeeds (error otherwise).
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # Use in a user facing function
#' fun <- function(i, d, l, chr, b) {
#'     .assert_scalar_int(i)
#'     TRUE
#' }
#' fun(i=1L)
#' try(fun())
#' try(fun(i="cat"))
#'
#' # Use in an internal function
#' internal_fun <- function(a) {
#'     .assert_string(a, arg = deparse(substitute(a)), call = sys.call(-1L))
#'     TRUE
#' }
#' external_fun <- function(b) {
#'     internal_fun(a=b)
#' }
#' external_fun(b="cat")
#' try(external_fun())
#' try(external_fun(b = letters))
#'
# -------------------------------------------------------------------------
#' @noRd
NULL

# -------------------------------------------------------------------------
.assert_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.integer(x))
        stopf("`%s` must be an integer vector.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_int <- .assert_integer

# -------------------------------------------------------------------------
.assert_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.double(x))
        stopf("`%s` must be a double vector.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_dbl <- .assert_double

# -------------------------------------------------------------------------
.assert_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_num <- .assert_numeric

# -------------------------------------------------------------------------
.assert_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.logical(x))
        stopf("`%s` must be a logical vector.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_lgl <- .assert_logical

# -------------------------------------------------------------------------
.assert_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.character(x))
        stopf("`%s` must be a character vector.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_chr <- .assert_character

# -------------------------------------------------------------------------
.assert_data_frame <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.data.frame(x))
        stopf("`%s` must be a data frame.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_list <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.list(x))
        stopf("`%s` must be a list.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_integer <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.integer(x) && length(x) == 1L))
        stopf("`%s` must be an integer vector of length 1.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_int <- .assert_scalar_integer

# -------------------------------------------------------------------------
.assert_scalar_integer_not_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.integer(x) && length(x) == 1L) || is.na(x))
        stopf("`%s` must be an integer vector of length 1 and not NA.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_int_not_na <- .assert_scalar_integer_not_na

# -------------------------------------------------------------------------
.assert_scalar_double <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.double(x) && length(x) == 1L))
        stopf("`%s` must be a double vector of length 1.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_dbl <- .assert_scalar_double

# -------------------------------------------------------------------------
.assert_scalar_double_not_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.double(x) && length(x) == 1L) || is.na(x))
        stopf("`%s` must be a double vector of length 1 and not NA.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_dbl_not_na <- .assert_scalar_double_not_na

# -------------------------------------------------------------------------
.assert_scalar_numeric <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.numeric(x) && length(x) == 1L))
        stopf("`%s` must be a numeric vector of length 1.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_num <- .assert_scalar_numeric

# -------------------------------------------------------------------------
.assert_scalar_numeric_not_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.numeric(x) && length(x) == 1L) || is.na(x))
        stopf("`%s` must be a numeric vector of length 1 and not NA.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_num_not_na <- .assert_scalar_numeric_not_na

# -------------------------------------------------------------------------
.assert_scalar_logical <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.logical(x) && length(x) == 1L))
        stopf("`%s` must be a logical vector of length 1.", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_scalar_lgl <- .assert_scalar_logical

# -------------------------------------------------------------------------
.assert_bool <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.logical(x) && length(x) == 1L) || is.na(x))
        stopf("`%s` must be boolean (TRUE/FALSE).", arg, .call = call)
}

# -------------------------------------------------------------------------
.assert_boolean <- .assert_bool

# -------------------------------------------------------------------------
.assert_scalar_character <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.character(x) && length(x) == 1L))
        stopf("`%s` must be a character vector of length 1.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_scalar_chr <- .assert_scalar_character

# -------------------------------------------------------------------------
.assert_scalar_character_not_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!(is.character(x) && length(x) == 1L) || is.na(x))
        stopf("`%s` must be a character vector of length 1 and not NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_scalar_chr_not_na <- .assert_scalar_character_not_na

# -------------------------------------------------------------------------
.assert_string <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_scalar_chr(x = x, arg = arg, call = call)
}

# -------------------------------------------------------------------------
.assert_non_negative_or_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_non_negative_or_na(x))
        stopf("`%s` values must be non-negative or NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_non_positive_or_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_non_positive_or_na(x))
        stopf("`%s` values must be non-positive or NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_non_negative <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_non_negative(x))
        stopf("`%s` values must be non-negative and not NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_non_positive <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_non_positive(x))
        stopf("`%s` values must be non-positive and not NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_positive <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_positive(x))
        stopf("`%s` values must be positive and not NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_negative <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_negative(x))
        stopf("`%s` values must be negative and not NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_positive_or_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_positive_or_na(x))
        stopf("`%s` values must be positive or NA.", arg, .call = call)

}

# -------------------------------------------------------------------------
.assert_negative_or_na <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (!is.numeric(x))
        stopf("`%s` must be a numeric vector.", arg, .call = call)

    if (!.all_negative_or_na(x))
        stopf("`%s` values must be negative or NA.", arg, .call = call)

}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.assert_not_missing <- function(x, arg, call) {
    if (missing(x))
        stopf("argument `%s` is missing, with no default.", arg, .call = call)
}

# -------------------------------------------------------------------------

.all_non_negative_or_na <- function(x) {
    min(0, x, na.rm = TRUE) >= 0
}

# -------------------------------------------------------------------------

.all_non_positive_or_na <- function(x) {
    max(0, x, na.rm = TRUE) <= 0
}

# -------------------------------------------------------------------------

.all_non_negative <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    min <- min(x)
    if (is.na(min))
        return(FALSE)
    if (min >= 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_non_positive <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    max <- max(x)
    if (is.na(max))
        return(FALSE)
    if (max <= 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_positive <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    min <- min(x)
    if (is.na(min))
        return(FALSE)
    if (min > 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_negative <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    max <- max(x)
    if (is.na(max))
        return(FALSE)
    if (max < 0) TRUE else FALSE
}

# -------------------------------------------------------------------------

.all_positive_or_na <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    nax <- is.na(x)
    if (sum(nax) == length(x))
        return(TRUE)
    if (min(x, na.rm = TRUE) <= 0) FALSE else TRUE
}

# -------------------------------------------------------------------------

.all_negative_or_na <- function(x) {
    if (length(x) == 0L)
        return(TRUE)
    nax <- is.na(x)
    if (sum(nax) == length(x))
        return(TRUE)
    if (max(x, na.rm = TRUE) >= 0) FALSE else TRUE
}

# -------------------------------------------------------------------------
