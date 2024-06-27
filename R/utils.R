stopf <- function(fmt, ..., .use_call = TRUE, .call = sys.call(-1L)) {
    .call <- if (isTRUE(.use_call)) .call[1L] else NULL
    msg <- sprintf(fmt, ...)
    err <- simpleError(msg, .call)
    stop(err)
}

warningf <- function(fmt, ..., .use_call = TRUE, .call = sys.call(-1L)) {
    .call <- if (isTRUE(.use_call)) .call[1L] else NULL
    msg <- sprintf(fmt, ...)
    err <- simpleWarning(msg, .call)
    warning(err)
}

if (getRversion() < '4.4.0') {
    `%||%` = function(x, y) if (is.null(x)) y else x
}
