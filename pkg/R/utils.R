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

# optimised grouped summation
.fgsum <- function(x, by, byname, sumname) {
    group_loc <- vec_group_loc(by)
    key <- group_loc$key
    loc <- group_loc$loc
    order <- order(key)
    key <- key[order]
    loc <- loc[order]
    chopped <- vec_chop(x, indices = loc)
    out <- list2DF(list(key = key, sum = vapply(chopped, sum, 1)))
    names(out) <- c(byname, sumname)
    out
}

# optimised implementation of ave(x, by, FUN = sum)
.fsum <- function(x, by) {
    group_loc <- vec_group_loc(by)
    key <- group_loc$key
    loc <- group_loc$loc
    chopped <- vec_chop(x, indices = loc)
    out <- lapply(chopped, sum)
    list_unchop(out, indices = loc)
}
