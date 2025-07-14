# optimised grouped summation
.fast_grouped_sum <- function(x, by, byname, sumname) {
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
.ave_sum <- function(x, by) {
    group_loc <- vec_group_loc(by)
    key <- group_loc$key
    loc <- group_loc$loc
    chopped <- vec_chop(x, indices = loc)
    out <- lapply(chopped, sum)
    list_unchop(out, indices = loc)
}
