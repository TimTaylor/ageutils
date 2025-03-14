ages_groups_to_factor <- function(x) {
    lvls <- x[!is.na(x)]
    lvls <- unique(lvls)
    index <- as.integer(sub("\\[([[:digit:]]+),.*", "\\1", lvls))
    index <- order(index)
    factor(x, levels = lvls[index], ordered = TRUE)
}
