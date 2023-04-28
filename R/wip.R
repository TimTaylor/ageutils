ages_groups_to_factor <- function(x) {
    lvls <- x[!is.na(x)]
    lvls <- unique(lvls)
    index <- as.integer(sub("\\[([[:digit:]]+),.*", "\\1", lvls))
    index <- order(index)
    factor(x, levels = lvls[index], ordered = TRUE)
}


make_weight_vector <- function(lower, upper, weight) {
    # lower <- c(3,  8, 10)
    # upper <- c(6, 10, 15)
    # weight <- c(1,  2,  3)
    # make_weight_vector(lower,upper,weight)

    n <- max(upper)
    weights_out <- numeric(length = n)
    ages <- seq_len(n) - 1L
    idx <- unlist(lapply(seq_along(lower), \(i) lower[i]:(upper[i] - 1)))
    weights_out[idx + 1L] <- rep.int(weight, (upper - lower))
    data.frame(age = ages, weight = weights_out)
}


