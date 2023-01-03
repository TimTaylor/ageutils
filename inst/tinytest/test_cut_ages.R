# single limit
dat <- 1:10
limit <- 5L
lower_bound <- rep.int(c(0, 5), times = c(4, 6))
upper_bound <- rep.int(c(5, Inf), times = c(4, 6))
expected <- data.frame(
    interval = factor(
        sprintf("[%s, %s)", lower_bound, upper_bound),
        levels = c("[0, 5)", "[5, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)

expect_identical(cut_ages(dat, c(0L, limit)), expected)

# multiple limits
dat <- c(1:5, 99:102)
limit <- c(3L, 98L)
lower_bound <- rep.int(c(0, 3, 98), times = c(2L, 3L, 4L))
upper_bound <- rep.int(c(3, 98, Inf), times = c(2L, 3L, 4L))
expected <- data.frame(
    interval = factor(
        sprintf("[%s, %s)", lower_bound, upper_bound),
        levels = c("[0, 3)", "[3, 98)", "[98, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, c(0L, limit)), expected)

# multiple limits with ages below minimum
dat <- c(1:5, 99:102)
limit <- c(3L, 98L)
lower_bound <- rep.int(c(NA_real_, 3, 98), times = c(2L, 3L, 4L))
upper_bound <- rep.int(c(NA_real_, 98, Inf), times = c(2L, 3L, 4L))
tmp <- sprintf("[%s, %s)", lower_bound, upper_bound)
tmp[1:2] <- NA_character_
expected <- data.frame(
    interval = factor(tmp, levels = c("[3, 98)", "[98, Inf)"), ordered = TRUE),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, limit), expected)

# NA handled correctly
dat <- c(1:5, 99:102)
dat[[1L]] <- NA_integer_
limit <- c(3L, 98L)
lower_bound <- rep.int(c(0, 3, 98), times = c(2L, 3L, 4L))
upper_bound <- rep.int(c(3, 98, Inf), times = c(2L, 3L, 4L))
interval <- sprintf("[%s, %s)", lower_bound, upper_bound)
lower_bound[[1L]] <- NA_real_
upper_bound[[1L]] <- NA_real_
interval[[1L]] <- NA_character_

expected <- data.frame(
    interval = factor(
        interval,
        levels = c("[0, 3)", "[3, 98)", "[98, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, c(0L, limit)), expected)

# limits greater than values
dat <- 1:5
limits <- 6:7
lower_bound <- rep.int(0, 5L)
upper_bound <- rep.int(6, 5L)
interval <- sprintf("[%s, %s)", lower_bound, upper_bound)
expected <- data.frame(
    interval = factor(
        interval,
        levels = c("[0, 6)", "[6, 7)", "[7, Inf)"),
        ordered = TRUE
    ),
    lower_bound = lower_bound,
    upper_bound = upper_bound
)
expect_identical(cut_ages(dat, c(0L, limits)), expected)

# all NA ages handled correctly
dat <- rep.int(NA_real_, 5L)
limits <- 6:7
expected <- data.frame(
    interval = factor(
        dat,
        levels = c("[0, 6)", "[6, 7)", "[7, Inf)"),
        ordered = TRUE
    ),
    lower_bound = dat,
    upper_bound = dat
)
expect_identical(cut_ages(dat, c(0L, limits)), expected)

# error messaging
expect_error(cut_ages("bob"))

expect_error(
    cut_ages("bob", 3),
    "`ages` must be numeric.",
    fixed = TRUE
)

expect_error(
    cut_ages(1:10, breaks = "5L"),
    "`breaks` must be numeric.",
    fixed = TRUE
)

expect_error(
    cut_ages(-1:10, 5L),
    "`ages` must be in the interval `[0, 200)` or NA.",
    fixed = TRUE
)

expect_error(
    cut_ages(1:10, breaks = NA_integer_),
    "`breaks` must be non-negative and coercible to integer.",
    fixed = TRUE
)

expect_error(
    cut_ages(1:10, breaks = c(2L, 2L)),
    "`breaks` must be non-negative and in strictly increasing order.",
    fixed = TRUE
)

