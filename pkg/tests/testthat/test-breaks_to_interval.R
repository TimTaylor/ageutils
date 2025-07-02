test_that("Errors work as expected", {
    expect_error(suppressWarnings(breaks_to_interval(c(Inf, 0, 10.3, 1000))))
    expect_snapshot(error = TRUE, breaks_to_interval(c(Inf, 0, 10.3, 1000)))

    expect_error(breaks_to_interval(c(NA_real_, 0, 10.3, 1000)))
    expect_snapshot(error = TRUE, breaks_to_interval(c(NA_real_, 0, 10.3, 1000)))

    expect_error(breaks_to_interval(c(1, 0, 10.3, 1000)))
    expect_snapshot(error = TRUE, breaks_to_interval(c(1, 0, 10.3, 1000)))

    expect_error(breaks_to_interval(c(0, 0, 10.3, 1000)))
    expect_snapshot(error = TRUE, breaks_to_interval(c(0, 0, 10.3, 1000)))

    expect_error(breaks_to_interval(TRUE))
    expect_snapshot(error = TRUE, breaks_to_interval(TRUE))

    expect_error(suppressWarnings(breaks_to_interval(.Machine$integer.max + 1)))
    expect_snapshot(error = TRUE, breaks_to_interval(.Machine$integer.max + 1))

    expect_error(breaks_to_interval(1, TRUE))
    expect_snapshot(error = TRUE, breaks_to_interval(1, TRUE))

    expect_error(breaks_to_interval(1, 1:2))
    expect_snapshot(error = TRUE, breaks_to_interval(1, 1:2))

    expect_error(breaks_to_interval(1, NA_real_))
    expect_snapshot(error = TRUE, breaks_to_interval(1, NA_real_))

    expect_error(breaks_to_interval(1, 1))
    expect_snapshot(error = TRUE, breaks_to_interval(1, 1))
})


test_that("breaks_to_interval gives correct result", {

    brks <- c(-55.3, 0, 10.3, 1000)
    lower <- c(-55,0,10,1000)
    upper <- c(0,10,1000,Inf)
    interval <- sprintf("[%.f, %.f)", lower, upper)
    interval <- factor(interval, levels = interval, ordered = TRUE)
    expected <- tibble::tibble(interval, lower = lower, upper = upper)

    expect_equal(breaks_to_interval(brks), expected)

    expect_equal(breaks_to_interval(brks), expected)

    expect_equal(tibble::validate_tibble(breaks_to_interval(brks)), expected)

    brks[1L] <- -3
    expect_silent(breaks_to_interval(brks))


    expected <- tibble::tibble(
        interval = factor("[1, 2)", levels = "[1, 2)", ordered = TRUE),
        lower = 1,
        upper = 2
    )

    expect_identical(breaks_to_interval(1, 2), expected)

    expect_equal(tibble::validate_tibble(breaks_to_interval(1, 2)), expected)

})
