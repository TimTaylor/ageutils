cut_ages_old <- function(ages, breaks) {
    .Call(C_cut_ages, ages, breaks)
}
