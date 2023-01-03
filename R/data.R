#' Aggregated population data
#'
#' A dataset derived from the 2021 UK census containing population for different
#' age categories across England and Wales.
#'
#' @format A data frame with 200 rows and 6 variables:
#' \describe{
#'   \item{area_code}{Unique area identifier}
#'   \item{area_name}{Unique area name}
#'   \item{age_category}{Left-closed and right-open age interval}
#'   \item{value}{count of individ}
#' }
#' @source \url{https://github.com/TimTaylor/census_pop_2021}
"pop_dat"
