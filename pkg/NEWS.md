# ageutils 0.1.2

* Small tweaks to vignette css and js.

# ageutils 0.1.1

* Added an introductory vignette.

* Small number of documentation improvements.

# ageutils 0.1.0

## Fixes

* Fix bug in the incorrect calculation of some counts in `reaggregate_rates()`.
  Thanks to Neil Wilkins for the report.

## Breaking changes

* `cut_ages()` and `breaks_to_interval()` now return columns named `lower` and
  `upper` as opposed to `lower_bounds` and `upper_bound` respectively. This is
  for consistency with the output of `reaggregate_counts()` and
  `reaggregate_rates()` (as well as the convenience of brevity).
  
* `reaggregate_counts()` and `reaggregate_rates()` are no longer implemented as
  generics.

## Other

* Motivated by the recent bug reports from Neil, `reaggregate_counts()` and
  `reaggregate_rates()` have been refactored internally to use a simpler
  approach. Whilst these functions are now a little less performant for our
  common use cases, they are, hopefully, easier to understand.

# ageutils 0.0.9

* Fix erroneous input assertion in `reaggregate_counts()` in relation to
  the arguments `bounds` and `population_bounds`. Thanks to Neil Wilkins for
  flagging.
  
* Fix bug in the incorrect calculation of some counts in `reaggregate_counts()`.
  Thanks again to Neil Wilkins for the report.
  
* Clarify one error message in `reaggregate_rates()`.

# ageutils 0.0.8

* The following functions which were deprecated in the 0.0.5 release have now
  been removed from the package and will now signal an error of class
  defunctError if called.

    - `aggregate_age_counts()`
    - `reaggregate_interval_counts()`
    - `reaggregate_interval_rates`
    - `split_interval_counts()`
  
  The use of these could be a little confusing and it was hard to achieve
  consistency with their APIs. `reaggregate_counts()` and `reaggregate_rates()`
  can be used to replicate most of the old functionality.

# ageutils 0.0.7

* Fixed bug in `reaggregate_rates()` where, in some circumstances, the last
  row of the data frame output would have an incorrect value.

# ageutils 0.0.6

* Simplifies the internal implementation of `cut_ages()`. This refactoring has
  resulted in minor tweaks to the given error messages as well as a small bug 
  fix relating to how `max_upper` argument was being rounded (i.e. we were
  calling `round()` rather than raising to the next integer(ish) number with
  `ceiling()` as intended).

# ageutils 0.0.5

* Adds new functions `reaggregate_counts()` and `reaggregate_rates()` which
  provide a simplified API for reaggregation compared to
  `reaggregate_interval_counts()` and `reaggregate_interval_rates()`. These
  use an elegant approach to reaggregation suggested by @BlackEdder.
  
* Deprecates (with warning of class "deprecatedWarning") the following functions:

    - `aggregate_age_counts()`
    - `reaggregate_interval_counts()`
    - `reaggregate_interval_rates`
    - `split_interval_counts()`
  
  The use of these could be a little confusing and it was hard to achieve
  consistency with their APIs. `reaggregate_counts()` and `reaggregate_rates()`
  can be used to replicate most of the old functionality.
  
# ageutils 0.0.4

* Fixes an error in `reaggregate_interval_rates()` which caused the first entry
  of the output to be incorrect when the `breaks` were not specified from `0`.
  Thanks to @BlackEdder for the report.

# ageutils 0.0.3

* Internal changes only.

# ageutils 0.0.2

*  `aggregate_age_counts()` will now only return a row corresponding to NA ages
  if they were present in the input data. Previously an NA-associated row would
  always be returned even if it's count was 0. Due to this change
  `reaggregate_interval_counts()` will now never return an NA-associated row.

* `split_interval_counts()` now matches the documentation and disallows missing
  (NA) bounds.

* `breaks_to_interval()` and `cut_ages()` both gain an argument, `max_upper`
  which allows users to explicitly set the maximum upper bound.
  
* New function `reaggregate_interval_rates()`.

* For the vignette we now use
  [markdown](https://cran.r-project.org/package=markdown) as a lighter
  alternative to [rmarkdown](https://cran.r-project.org/package=rmarkdown).

# ageutils 0.0.1

Initial release of `ageutils` which provides a collection of efficient functions
for working with individual ages and corresponding intervals. These include
functions for efficient conversion from an age to an interval, aggregation of
ages with associated counts in to intervals and the splitting of interval counts
based on specified age distributions.

Functions are derived from those in the
[ympes](https://cran.r-project.org/package=ympes) with the intention being to
remove these functions from that package in favour of this one going forward.
