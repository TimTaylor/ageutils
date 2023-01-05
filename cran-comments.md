## R CMD check results

* checked on R Under development (unstable) (2023-01-04 r83558) - Fedora 37
* checked on R Under development (unstable) (2023-01-04 r83561 ucrt) - win-builder

0 errors | 0 warnings | 1 note

* This is a new release.


## Additional comments

* This package extracts the functionality for working with age categories from
  the ympes package which I also maintain. The motivation for this is to keep
  ympes focussed on small "helper functions" whilst the functionality here is
  more focussed. It has the added benefit of removing the compiled code from
  ympes.
  
* Assuming this is accepted - I will push a release of ympes, deprecating
  the overlapping functionality and, in a subsequent release making it defunct.
