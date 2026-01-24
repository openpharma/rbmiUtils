## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mark Baillie <bailliem@gmail.com>'

New submission
Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2025-11-19 as requires archived package
    'rbmi'

Package was archived on CRAN due to 'rbmi' archived. 
* This version is a minor update and re-release after rbmi is now available on cran. 
* Moved `tidyr` from Suggests to Imports as it is used in exported function `tidy_pool_obj()`.
* Added examples to `gcomp_responder()` documentation.
* Standardized examples to use native pipe operator `|>`.
