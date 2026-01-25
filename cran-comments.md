This is a resubmission of a previously archived package.

## R CMD check results
0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mark Baillie <bailliem@gmail.com>'
New submission
Package was archived on 2025-11-19 as it required the archived package 'rbmi'.

## Resubmission Notes
* The dependency 'rbmi' is now back on CRAN, resolving the original reason for archiving.
* Moved 'tidyr' from Suggests to Imports (required for exported function 'tidy_pool_obj()').
* Added examples to 'gcomp_responder()' documentation.
* Updated all examples to use the native pipe operator (|>).
