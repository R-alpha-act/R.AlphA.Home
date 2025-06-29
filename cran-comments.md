# cran-comments.md

## Resubmission
This is a new version of the R.AlphA.Home package (1.1.0), updating from version
1.0.0 which was previously accepted on CRAN.

## Test environments
* local macOS install, R 4.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs 
NOTES : maintainer mail change

## Major changes in this version

### New features
* Added `ralpha_fold()` and `ralpha_unfold()` suggested to ctrl+up/down
* New `tmr()` function: improved timer using static variables for better performance
* New `setOption()` function: convenient way to set global options from named lists
* Added `printif()` function: conditional printing based on debug levels

### Deprecations
* `timer()` is now deprecated in favor of the new `tmr()` function
* `foldAllBr()` is now deprecated in favor of `ralpha_fold()` and `ralpha_unfold()`

### Bug fixes
* Fixed `importAll()` handling of different path and fileList combinations
* Improved column type harmonization in `importAll()` when importing multiple files

## Backward compatibility
All changes are backward compatible. Deprecated functions still work but issue
# deprecation warnings directing users to the new alternatives.

## Dependencies
No new dependencies were added in this version. All existing dependencies remain unchanged.

## Examples and tests
* All examples run in < 5 seconds
* Added tests for new functions
* Examples requiring RStudio are properly wrapped in conditional checks

## Notes on RStudio-dependent functions
The new `ralpha_fold()` and `ralpha_unfold()` functions require RStudio to work.

## Reverse dependencies
There are currently no reverse dependencies for this package.
