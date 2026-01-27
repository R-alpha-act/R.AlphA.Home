## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission after archival. Last CRAN version (1.1.0) required archived package 'qs'.

## Changes since 1.1.0

### v2.0.2 (this submission)
- Bug fix: ralpha_fold()/ralpha_unfold() timing issue with async executeCommand

### v2.0.0
- Breaking: root() now accepts ... for path building
- New: root() can build paths relative to source file directory
- Improvement: Rtimer output simplified

### v1.1.3 (never submitted to CRAN)
- Removed qs/arrow dependencies (parquet/feather/qs formats dropped from importAll)
- Removed stringi/stringr (replaced by base R gsub/grepl)
- Replaced openxlsx with readxl + writexl
- Replaced diffr with diffobj
- Dependencies reduced from 25 to 12 imports (-52%)

## Test environments

* local macOS (darwin), R 4.x
* GitHub Actions (ubuntu, macOS, windows)
