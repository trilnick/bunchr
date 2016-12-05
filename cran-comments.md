Sorry for updating this package so soon. I had forgotten to include a new function
as an export in the NAMESPACE file. It still worked normally on my local machine, and the function was included in the documentation, so I didn't realize something was wrong. Only once I updated the package from CRAN, I realized that R was not finding this function, even when the package was attached.

## Test environments
* local OS Windows 10, R version 3.3.2
* ubuntu 12.04.5 (on travis-ci), R version 3.3.1

## R CMD check results
No errors, warnings, or notes

## Downstream dependencies
There are currently no downstream dependencies for this package.
