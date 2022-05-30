## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* There is a call `:::` in a package to refer to its own object.
However, this call in reality is in the user's environment, not
package environment. Package adds to the function chosen by user
some code (on-the-fly) and one element of this code is internal
function - `shinybreakpoint:::get_envir()`.
