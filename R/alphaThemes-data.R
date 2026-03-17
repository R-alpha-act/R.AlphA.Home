#' R.AlphA theme registry
#'
#' Named list of R.AlphA editor themes. Each theme contains a \code{colElts}
#' list (color elements used by \code{\link{RSTheme_dark}}) and a \code{thName}
#' string.
#'
#' @format A named list with entries:
#' \describe{
#'   \item{ralpha_dark}{Dark theme (default)}
#'   \item{ralpha_mid}{Mid-tone theme (work in progress)}
#' }
#'
#' @examples
#' names(alphaThemes)
#' names(alphaThemes$ralpha_dark$colElts)
"alphaThemes"
