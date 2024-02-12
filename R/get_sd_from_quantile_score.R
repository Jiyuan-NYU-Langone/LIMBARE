#' get_sd_from_quantile_score
#'
#' Description
#'
#' Details
#'
#' @param m mean
#' @param y standard deviation
#' @param p vector of probabilities.
#'
#' @import nlme
#' @import segmented
#' @import dplyr
#' @noRd


get_sd_from_quantile_score <- function(m, p, x) {
  get_quantile_score <- function(y) {
    qnorm(mean=m,sd=y,p=p)
  }
  f <- function(y) { (get_quantile_score(y) - x)^2 }
  opt <- optim(par = 1, fn = f, method = 'CG')
  return(opt$par)
}
