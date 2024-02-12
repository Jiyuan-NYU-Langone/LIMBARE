#' slope
#'
#' Description
#' Returns the estimated slopes and standard errors of each segment.
#'
#' Details
#'
#' @param model limbare model object
#' @param seg.Z the segmented variable(s), i.e. the continuous
#' covariate(s) which have a piece-wise linear relationship with
#' the response variable. It is a formula with no response variable,
#' such as seg.Z=~x.
#'
#' @export
#'
#' @examples
#' slope(model = model, seg.Z = ~x)

slope <- function(model, seg.Z){
  var_name=setdiff(grep(as.character(seg.Z)[2], row.names(model$fixed), value = TRUE),
                   grep("psi", row.names(model$fixed), value = TRUE))
  Estimate=cumsum(model$fixed[var_name, "Est."])

  std.err=c()
  for (i in 1:length(var_name)){
    std.err=c(std.err,
              sqrt(t(1:i) %*% model$covariance_matrix[var_name[1:i], var_name[1:i]] %*% 1:i))
  }
  p.value=ifelse(2*pnorm(Estimate, mean = 0, sd = std.err)>1,
                 2*(1-pnorm(Estimate, mean = 0, sd = std.err)),
                 2*pnorm(Estimate, mean = 0, sd = std.err))
  lb_95=Estimate-1.96*std.err
  ub_95=Estimate+1.96*std.err

  M=cbind(Estimate, std.err, p.value, lb_95, ub_95)
  row.names(M)=paste("Slope", 1:length(var_name), sep = ".")
  return(M)
}
