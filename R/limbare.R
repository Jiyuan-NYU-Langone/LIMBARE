#' limbare
#'
#' Description
#' A robust method to estimate breakpoints on longitudinal studies.
#' This method combined with least trimmed squared technique to
#' accommodate outliers in the dataset. Fits linear mixed models
#' with piece-wise relationships between response and one or more
#' covariates.
#'
#' Details
#'
#' @param mixed.model linear mixed effects model of class "lme".
#' @param alpha percentage of data after trimming
#' @param seg.Z the segmented variable(s), i.e. the continuous
#' covariate(s) which have a piece-wise linear relationship with
#' the response variable. It is a formula with no response variable,
#' such as seg.Z=~x or seg.Z=~x1+x2.
#' @param npsi A named vector or list meaning the number (and not locations) of breakpoints to be estimated.
#' @param tol tolerance level
#' @param max.iter max number of iterations
#'
#' @import nlme
#' @import segmented
#' @import dplyr
#' @export
#'
#' @examples
#' obs_data=sample_data
#'
#' mixed.model=lme(y~x+duration, random = ~1|Subject_ID/Eye, data=obs_data, na.action = na.omit)
#' model=limbare(mixed.model, seg.Z = ~x, npsi=list(x=1), alpha=0.9, tol = 0.005, max.iter = 200)
#'
#' model$psi
#' summary(model$model)

limbare <- function(mixed.model, alpha, seg.Z, npsi, tol, max.iter){
  set.seed(99)
  #tryCatch({
  converge=10
  track=0
  data1=as.data.frame(getData(mixed.model))
  index <- sample(seq(1, nrow(data1), 1), nrow(data1)*alpha, replace = FALSE)
  while(converge > tol & track < max.iter){
    data_trim<- data1[index, ]
    seg_model<-segmented.default(update(mixed.model, data=data_trim),
                                 seg.Z = seg.Z, npsi=npsi)

    data1$resid2=(data1[, as.character(getResponseFormula(mixed.model))[2]]-predict.segmented1(seg_model, data1, .coef=fixef(seg_model)))^2
    data1=data1[order(data1$resid2), ]
    converge<- 1 - length(intersect(row.names(data1)[1:(nrow(data1)*alpha)],index))/(nrow(data1)*alpha)
    track<- track+1
    index <- row.names(data1)[1:(nrow(data1)*alpha)]
  }

  data_new=getData(seg_model)
  data_new$pred=predict.segmented1(seg_model, data_new, .coef=fixef(seg_model))
  data_new=rbind(data_new, data_new[sample(1:nrow(data_new), nrow(data_new)*(1-alpha)), ])

  sd1=get_sd_from_quantile_score(0, (1+alpha)/2, max(residuals(seg_model)) )
  sd2=get_sd_from_quantile_score(0, (1-alpha)/2, min(residuals(seg_model)) )

  sd_new=mean(c(sd1, sd2))
  data_new$y_new=data_new$pred+rnorm(nrow(data_new), mean = 0, sd=sd_new)

  fixed_new=eval(paste("y_new", as.character(getCovariateFormula(mixed.model))[1], as.character(getCovariateFormula(mixed.model))[2]))
  seg_model_new=segmented.default(update(mixed.model, fixed=fixed_new, data=data_new),
                                  seg.Z = seg.Z, npsi=npsi)

  psi=cbind(seg_model$psi[, 2], seg_model_new$psi[, 3])
  colnames(psi)=c("Est.", "St.Err")
  fixed=cbind(coef(summary(seg_model))[, "Value"], coef(summary(seg_model_new))[, "Std.Error"])
  colnames(fixed)=c("Est.", "St.Err")
  fixed=cbind(fixed, p.value=2*(1-pnorm(abs(fixed[, "Est."]), mean=0, sd=fixed[, "St.Err"])))
  #return(list(seg_model, seg_model_new))
  return(list(data=getData(seg_model),
              model=seg_model, alpha=alpha, psi=psi, fixed=fixed,
              seg.Z=seg.Z,
              covariance_matrix=vcov(seg_model_new)))
  #}, error = function(e){
  #  "ERROR"}
  #)
}
