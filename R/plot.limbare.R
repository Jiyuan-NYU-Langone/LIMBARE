#' plot.limbare
#'
#' Description
#'
#' Details
#'
#' @param model limbare model object
#' @param seg.Z the segmented variable(s), i.e. the continuous
#' covariate(s) which have a piece-wise linear relationship with
#' the response variable. It is a formula with no response variable,
#' such as seg.Z=~x.
#' @param intercept a constant to adjust the piece-wise lines vertically.
#' Default is 0.
#' @param breakpoint If TRUE, a vertical line to indicate the estimated
#' breakpoints will be presented. Default is FALSE.
#' @param breakpoint.CI If TRUE, two vertical lines to indicate the 95% CI
#' of the estimated breakpoints will be presented. Default is FALSE.
#' @param ... some settings for this generic require additional arguments.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' plot.limbare(model, seg.Z = ~x, intercept = 50, break.point = T,
#'              break.point.CI = T, color = "red")+
#'   geom_point(data=obs_data, aes(x, y), color = "red")+
#'   geom_point(data = model$data, aes(x, y))

plot.limbare <- function(model, seg.Z, intercept = 0,
                         break.point = FALSE,
                         break.point.CI = FALSE, ...){
  x.breaks = c(range(model$data[, as.character(seg.Z)[2]])[1],
               model$psi[, "Est."],
               range(model$data[, as.character(seg.Z)[2]])[2])
  slo = slope(model, seg.Z)[, "Estimate"]
  npsi = nrow(model$psi)

  y.breaks = c(slo[1]*x.breaks[1])
  for (i in 1:(npsi+1)){
    y.breaks = c(y.breaks, y.breaks[i]+(x.breaks[(i+1)]-x.breaks[i])*slo[i])
  }

  y.breaks = intercept + y.breaks

  p=ggplot()+xlim(range(x.breaks))+ylim(range(y.breaks))
  for (i in 1:(npsi+1)){
    p <- p+geom_segment(aes_string(x = x.breaks[i], y = y.breaks[i],
                                   xend = x.breaks[i+1], yend = y.breaks[i+1]), ...)
  }

  if (intercept==0){
    p <- p+theme_bw()+theme(axis.text.y = element_blank())+
      xlab(as.character(model$seg.Z)[2])+
      ylab(as.character(model$model$terms[[2]]))
  } else {
    p <- p+theme_bw()+
      xlab(as.character(model$seg.Z)[2])+
      ylab(as.character(model$model$terms[[2]]))
  }
  if (break.point){
    for (i in 1:npsi)
      p <- p+geom_vline(xintercept = model$psi[i, "Est."], color = "darkgray")
  }

  if (break.point.CI){
    for (i in 1:npsi){
      p <- p+geom_vline(xintercept = (model$psi[i, "Est."]-1.96*model$psi[i, "St.Err"]), color = "darkgray", linetype = "dashed")+
        geom_vline(xintercept = (model$psi[i, "Est."]+1.96*model$psi[i, "St.Err"]), color = "darkgray", linetype = "dashed")
    }
  }

  return(p)
}
