#' Plot Survival Curve
#'
#' This function takes numerical vectors for status and time, and plots the survival curve S(t).
#' @param status Numeric vector indicating the status (usually 1 = event occurred, 0 = censored).
#' @param time Numeric vector indicating the time to event or censorship.
#' @importFrom survival Surv survfit
#' @importFrom graphics plot
#' @return The plot of the survival curve.
#' @export
#' @examples
#' # Assuming `status` and `time` vectors are defined or loaded from external data
#' survCurv(status, time)
survCurv = function(status, time) {
  require(survival)
  surv_object = Surv(time = time, event = status)
  surv_fit = survfit(surv_object ~ 1)
  plot(surv_fit, xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
  return(invisible(surv_fit))
}

# Test dataset
#  library(survival)
#  data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#  survCurv(data$status, data$time)

