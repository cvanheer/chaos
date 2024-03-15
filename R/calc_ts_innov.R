#' calc time series innovation
#'
#' @param a1 Yule walker parameter alpha1 - how much you weight the previous lag1 data point
#' @param a2 Yule walker parameter alpha2 - how much you weight the lag2 data point
#' @param sigma.ar2 the standard deviation of the entire time series
#'
#' @return sigma.innov the standard deviation of the innovation of the time series
#' @export
#'
#' @examples calc.ts.innov(0.6, -0.4, 15)
calc.ts.innov <- function(a1, a2, sigma.ar2){
  ## ---------------------------------------------------------------------
  ## Description: uses the desired standard deviation of an AR2 time series
  ## to work out what the standard deviation of the accompanying white
  ## noise process should be according to an equation in Gottman (1985)
  ## page 128 Part III of book "Time Series Analysis"
  ## Returns: the standard deviation of the white noise series (also called
  ## innovations)
  ## NOTE: this equation is a TRANSFORMED version of the equation in Gottman
  ## which tells you how you how to get the SD of a time series given the
  ## innovation SD but we want the reverse so you have to multiply both sides
  ## by (sigma.ts^2) * ((1 + a2) * ( 1 - a1 - a2 ) * (1 + a1 - a2)) and divide by (1-a2)
  ## DO NOT PANIC!
  ## ---------------------------------------------------------------------
  # Note we square the time series SD because this equation calls for the
  # variance
  numerator <- (sigma.ar2^2) * ((1 + a2) * ( 1 - a1 - a2 ) * (1 + a1 - a2))
  denominator  <-  (1 - a2)

  # Take the square root of the variance to get back to a SD
  sigma.innov <- sqrt(numerator/denominator)

  return(sigma.innov)
}
