#' Simulate experiment trials
#'
#' @param n.trials numerical integer number of trials you want in total in your AR2 process
#' @param sd.dev.innov numerical integer the standard deviation of the innovation of the time series
#' @param a1 numerical Yule walker parameter alpha1 - how much you weight the previous lag1 data point
#' @param a2 numerical Yule walker parameter alpha2 - how much you weight the lag2 data point
#' @param gen.mean numerical integer Generative mean of the underlying process which is added to the AR2 series. You can use 0 if you do
#' not want anything added, but otherwise it is a time series as long as n.trials
#' @param accept.sd.value numerical the standard deviation of the time series we want
#' @param bound acceptable
#'
#' @return dataset - tibble with all trial info in it
#' @export
#'
#' @examples
simulate_experiment_trials <- function(n.trials, sd.dev.innov, a1, a2, gen.mean, accept.sd.value, bound){
  ## Description: this function simulates autocorrelated data with a lag of 2,
  ## given a set of Yule Walker coefficients (a1, a2) and a specific SD value
  ## input. In order to get a time series with a specific standard deviation you need to know
  ## what the standard deviation of the white noise parameter is which you can then use to
  ## solve an analytic solution for the standard deviation of the actual time series
  ## Note: we use rounding to calculate the sd so the "accept.sd.value" will not be exact
  # -------------------------------

  n.burnin <- 1000
  n.samples <- n.trials + n.burnin

  ## Ideally we want a point that is between the lower and upper bound
  ## First create something that is outside the bound so we can get the while loop going
  ## so we have an initial value
  sd.timeseries <- accept.sd.value * 2 # add more than acceptable bound

  # While the sd.value is between
  while (!between(sd.timeseries, accept.sd.value - bound, accept.sd.value + bound)){
    ## Get white noise
    E <- rnorm(n = n.samples+ 2, mean = 0, sd = sd.dev.innov)

    ## Zeros for now
    Y <- matrix(0, n.samples)

    ## Update rule according to Yule Walker Equations
    for (i in 3:n.samples){

      ## Current sample = white noise + lag1 + lag2
      Y[i] <- E[i+2] + a1*Y[i-1] + a2*Y[i-2]

    }

    ## Take burn.in of samples so that the AR process has time to stablise
    Y <- Y[(n.burnin+1):length(Y)]
    Y <- Y + gen.mean
    dataset <- tibble(ar2_samples = Y)
    dataset$sd <- sd(dataset$ar2_samples)

    sd.timeseries <- dataset$sd[1]
  }

  return(dataset)
}




