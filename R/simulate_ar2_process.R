#' simulate_ar2_process.R
#' @param n.trials numerical integer number of trials you want in total in your AR2 process
#' @param a1 numerical Yule walker parameter alpha1 - how much you weight the previous lag1 data point
#' @param a2 numerical Yule walker parameter alpha2 - how much you weight the lag2 data point
#' @param gen.mean numerical integer Generative mean of the underlying process which is added to the AR2 series. You can use 0 if you do
#' not want anything added, but otherwise it is a time series as long as n.trials
#' @param sigma.ar2 underlying standard deviation of the AR2 process
#' @param sigma.ar2.bound numerical acceptable tolerance for SD generated e.g within 0.01 plus or minus of gen_sd
#' @param sigma.innov innovation/error standard deviation in the AR2 process
#' @param n.burnin numerical the number of samples you want to throw out before sampling the AR2 process
#' @return dataset - tibble with all trial info in it
#' @export
#' @importFrom dplyr between
#' @importFrom tibble tibble
#' @importFrom stats rnorm
#' @importFrom chaos chaos_palette
#' @examples simulate_ar2_process(n.trials = 280, a1 = 0.5, a2 = 0.3, gen.mean= 0, sigma.ar2 = 15, sigma.ar2.bound = 0.01, sigma.innov = 10, n.burnin = 1000)
simulate_ar2_process <- function(n.trials, a1, a2, gen.mean, sigma.ar2, sigma.ar2.bound, sigma.innov, n.burnin){
  ## Description: this function simulates auto correlated data with a lag of 2,
  ## given a set of Yule Walker coefficients (a1, a2) and a specific SD value
  ## input. In order to get a time series with a specific standard deviation you need to know
  ## what the standard deviation of the white noise parameter is which you can then use to
  ## solve an analytic solution for the standard deviation of the actual time series
  ## Note: we use rounding to calculate the sd so the "accept.sd.value" will not be exact
  # -------------------------------

  n.samples <- n.trials + n.burnin

  ## Ideally we want a point that is between the lower and upper bound
  ## First create something that is outside the bound so we can get the while loop going
  ## so we have an initial value
  sd.timeseries <- sigma.ar2 * 2 # add more than acceptable bound

  # While the sd.value is between
  while (!dplyr::between(sd.timeseries, sigma.ar2 - sigma.ar2.bound, sigma.ar2 + sigma.ar2.bound)){
    ## Get white noise
    E <- stats::rnorm(n = n.samples + 2, mean = 0, sd = sigma.innov)

    ## Zeros for now
    Y <- matrix(0, n.samples)

    ## Update rule according to Yule Walker Equations
    for (i in 3:n.samples){

      ## Current sample = white noise + lag1 + lag2
      Y[i] <- E[i+2] + a1*Y[i-1] + a2*Y[i-2]

    }

    # Take burn-in of samples so it has time to stablise
    Y_burnin <- Y[(n.burnin+1):length(Y)]
    dataset <- tibble::tibble(ar2_samples = Y_burnin + gen.mean)
    # Update the sd.timeseries for the while loop
    sd.timeseries <- sd(dataset$ar2_samples)
  }

  # Update before return after while loop
  dataset$ar2_samples_sigma <- sd(dataset$ar2_samples)
  dataset$trial_no <- 1:n.trials

  return(dataset)
}



