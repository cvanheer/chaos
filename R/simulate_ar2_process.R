# # --- * --- * --- * --- * --- * --- * --- * --- * --- *
# Simulate AR2 process
# --- * --- * --- * --- * --- * --- * --- * --- * --- *
# This is a function that generates AR2 processes in R for my PhD
# where I want to add a generative process to.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' simulate_ar2_process
#'
#' @param n.trials an integar number of trials you want in total in your AR2 process
#' @param std.dev the standard deviation of the innovations in the process (errors)
#' @param a1 Yule walker parameter alpha1 - how much you weight the previous lag1 data point
#' @param a2 Yule walker parameter alpha2 - how much you weight the lag2 data point
#' @param gen.mean Generative mean of the underlying process which is added to the AR2 series. You can use 0 if you do
#' not want anything added, but otherwise it is a time series as long as n.trials
#' @param burnin the number of samples you want to exclude before the process stabilises
#' @import tibble
#' @return dataset - a tibble with the AR2 series
#' @export
#'
#' @examples
simulate_ar2_process <- function(n.trials, std.dev, a1, a2, gen.mean, n.burnin){
  ## -------------------------------------------------------------------
  ## Description: This function generates a second order process given
  ## n.trials = number of samples (min 4)
  ## std.dev = standard deviation
  ## a1 parameter (Yule Walker)
  ## a2 parameter (Yule Walker)
  ## The a1 and a2 parameters tell us how much to weight previous samples
  ## -------------------------------------------------------------------

  ## Generate random noise (add 2 points because starting point)
  ## The standard deviation of this process is close to the sd
  ## of the distribution that was put in. We want to be
  ## able to control the SD of the parent distribution though

  n.samples <- n.trials + n.burnin

  # Get white noise which is Gaussian - this is called the
  # "innovations" of the AR2 process - this term is typically used in
  # forecasting to describe the fact that information in an AR2 series
  # is predictable to within an innovation/error term. Because the errors
  # are derived from a Gaussian process independently on each trial, this is
  # the "unpredictable" part of the sequence.
  E <- rnorm(n = n.samples + 2, mean = 0, sd = std.dev)

  ## Create Y values which are zeros for now
  Y <- matrix(0, n.samples)

  ## Update rule according to Yule Walker Equations
  for (i in 3:n.samples){

    ## Current sample = white noise + lag1 + lag2
    Y[i] <- E[i+2] + a1*Y[i-1] + a2*Y[i-2]

  }

  ## Take burn.in of samples so that the AR process has time to stablise
  dataset <- Y[(n.burnin+1):length(Y)]
  dataset <- tibble(ar2_samples = dataset + gen.mean)
  dataset$sd <- sd(dataset$ar2_samples)
  return(dataset)

}
