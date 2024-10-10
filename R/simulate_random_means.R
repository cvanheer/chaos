#' simulate_random_means.R
#  Author: Christina Van Heer, christinavanheer@gmail.com
#' Description: this creates a set of means for each block of trails before a change point.
#' The number of means required depends on how long the trial sequences are before a change point
#' @param change_points vector int - the number of change points you want of each
#' @param lower_gen_mean int - smallest mean that you want the gaussian to be
#' @param upper_gen_mean int - largest mean that you want the gaussian to be
#' @return gen_means vect int - a vector of generative means the length of the experiment
#' @export
#' @importFrom stats runif
#' @examples \dontrun{ rand_means(n_change_points = 20, lower_gen_mean = 100, upper_gen_mean = 250) }
rand_means <- function(change_points, lower_gen_mean, upper_gen_mean){

  # Get the number of change points
  n_change_points <- length(change_points)

  # Randomly draw means from a uniform distribution
  means <- round(stats::runif(n = n_change_points, min = lower_gen_mean, max = upper_gen_mean), 0)

  # Shuffle the means a bit to make sure they are ranodmised
  shuffled_means <- sample(means, size = n_change_points, replace = FALSE)

  # Replicate means for every trial
  gen_means <- rep(x = shuffled_means, times = change_points)

  return(gen_means)
}
