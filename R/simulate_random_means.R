#' simulate_random_means.R
#  Author: Christina Van Heer, christinavanheer@gmail.com
#' Description: this creates a set of means for each block of trails before a change point.
#' The number of means required depends on how long the trial sequences are before a change point
#' @param n_change_points int - the number of change point events you want in your experiment
#' @param lower_gen_mean int - smallest mean that you want the gaussian to be
#' @param upper_gen_mean int - largest mean that you want the gaussian to be
#' @return shuffled_means vect - a vector of integars that contain the generative means in your experiment
#' @export
#' @importFrom stats runif
#' @examples \dontrun{ rand_means(n_change_points = 20, lower_gen_mean = 100, upper_gen_mean = 250) }
rand_means <- function(n_change_points, lower_gen_mean, upper_gen_mean){

  # Randomly draw means from a uniform distribution
  means <- round(stats::runif(n = n_change_points, min = lower_gen_mean, max = upper_gen_mean), 0)

  # Shuffle the means a bit to make sure theyre randomsied
  shuffled_means <- sample(means, size = n_change_points, replace = FALSE)

  return(shuffled_means)
}
