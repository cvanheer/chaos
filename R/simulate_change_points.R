# Functions to create trial sequences
#' simulate_change_points.R
#' Author: Christina Van Heer, christinavanheer@gmail.com
#' Description: Create a sequence of trials for each one with change points
#' @param n.trials the total number of trials in the experiment
#' @param min.trials minimum number of trials before a change point
#' @param max.trials maximum number of trials before a change point occurs
#' @param exp.rate average number of trials before a change point - controls the hazard function (1/lambda) this exp.rate is the lambda
#' @importFrom stats rexp
#' @return change_point_locations the trial number of the change point
#' @export
#'
#' @examples  \dontrun{
#' get_change_points(n.trials = 280, min.trials = 5, max.trials = 50, exp.rate = 20)
#' }
#'
get_change_points <- function(n.trials, min.trials, max.trials, exp.rate){

 # ----- Unpack function input --------
 n_trials <- n.trials
 min_trials <- min.trials
 max_trials <- max.trials
 exp_rate <- exp.rate

 # ----- Generate change points ---------
  trial_counter <- 0
  while(sum(trial_counter) <= n_trials){
    # Draw from random number from exponential distribution for
    # waiting time before next change point
    draw <- 0
    while(draw == 0 | draw >= max_trials){
      draw <- min_trials +
        round(stats::rexp(1, 1/exp_rate), 0)
    }
    trial_counter <- c(trial_counter, draw)
  }

 # ----- Clean up change points so we have right amount ---
  # Get rid of leading zero from first iteration of while loop
  trial_counter <- trial_counter[2:length(trial_counter)]

  # Calculate number of trials you have to get rid to keep it at n_trials number
  trials_to_truncate <- sum(trial_counter) - n_trials
  # Chop this number (trials_to_truncate) off on the last set of trials
  trial_counter[length(trial_counter)] <- trial_counter[length(trial_counter)] - trials_to_truncate

  # Check that there are no zeros at the end - note that ifelse doesnt deal with
  # vectorised stuff well so you need to assign inside the ifelse clause
  ifelse(trial_counter[length(trial_counter)] == 0,
         # If the last number is 0, get rid of it (only count up to the second last set of trials)
         trial_counter <- trial_counter[1:length(trial_counter) - 1],
         # Otherwise keep trial_counter as is
         trial_counter <- trial_counter[1:length(trial_counter)])

  # Get trials which are change points
  change_point_locations <- cumsum(trial_counter)
  change_point_locations <- change_point_locations[1:length(change_point_locations)-1]

  # The actual location is NEXT trial
  change_point_locations <- change_point_locations  + 1

  return(change_point_locations)
}
