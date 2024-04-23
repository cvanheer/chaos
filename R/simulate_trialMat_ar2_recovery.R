#' simulate_trialMat_ar2_recovery.R
#' @param n.types numerical integer the number of types of AR2 sequences you will have
#' @param n.trials numerical integer number of trials you want in total in your AR2 process
#' @param a1 numerical Yule walker parameter alpha1 - how much you weight the previous lag1 data point
#' @param a2 numerical Yule walker parameter alpha2 - how much you weight the lag2 data point
#' @param gen.mean numerical integer Generative mean of the underlying process which is added to the AR2 series
#' @param sigma.ar2 underlying standard deviation of the generative distribution
#' @param sigma.ar2.bound numerical acceptable tolerance for SD generated e.g within 0.01 plus or minus of gen_sd
#' @param n.burnin numerical the number of samples you want to throw out before sampling the AR2 process
#' @param n.iter numerical, number of times you want to simulate the AR2 process with a particular parameter combo before it gives up
#' @return trialMat = trialMatrix of simulated experiment data as nested datasets
#' @export
#' @importFrom tidyselect everything
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom dplyr select mutate arrange
#' @importFrom purrr pmap
#' @importFrom chaos simulate_ar2_process
#' @importFrom chaos chaos_palette
#' @examples simulate_trialMat_ar2_recovery(n.types = 4, n.trials = 280, a1 = 0.6, a2 = -0.4, gen.mean = 0, sigma.ar2 = 15, sigma.ar2.bound = 0.001, n.burnin = 1000, n.iter = 50000)
simulate_trialMat_ar2_recovery <- function(n.types, n.trials, a1, a2, gen.mean, sigma.ar2, sigma.ar2.bound, n.burnin, n.iter){
  # ---------------------------------------------
  # Description: this is the script I used to create my trials for the AR2 PhD params, with some small
  # modifications so that I can simulate trials for the purposes of Kalman Filter model recovery.
  # ---------------------------------------------

  # Form into parameter list
  params <- list(n_types = n.types,
                 n_trials = n.trials,
                 a1 = a1,
                 a2 = a2,
                 gen_mean = gen.mean,
                 sigma_ar2 = sigma.ar2, # ts = time series
                 sigma_ar2_bound = sigma.ar2.bound,
                 n_burnin = n.burnin,
                 n_iter = n.iter)

  # We use expand grid here to create combinations of the parameters inside the expand_grid(tibble())
  trialMat <- tidyr::expand_grid(
    tibble::tibble(
      a1 = params$a1,
      a2 = params$a2,
      ar2_type = params$n_types,
     )) |>
    dplyr::mutate(
      n_iter = params$n_iter,
      gen_mean = params$gen_mean,
      sigma_ar2_bound = params$sigma_ar2_bound,
      n_trials =  params$n_trials,
      sigma_ar2 = params$sigma_ar2,
      n_burnin = params$n_burnin # we do not generally vary burnin but I have placed it here just in case someone wants to
    )

  ## Get what the SD of the innovations should be for every iteration of the
  # simulations - we use a1 and a2 and the overall time series sd (gen_sd) to calculate
  # this using a transformed equation from Gottman
  trialMat <- trialMat |>
    dplyr::mutate(
      # Calculate what the innovation sd needs to be in order to have a standard deviation
      # of 15 for the generative process - controlled by gen_sd parameter
      sigma_innov = chaos::calc.ts.innov(a1, a2, sigma_ar2)) |>
    dplyr::select(n_trials, ar2_type, a1, a2, gen_mean,
                  sigma_ar2, sigma_ar2_bound, sigma_innov, n_iter,
                  tidyselect::everything())

 # Simulate AR2 process using each row as input parameters
  trialMat <- trialMat|>
    dplyr:: mutate(
      # Run the function which constrains the sd of the entire resulting time series even more
      dataset = purrr::pmap(list(n_trials, a1, a2, gen_mean, sigma_ar2, sigma_ar2_bound, sigma_innov, n_burnin, n.iter),
                  simulate_ar2_process, .progress = TRUE),
    )

  return(trialMat)
}
