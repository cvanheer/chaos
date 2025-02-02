% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_random_means.R
\name{rand_means}
\alias{rand_means}
\title{simulate_random_means.R
Description: this creates a set of means for each block of trails before a change point.
The number of means required depends on how long the trial sequences are before a change point}
\usage{
rand_means(change_points, lower_gen_mean, upper_gen_mean)
}
\arguments{
\item{change_points}{vector int - the number of change points you want of each}

\item{lower_gen_mean}{int - smallest mean that you want the gaussian to be}

\item{upper_gen_mean}{int - largest mean that you want the gaussian to be}
}
\value{
gen_means vect int - a vector of generative means the length of the experiment
}
\description{
simulate_random_means.R
Description: this creates a set of means for each block of trails before a change point.
The number of means required depends on how long the trial sequences are before a change point
}
\examples{
\dontrun{ rand_means(n_change_points = 20, lower_gen_mean = 100, upper_gen_mean = 250) }
}
