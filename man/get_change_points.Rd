% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_change_points.R
\name{get_change_points}
\alias{get_change_points}
\title{simulate_change_points.R
Author: Christina Van Heer, christinavanheer@gmail.com
Description: Create a sequence of trials for each one with change points}
\usage{
get_change_points(n.trials, min.trials, max.trials, exp.rate)
}
\arguments{
\item{n.trials}{the total number of trials in the experiment}

\item{min.trials}{minimum number of trials before a change point}

\item{max.trials}{maximum number of trials before a change point occurs}

\item{exp.rate}{average number of trials before a change point - controls the hazard function (1/lambda) this exp.rate is the lambda}
}
\value{
trial_counter the number of trials before a change point
}
\description{
simulate_change_points.R
Author: Christina Van Heer, christinavanheer@gmail.com
Description: Create a sequence of trials for each one with change points
}
\examples{
 \dontrun{
get_change_points(n.trials = 280, min.trials = 5, max.trials = 50, exp.rate = 20)
}

}
