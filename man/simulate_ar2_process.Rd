% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_ar2_process.R
\name{simulate_ar2_process}
\alias{simulate_ar2_process}
\title{simulate_ar2_process.R}
\usage{
simulate_ar2_process(
  n.trials,
  a1,
  a2,
  gen.mean,
  sigma.ar2,
  sigma.ar2.bound,
  sigma.innov,
  n.burnin
)
}
\arguments{
\item{n.trials}{numerical integer number of trials you want in total in your AR2 process}

\item{a1}{numerical Yule walker parameter alpha1 - how much you weight the previous lag1 data point}

\item{a2}{numerical Yule walker parameter alpha2 - how much you weight the lag2 data point}

\item{gen.mean}{numerical integer Generative mean of the underlying process which is added to the AR2 series. You can use 0 if you do
not want anything added, but otherwise it is a time series as long as n.trials}

\item{sigma.ar2}{underlying standard deviation of the AR2 process}

\item{sigma.ar2.bound}{numerical acceptable tolerance for SD generated e.g within 0.01 plus or minus of gen_sd}

\item{sigma.innov}{innovation/error standard deviation in the AR2 process}

\item{n.burnin}{numerical the number of samples you want to throw out before sampling the AR2 process}
}
\value{
dataset - tibble with all trial info in it
}
\description{
simulate_ar2_process.R
}
\examples{
simulate_ar2_process(n.trials = 280, a1 = 0.5, a2 = 0.3, gen.mean= 0, sigma.ar2 = 15, sigma.ar2.bound = 0.01, sigma.innov = 10, n.burnin = 1000)
}
