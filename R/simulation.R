#' Simulate Geometric Transmissions
#'
#' Simulates the number of trials required
#' before the first success using the
#' geometric distribution.
#'
#' The simulation is based on random draws
#' from a geometric distribution.
#'
#' @param p Probability of success.
#' @param n Number of simulations.
#'
#' @return A numeric vector containing
#' simulated trial counts.
#'
#' @examples
#' simulate_transmissions(0.2, 100)
#'
#' @export
simulate_transmissions <- function(p,n){

  rgeom(n,p)+1

}

#' Monte Carlo Reliability Estimation
#'
#' Estimates reliability using Monte Carlo
#' simulation of a geometric distribution.
#'
#' The function performs repeated simulations
#' and approximates the probability of success.
#'
#' @param p Probability of success.
#' @param n_sim Number of simulations.
#'
#' @return Estimated reliability value.
#'
#' @examples
#' monte_carlo_success(0.2, 10000)
#'
#' @export


monte_carlo_success <- function(p,n_sim =10000){

  simulations <- rgeom(n_sim,p)+1

  theorical_mean <- 1/p

  simulated_mean <- mean(simulations)

  error <- abs(theorical_mean-simulated_mean)

  list(

    theorical_mean = theorical_mean,

    simulated_mean = simulated_mean,

    error = error
  )

}



