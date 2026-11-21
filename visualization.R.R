#' Plot Geometric Probability Mass Function
#'
#' Displays the probability mass function
#' of a geometric distribution.
#'
#' @param p Probability of success.
#' @param max_x Maximum trial number displayed.
#'
#' @return A probability mass function plot.
#'
#' @examples
#' plot_geometric_pmf(0.2)
#'
#' @export

plot_geometric_pmf <- function(p,max_x=15){

  if(p<=0 || p>= 1){

    stop("p must be betwenn 0 and 1")
  }

  x <- 1:max_x

  y <- p*(1-p)^(x-1)

  barplot(
    y,
    names.arg = x,
    col="steelblue",
    main = "Geometric Distribution probability mass function",
    xlab = "trial Number",
    ylab = "probability"

  )
}


#' Plot Geometric Cumulative Distribution Function
#'
#' Displays the cumulative distribution
#' function of a geometric distribution.
#'
#' @param p Probability of success.
#' @param max_x Maximum trial number displayed.
#'
#' @return A cumulative probability plot.
#'
#' @examples
#' plot_geometric_cdf(0.2)
#'
#' @export

plot_geometric_cdf <-function(p,max_x=15){


  if(p<=0 || p>= 1){

    stop("p must be betwenn 0 and 1")
  }

  x <- 1:max_x

  y <- 1-(1-p)^x

  plot(

    x,
    y,
    type = "b",
    pch = 19,
    col = "red",
    main ="Geometric distribution cdf",
    xlab ="Trial number",
    ylab ="cumulative probability"

  )

}

#' Plot Monte Carlo Simulation Results
#'
#' Displays a histogram of simulated
#' geometric transmission trials.
#'
#' @param p Probability of success.
#' @param n_sim Number of simulations The default value is
#' 10000 but can be modified by the user.
#'.
#'
#' @return A histogram.
#'
#' @examples
#' plot_simulation(0.2)
#'
#' @export

plot_simulation <- function(p,n_sim=10000){


  if(p<=0 || p>= 1){

    stop("p must be betwenn 0 and 1")
  }

  data <- rgeom(n_sim,p)+1

  hist(
    data,
    col = "orange",
    main ="Monte Carlo Simulation",
    xlab = "Number of trials",
    ylab = "frequency"

  )

}

