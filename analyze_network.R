#' Comprehensive Network Reliability Analysis
#'
#' Performs a complete reliability analysis
#' for a retransmission system modeled using
#' the geometric distribution.
#'
#' The function combines probability,
#' statistical, reliability, simulation,
#' and decision-support measures.
#'
#' @param p Probability of success.
#' @param k Maximum number of trials.
#' @param x Trial number of interest.
#' @param target_reliability Desired
#' reliability level. The default value
#' is 0.95 but can be modified.
#'
#' @param n_sim Number of Monte Carlo
#' simulations. The default value is
#' 10000 but can be modified.
#'
#' @param n_sim Number of Monte Carlo simulations.
#'
#' @return A list containing probabilities,
#' reliability measures, retransmission
#' recommendations, and simulation results.
#'
#' @examples
#' analyze_network(
#'   p = 0.2,
#'   k = 5,
#'   x = 3,
#'   target_reliability = 0.95
#' )
#'
#' @export

analyze_network <- function(p,
                            k,
                            x,
                            n_sim=10000,
                            target_reliability = 0.95){

  results <- list(

    #probabilities

    probability_at_x = geom_probability(x,p),

    cumulative_probability = geom_cumulative(x,p),

    #statistics

    mean_trials = geom_mean(p),

    variance = geom_variance(p),

    standard_deviation = geom_sd(p),

    #reliability

    success_before_k  = success_before_k (k,p),

    packet_loss_probability = packet_loss_probability(p,k),

    reliability = network_reliability(k,p),

    #retransmission

    expected_retransmissions = expected_retransmissions(p),

    recommended_retransmissions = recommend_retries(p,target_reliability ),

    #planning

    required_trials=required_trials(p,target_reliability),

    #simulation

    monte_carlo = monte_carlo_success(p, n_sim)


  )
  return(results)
}
