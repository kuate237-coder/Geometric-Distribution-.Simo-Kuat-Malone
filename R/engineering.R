#'Success Probability Before trial k
#'
#'Computes the probability of obtaining at least one success before or trial k.
#'
#'Formula:
#'
#'p(X<=k) = 1-(1-p)^k
#'
#'@param k Maximum trial number.
#'@param p Probability of success.
#'
#'@return A numeric probability value.
#'
#'@examples
#'success_before_k(5,0.2)
#'
#'@export


success_before_k <- function(k,p){

  1-(1-p)^k

}

#'Expected Number of Retransmissions
#'
#'Computes the average number of retransmission
#'required before the fiirst succees.
#'
#'Formula:
#'
#'E(X) - 1
#'
#'@param p Probability of success.
#'
#'@return Expected retransmissions.
#'
#'@examples
#'expected_retransmissions(0.2)
#'
#'@export

expected_retransmissions <- function(p){

  (1/p)-1

}

#'Network Reliability
#'
#'Evaluates network reliability over
#'a maximum of k trials.
#'
#'@param k Maximum trial number .
#'@param p Probability of sucess.
#'
#'@return Reliability Value.
#'
#'@examples
#'network_reliability(5,0.2)
#'
#'@export

network_reliability <- function(k,p){

  success_before_k(k,p)

}
