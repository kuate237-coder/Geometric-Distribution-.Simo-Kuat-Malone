#'Required Number of Trials
#'
#'Computes the minimum number of trials
#'required to reach a target reliability.
#'
#'The function determines the smallest value
#'of k such that :
#'
#'p(X<=k) >= target_reliability
#'
#'@param p Probability of success.
#'@param target_reliability Desired reliability level.
#'
#'@return Minimum required number of trials.
#'
#'@examples
#'required_trials(0.2,0.95)
#'
#'@export

required_trials <- function(p,target_reliability){

  if(p<=0 || p>=1){
    stop("p must be between 0 and 1")

  }

  if(target_reliability <=0 || target_reliability >=1){

    stop("target_reliability must be between 0 and 1")

  }
  k <- ceiling(
    log(1-target_reliability) /log(1-p)
  )
  return(k)

}


#'Packet Loss Probability
#'
#'Computes the probability that all
#'transmissiom attempts fail after k trials.
#'
#'Formula:
#'
#'p(X >k) = (1-p)^k
#'
#'@param p probability of success.
#'@param k Maximum number of trials .
#'
#'@return Packet loss probability.
#'
#'@examples
#'packet_loss_probability(0.2,5)
#'
#'@export


packet_loss_probability <- function(p,k){


  if(p<=0 || p>=1){
    stop("p must be between 0 and 1")
  }
  (1-p)^k

}

#'Recommended Number of Retransmissions
#'
#'Computes the recommended number of
#'retransmissions required to achieve
#'a target reliability level.
#'
#'The number of retransmissions is equal
#'to he required number of trials minus one.
#'
#'@param p Probability of success.
#'@param target_reliability Desires reliability level.
#'
#'@return Recommend number of retransmissions.
#'
#'@examples
#'recommend_retries(0.2,0.95)
#'
#'@export

recommend_retries <- function(p,target_reliability){

  required_trials(p,target_reliability)-1
}

