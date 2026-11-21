#'Geometric Distribution Mean
#'
#'Computes the expected number of trials
#'before the first success.
#'
#'formula:
#'
#'E(X) = 1/p
#'
#'@param p Probability of success.
#'
#'@return A numeric mean value.
#'
#'@examples
#'geom_mean(0.2)
#'
#'@export

geom_mean <- function(p){

  1/p

}

#'Geometric Distribution Variance
#'
#'Computes the variance of a geometric distribution.
#'
#'Formula:
#'
#'var(X) = (1-p)/p^2
#'
#'@param p Probability of success
#'
#'@return A numeric variance value.
#'
#'@examples
#'geom_variance(0.2)
#'
#'@export

geom_variance <- function(p){

  (1-p)/(p^2)

}

#'Geometric Distribution Standard Deviation
#'
#'Computes the standard deviation of a
#'geometric distribution.
#'
#'Formula :
#'
#'SD(X) = sqrt((1-p)/p^2)
#'
#'@param p Probability of success
#'
#'@return A numeric standard deviation value.
#'
#'@examples
#'geom_sd(0.2)
#'
#'@export


geom_sd <- function(p){

  sqrt((1-p)/(p^2))
}
