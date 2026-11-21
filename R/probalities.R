#'
#'Geometric Probability Function
#'
#'Computes the probabibility that the first success occurs exactly at trial x.
#'
#'The function is based on the geometric distribution:
#'
#'p(X=x) = p(1-p)^(x-1)
#'
#'@param x trial number.
#'@param p probability of sucess.
#'
#'@return Numeric Probability Value
#'
#'@examples
#'geom_probability(3,0.2)
#'
#'@export

geom_probability <- function(x,p){

  p*(1-p)^(x-1)

}

#'Geometric Cumulative Distribution Function
#'
#'Computes the cumulative probability that the
#'first success occurs on or before trial x.
#'
#'p(X<=x)= 1 -(1-p)^x
#'
#'@param x trial number.
#'@param p probability of success
#'
#'@return A numeric cumulative probability value.
#'
#'@examples
#'geom_cumulative(3,0.2)
#'
#'@export

geom_cumulative <- function(x,p){
  1-(1-p)^x
}
