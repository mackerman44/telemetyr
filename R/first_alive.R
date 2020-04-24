#' @title First Alive Vector
#'
#' @description For each individual, return an integer marking the first occassion that individual is known to be alive.
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param y detection history matrix of 0's and 1's with one row per individual and one column per detection site.
#'
#' @export
#' @return a vector containing integers

first_alive = function(y) {
  f = apply(y, 1, function(x) min(which(x == 1)))
  return(f)
}
