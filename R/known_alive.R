#' @title Known Alive Matrix
#'
#' @description Fill in detection history matrix with 1's at points where individual was known to be alive, and NA's elsewhere
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param y detection history matrix of 0's and 1's with one row per individual and one column per detection site.
#'
#' @export
#' @return a vector containing 1's and NA's

known_alive = function(y) {
  z = matrix(NA,
             nrow = nrow(y),
             ncol = ncol(y))
  for(i in 1:nrow(y)) {
    z[i,] = known_alive_vec(y[i,])
  }
  return(z)
}
