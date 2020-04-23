#' @title Known Alive Vector
#'
#' @description Fill in vector with 1's at points where individual was known to be alive, and NA's elsewhere
#'
#' @author Kevin See and Mike Ackerman
#'
#' @param xi vector of 0's and 1's from a detection history
#'
#' @return a vector containing 1's and NA's

known_alive_vec = function(x_i) {
  if(sum(x_i, na.rm = T) == 0) {
    z = rep(NA, length(x_i))
    return(z)
  }
  f_alive = min(which(x_i == 1))
  l_alive = max(which(x_i == 1))
  z = rep(NA, length(x_i))
  z[f_alive:l_alive] = 1
  return(z)
}
