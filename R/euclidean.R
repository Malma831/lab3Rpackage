#' Title
#'
#' @param x1 first number
#' @param x2 second number
#' @references \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{wikipedia page}
#' @return returns greater common divisor of two value
#' @export
#'
#' @examples euclidean(100, 1000)

euclidean <- function(x1, x2){
  if(!is.numeric(x1) | !is.numeric(x2) | length(x1)!=1 | length(x2)!=1)
    {stop("Not scalar or numerical")}

  while(x2 != 0){
    t <- x2
    x2 <-  x1 %% x2
    x1 <- t


  }

  return(x1)
}


