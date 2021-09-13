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

  if(!is.numeric(x1)){
    stop("x1 is not numeric")
  } else if(x1 %% 1 != 0) {
    stop("x1 is not an integer")
  }

  if(!is.numeric(x2)){
    stop("x2 is not numeric")
  } else if(x2 %% 1 != 0){
    stop("x2 is not an integer")
  }

  while(x2 != 0){
    t <- x2
    x2 <-  x1 %% x2
    x1 <- t

  }

  return(x1)
}


