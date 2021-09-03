#' Title
#'
#' @param x1
#' @param x2
#'
#' @return
#' @export
#'
#' @examples
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


  while(x2 != 0){ #x2 är b och x1 är a från pseudokod.
    t <- x2
    x2 <-  x1 %% x2
    x1 <- t


  }

  return(x1)
}


