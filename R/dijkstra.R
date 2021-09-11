

#' Dijkstra's algorithm
#'
#'Calculates the shortest distance from a source node to all other nodes in a graph. The function is built for the
#'graph that is presented on the wikipedia page for Dijkstra's algorithm
#'
#'
#' @param data is a data.frame with three columns: v1, v2, and w.
#' @param init_node is the node to start from
#' @references \href{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}{Wikipedia page}
#' @return Returns a vector with the shortest distance from init_node to all the other nodes in the graph
#' @export
#'
#' @examples dijkstra(wiki_graph,1)
#'
#'
#'
#'
dijkstra <- function(data, init_node){

  if(!is.data.frame(data)) stop("The graph is not a data.frame")
  if(!is.numeric(init_node)) stop("init_node is not a numeric.")
  if(!(init_node %in% unique(data$v1))) stop("init_node does not exist in the graph.")
  if(ncol(data) != 3 | !all(names(data) %in% c("v1", "v2", "w"))) stop("Wrong input")



  Q <- unique(data$v1)
  dist <- rep(Inf, length(Q))
  prev <- rep(NA, length(Q))
  dist[init_node] <- 0


 while(length(Q) != 0){

    #Returnerar minsta värdet i dist, blir init_node då den har dist = 0.
     u <- Q[which.min(dist[Q])]
     #Ska sedan ta bort u från Q

     Q <- Q[!(Q == u)]


     for(v in Q[Q %in% data$v2[data$v1 == u]]){

         #v1 är noden man "står på" och v2 är grannar.
         alt <- dist[u] + data$w[data$v1 == u & data$v2 == v]
         if(alt < dist[v]){
           dist[v] <- alt
           prev[v] <- u #behövs denna?

         }

       }

 }
 return(dist)

}



