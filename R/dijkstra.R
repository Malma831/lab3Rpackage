#Dijkstra


wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))


dijkstra <- function(data, init_node){

  if(!is.data.frame(data)) stop("The graph is not a data.frame")
  if(!is.numeric(init_node)) stop("init_node is not a numeric.")

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


dijkstra(wiki_graph,1)
