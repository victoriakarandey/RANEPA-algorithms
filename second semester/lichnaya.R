# search for an eulerian cycle

#check if the graph is connected
library(igraph)

mat <- matrix(c(0, 1, 1, 0, 0, 0,
                1, 0, 1, 1, 0, 0, 
                1, 1, 0, 1, 1, 0, 
                0, 1, 1, 0, 1, 1, 
                0, 0, 1, 1, 0, 1,
                0, 0, 0, 1, 1, 0), nrow = 6, ncol = 6, byrow = TRUE)

graph <- graph_from_adjacency_matrix(mat)
                 
#check if graph is connected with the help of
BFS <- function(graph, start = 1){
  
  queue <- c(start)
  visited <- c()
  
  while (length(queue) != 0){
    queue <- queue[-1]
    
    for (i in 1:vcount(graph)){
      #checking all neighbors are visited
      for (j in neighbors(graph, i, mode = 'out')){ 
        if (is.element(j, visited) == FALSE){ 
          queue <- c(queue, j)
          visited <- c(visited, j)
        }
      }
    }
  }
  return(c(visited))
}

isConnected <- function(graph){
  if (length(BFS(graph)) == vcount(graph)){ #все вершины пройдены BFS
    status <- TRUE
   }else {
    status <- FALSE
   }
  return(status)
}

isEulerian <- function(graph){
  his <- TRUE
  vertices <- V(graph)
  if (is.directed(graph)){
    for (i in vcount(graph)){
      if (degree(graph, v = vertices[i], mode = 'in') !=
          degree(graph, v = vertices[i], mode = 'out')){
        his <- FALSE
      } 
    }
  }else{
    for (i in vcount(graph)){
      if ((degree(graph, v = vertices[i]) %% 2) == 1){
        his <- FALSE
      }
    }
  }
  return(his)
}

isGraphNice <- function(graph){
  existence <- FALSE
  if (isConnected(graph) & isEulerian(graph) == TRUE){ 
    existence <- TRUE }
  return(existence)
}

isGraphNice(graph)