
library(igraph)

edg <- c(1,2,
         2,3,
         2,4,
         3,9,
         4,5,
         5,6,
         5,7,
         5,8)
g <- graph(edg, n = max(edg), directed = TRUE)

BFS <- function(tr, start = 1){
  V(tr)$color <- 'red'
  V(tr)$color[start] <- 'green'
  
  queue <- c(start)
  visited <- c()
  
  while (length(queue) != 0){
    queue <- queue[-1]
    
    for (i in 1:vcount(tr)){
      #checking all neighbors are visited
      for (j in neighbors(tr, i, mode = 'out')){ 
        if (is.element(j, visited) == FALSE){ 
          queue <- c(queue, j)
          visited <- c(visited, j)
          
          V(tr)[j]$color <- 'green'
        }
      }
    }
  }
 
  plot(tr)
  return(c(visited, queue))
}

  
BFS(g)
  
  
  DFS <- function(graph, start = 1) {
    visited <- list()
    visited <- append(visited, start)

    DFS_recursion(start, visited, graph)
    
    
    return(visited)
  }
  
  DFS_recursion <- function(vertex, visited, graph) {
    # получаем всех соседей вершины и проходим по ним рекурсивно
    nbs <- neighbors(graph, vertex, mode = 'out')
    visited_print <- c()
    print(nbs)
      for(i in nbs) {
        if((i %in% visited) == FALSE) {
          visited <- unlist(append(visited, i))
          print(visited)
          visited_print <- c(visited_print, i)
          DFS_recursion(i, visited, graph)
        }
      }
    return(visited_print)
  }
  
  DFS(g)