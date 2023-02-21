
bubble_sort <- function(massiv) {
  counter <- 0
  n <- length(massiv)
  if (n >= 2) {
    for (j in 1:n) {
      for (i in 1:(n-1)) {
        if (massiv[i] > massiv[i+1]) {
          karman <- massiv[i]
          massiv[i] <- massiv[i+1]
          massiv[i+1] <- karman
          counter <- counter + 1
        }
      }
    }
  }
  print(counter)
  return(c(massiv))
}
  
x <- c(223, 222, -4774, 999, 8)
bubble_sort(x)