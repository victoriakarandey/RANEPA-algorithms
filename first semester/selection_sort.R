
selection_sort <- function(massiv) {
  n <- length(massiv)
  for (i in 1:n){
    min <- moymin(massiv[i:n])[1]
    pos <- moymin(massiv[i:n])[2]+i-1
    karman <- massiv[i]
    massiv[i] <- min
    massiv[pos] <- karman
  }
  return(massiv)
} 
  
