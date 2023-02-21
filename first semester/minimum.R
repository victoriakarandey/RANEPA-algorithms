#функция, которая выдает минимум и его позицию  в массиве



moymin <- function(massiv) {
  curmin <- massiv[length(massiv)]
  pos <- 1
  for (i in 1:length(massiv)) {
    if (massiv[i] <= curmin) {
      curmin <- massiv[i]
      pos <- i
    }
  }
  return(c(curmin, pos))
}
moymin(c(4,-9,-9,4,3))
