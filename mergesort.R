
merge <- function(mass1, mass2) {
  massiv <- c()
  
  if (mass1[length(mass1)] < mass2[1]) {
    massiv <- c(mass1, mass2)
    return(massiv)
  }
  else if (mass1[1] > mass2[length(mass2)]) {
    massiv <- c(mass2, mass1)
    return(massiv)
  }
  
  for (i in 1:(length(mass1) + length(mass2))) {
    if (length(mass1) == 0 || length(mass2) == 0) {
      massiv <- c(massiv, mass1, mass2)
      return(massiv)
    } else if (mass1[1] < mass2[1]) {
      massiv <- c(massiv, mass1[1])
      mass1 <- mass1[-1]
    } else {
      massiv <- c(massiv, mass2[1])
      mass2 <- mass2[-1]
    }
  }
}


merge_sort <- function(mass) {
  if (length(mass) <= 1) {
    return(mass)
  } else if (length(mass) == 2) {
    leftone <- mass[1]
    rightone <- mass[2]
    smallmass <- merge(leftone, rightone)
    return(smallmass)
  } else {
    middle <- (length(mass) - 1) %/% 2 + 1
    left <- mass[1:middle]
    right <- mass[(middle+1):length(mass)]
    left <- merge_sort(left)
    right <- merge_sort(right)
    return(merge(left, right))
  }
}

merge_sort(c(1, 3, 5, 2))

