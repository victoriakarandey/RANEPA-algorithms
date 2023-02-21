
quick_sort <- function(mass) {
  if (length(mass) < 2) {
    return(mass)
  } else {
    opor <- mass[1]
    rest <- mass[-1]
    left <- quick_sort(rest[rest<opor])
    right <- quick_sort(rest[rest>=opor])
    otvet <- c(left, opor, right)
    return(otvet)
  }
}



