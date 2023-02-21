fib <- function(n){
  if (n == 1){
    return(0)
  } else if (n == 2) {
    return(1)
  } else {
    return(fib(n-1) + fib(n-2))
  }
}
fib(5000)

# fib(4) = fib(3) + fib(2)

fibonacci <- function(n){
  if (n == 1){
    return(0)
  } else {
    numbers <- c(0, 1)
    for (i in 1:(n-2)){
      numbers <- c(numbers, (numbers[i] + numbers[i+1]))
      chislo <- numbers[length(numbers)]
    }
      print(tail(numbers))
     return(chislo)
  }
}

