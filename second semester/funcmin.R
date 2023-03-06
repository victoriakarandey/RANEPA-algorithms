

cel <- function(x){
  return(sin(x))
}


funcmin <- function( a, b, E){
  xl <- a
  xr <- b
  pt <- c()
  while((xr - xl)>= E){
    if (cel(xr) > cel(xl)) {
      pt <- c(pt, xr)
      xr <- (xr - xl)/1.618 + xl
      
    } else if (cel(xr) < cel(xl)) {
      pt <- c(pt, xl)
      xl <- xr - (xr - xl)/1.618
    }
  }
  if (cel(xl) < cel(xr)) {
    pt <- c(pt, xl)
  } else if (cel(xl) > cel(xr)){
    pt <- c(pt, xr)
  }
  xmin <- xl + (xr - xl)/2
  ymin <- cel(xmin)
  
  x <- -101:101
  y <- cel(x)
  
  plot(x,y, type = 'l')
  
  points(pt[1:5], cel(pt[1:5]), col = 'mediumpurple1', pch = 16)
  points(pt[length(pt):length(pt)-4], 
         cel(pt[length(pt):length(pt)-4]),col = 'deeppink3', pch = 8)
  
  return(c(xmin, ymin))
}

funcmin(-10, 10, 0.1)



