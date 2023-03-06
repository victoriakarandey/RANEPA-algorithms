
zel <- function(x,y){
  return((1-x)^2 + 100*(y-x^2)^2)
}
dvig <- function(x0,y0,h){
  xh <- c(0,-h,+h )[which.min(c(zel (x0, y0), 
                                zel (x0-h, y0),zel (x0+h, y0)))]
  yh <- c(0,-h,+h )[which.min(c(zel (x0 + xh, y0), 
                                zel (x0+xh, y0-h), zel (x0+xh, y0+h)))]
  return(c(xh, yh))
}

pattern_search <- function(x0, y0, h, E, nmax) {
  counter <- 0
  vekh <- dvig(x0, y0, h)
  counter <- counter + 6
  
  while (h > E & counter <= nmax){
    while ((vekh[1]^2 + vekh[2]^2) == 0 & h > E & counter <= nmax){
      h <- h/2
      vekh <- dvig(x0, y0, h)
      counter <- counter + 6
    }
    if ((vekh[1]^2 + vekh[2]^2) != 0 & h > E & counter <= nmax){
      x0 <- x0 + vekh[1]
      y0 <- y0 + vekh[2]
      while (zel(x0, y0) > zel(x0 + vekh[1], y0 + vekh[2]) & counter <= nmax) {
        x0 <- x0 + vekh[1]
        y0 <- y0 + vekh[2]
        counter <- counter + 2
      }
      vekh <- dvig(x0, y0, h)
      counter <- counter + 6
    }
  }
  return(c(x0, y0, counter))
}

pattern_search(800, 200, 6, 1, 100)
 