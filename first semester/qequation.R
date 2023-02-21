
uravn <- function(aa, bb, cc) {
  if (aa != 0){
    D <- bb**2 - 4*aa*cc
    if (D>0) {
      x1 = (-bb + sqrt(D))/(2*aa)
      x2 = (-bb - sqrt(D))/(2*aa)
      return(c(x1, x2))
    } else {
      print('net resheniy')
      return(NA, NA)
    }
  } else {
    x1 = cc/bb
    return(x1) 
  }
}
