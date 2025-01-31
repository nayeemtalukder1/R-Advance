NRM <- function(x){
  fx <- (x^2 - 2*x -2)
  fxa <- (2*x - 2)
  xf <- x-(fx/fxa)
  it <- 1
  while (abs(fx)>0.0001&it<20) {
    x <- xf
    fx <- (x^2 - 2*x -2)
    fxa <- (2*x - 2)
    xf <- x-(fx/fxa)
    it <- it + 1
  }
  list(a=xf,iteration=it)
}
NRM(3)
