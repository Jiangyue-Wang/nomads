location_variance <- function(x, y){
  Vx <- var(x)
  Vy <- var(y)
  return(Vx+Vy)
}
