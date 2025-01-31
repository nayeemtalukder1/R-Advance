solve_quadratic <- function(a, b, c) {
  discriminant <- b^2 - 4 * a * c
  
  if (discriminant < 0) {
    return("No real roots")
  }
  
  root1 <- (-b + sqrt(discriminant)) / (2 * a)
  root2 <- (-b - sqrt(discriminant)) / (2 * a)
  
  return(c(root1, root2))
}

# Example usage
a <- 1
b <- -3
c <- 2

roots <- solve_quadratic(a, b, c)
print(roots)
