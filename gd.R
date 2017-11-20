gd <- function(x, y, lrate=0.1, theta0=0, theta1=0, tol=10^-6) {
  jtheta = rep(0, 100)
  err = vector(mode = "numeric")
  diff = sum((jtheta - y) ** 2) / length(x)
  plot(y ~ x); abline(theta0, theta1)
  j = 1
  
  while(diff > tol) {
    for(i in 1:length(x)){
      jtheta[i] = theta0 + theta1 * x[i]
    }
    
    err = c(err, sum((jtheta - y) ** 2) / length(x))
    diff = ifelse(length(err) > 1, abs(err[j] - err[j-1]), err[1])
    theta0 = theta0 - lrate * sum((jtheta - y)) / length(x)
    theta1 = theta1 - lrate * sum(x * (jtheta - y)) / length(x)
    abline(theta0, theta1)
    j = j + 1
  }
  #plot(x = 1:(j-1), y = err, type = "l")
  return(c(theta0, theta1))
}

gd(x, y)
lm(y~x)
x = rnorm(100)
y = rnorm(100) + 2 * x
plot(x, y)

theta0 = 0
theta1 = 0
lrate = 0.1
tol = 10^-3

plot(a[,1], a[,2], type = "line")
