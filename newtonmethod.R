#record time 
ptm <- proc.time()
proc.time() - ptm

#Newton Method
# reading data
x<-read.table("/Users/shaoyuming/Desktop/ex4Data/ex4x.dat",header=F)
y<-read.table("/Users/shaoyuming/Desktop/ex4Data/ex4y.dat",header=F)

# sigmoid
g = function (z) {
  return (1 / (1 + exp(-z) ))
} # plot(g(c(1,2,3,4,5,6)))

# hypothesis 
h = function (x,th) {
  return( g(x %*% th) )
} # h(x,th)

# cost
J = function (x,y,th,m) {
  return( 1/m * sum(-y * log(h(x,th)) - (1 - y) * log(1 - h(x,th))) )
} # J(x,y,th,m)

# derivative of J (gradient)
grad = function (x,y,th,m) {
  return( 1/m * t(x) %*% (h(x,th) - y))
} # grad(x,y,th,m)

# Hessian

H = function (x,y,th,m) {
  return (1/m * t(x) %*% x * diag(h(x,th)) * diag(1 - h(x,th)))
} 

# H(x,y,th,m) Make it go (iterate until convergence):

# setup variables
j = array(0,c(10,1))
m = length(x$V1)
x = matrix(c(rep(1,m), x$V1, x$V2), ncol=3)
y = matrix(y$V1, ncol=1)
th = matrix(0,3)

# iterate 
# note that the newton's method converges fast, 10x is enough
for (i in 1:10) {
  j[i] = J(x,y,th,m) # stores each iteration Cost
  th = th - solve(H(x,y,th,m)) %*% grad(x,y,th,m) 
}

#Have a look at the cost function by iteration:
plot(j, xlab="iterations", ylab="cost J")


print(th)
