#assign a class
#plot from class

iter <- 1:10
gr <- rep(c(1:5), each = 2)
best <- 10:1
obj <- rnorm(10, mean = 5, sd = 2)

x <- structure(class = "lf_history", list(
  iter = iter,
  gr = gr,
  best = best,
  obj = obj
))

plot.lf_history <- function(x){
  pDat <- data.frame(iter = x$iter,
                     best = x$best,
                     obj = x$obj,
                     gr = x$gr)
  p <- ggplot2::ggplot(data = pDat,
                       ggplot2::aes(x = iter,
                                    color = as.factor(gr))) +
    ggplot2::geom_point(ggplot2::aes(y = obj)) +
    ggplot2::geom_line(ggplot2::aes(y = best),
                       color = "blue") +
    ggplot2::theme(legend.position = "none")
  return(p)
}
plot(x)









##### Karmarkar Input

for g = 5:5:100
A = [1 2];
b = [2];   %column vector
c = [3 2]; %row vecter

Q = g;
[Q, c, y] = karmarkar_transform(A,b,c,Q)
[x, iter] = karmarkar(c,y);

u = size(iter,1);

dot(c, x);
xy = [(Q+1)*iter(:,2) (Q+1)*iter(:,3)];
%% Plot
plot(xy(1:20,1),xy(1:20,2),'o-')
str = {'Q=' Q}
text(xy(1,1)+.25,xy(1,2),str)
%xlim([0 1.6]);
%ylim([0 1.6]);
xlabel('X1')
ylabel('X2')
hold all
end
%% Determine if constraints are binding (0 = binding)
[z,u]=size(y);
bound = zeros(z,2);
for i = 1:z
bound(i,1) = i;
if y(i,:)*x > 10^-5
bound(i,2) = 1;
end
end
bound


%plot(iter(:,9))

###### Karmarkar Transform
A <- matrix(c(1,2), byrow = T, ncol = 2)
b <- 2
c <- matrix(c(3,2), byrow = T, ncol = 2)
#Q <- (1:20)*5

karmarkarTransform <- function(A, b, c){
  At <- t(A)
  m <- dim(A)[1]
  n <- dim(A)[2]
  rowSum <- apply(A, MARGIN = 1, sum)
  colSum <- apply(A, MARGIN = 2, sum)
  l <- 0
  for (element in A) l <- l + log(1 + abs(element))
  L <- ceiling(1 + log(1 + abs(max(c))) + log(1 + m) + l)
  Q <- 2^L
  y <- rbind(cbind(A, -diag(m), matrix(0,m,m+n+1), -b, -rowSum+b+matrix(1,1,m)),
             cbind(matrix(0,n,m+n), At, diag(n), matrix(0,n,1), -t(c), -colSum+t(c)-matrix(1,n,1)),
             cbind(c, matrix(0,1,m), -t(b), matrix(0,1,n+2), sum(b)-sum(c)),
             cbind(matrix(1,1,2*(n+m)+1), -Q, Q-2*(m+n)-1))
  c <- cbind(matrix(0,1,2*(m+n)+2), 1)
  return(list(Q = Q, c = c, y = y))
}
k <- karmarkarTransform(A, b, c)


######## Karmarkar 
karmarkar <- function(c, y){
  m <- dim(y)[1]
  n <- dim(y)[2]
  r <- 1 / sqrt(n * (n - 1))
  x <- matrix(1 / n, n, 1)
  yn <- x
  a <- (n - 1) / (3 * n)
  k <- 0
  l <- 0
  for (element in y) l <- l + log(1 + abs(element))
  L <- ceiling(1 + log(1 + abs(max(c))) + log(1 + m) + l)
  iter <- cbind(0, t(x))
  while (c %*% x > 2^(-L) & c %*% x > 10^-7){ # %*% is the inner (dot) product
    k <- k + 1
    D <- diag(as.vector(x))
    P <- rbind(cbind(y %*% D),
               matrix(1,1,n))
    cBar <- c %*% D
    cp <- (diag(n) - t(P) %*% solve(P %*% t(P),P)) %*% t(cBar)
    yk <- yn - a * r * (cp / norm(cp))
    x <- (D %*% yk) / as.numeric(matrix(1, 1, n) %*% D %*% yk)
    iter <- rbind(iter, cbind(k, t(x)))
    print(c %*% x)
  }
  
  return(list(x = x, iter = iter))
}

karmarkar(k$c, k$y)





####################### AFFINE
A <- matrix(c(1, -1, 1, 0,
              1, 2, 0 ,1),
            byrow = T, ncol = 4)
b <- matrix(c(1, 2), 
            byrow = T, ncol = 1)
c <- matrix(c(-1, -2, 0, 0),
            byrow = T, ncol = 4)
x0 <- matrix(c(0.5, 0.5, 1, 0.5),
             byrow = T, ncol = 1)

Affine <- function(A, b, c, x){
  e <- 10^-10
  cp <- x
  m <- dim(A)[1]
  n <- dim(A)[2]
  k <- 0
  z <- c %*% x
  iter <- cbind(k, t(x), z)
  a <- 0.99
  while (norm(cp, "I") > e){
    k <- k + 1
    D <- diag(as.vector(x))
    cb <- c %*% D # row vector
    cp <- (diag(n) - t(A %*% D) %*% MASS::ginv(A %*% D %*% D %*% t(A)) %*% A %*% D) %*% t(cb) # col vector
    d <- -D %*% cp # col vector
    l <- min((x[d<0] / (-d[d<0]))) # scalar
    x <- x + a * l * d # col vector
    z <- c %*% x # scalar
    iter <- rbind(iter, cbind(k, t(x), z))
  }
  iter <- as.data.frame(iter)
  colnames(iter) <- c("Iteration", paste0("X",1:n), "Objective")
  iter <- tidyr::gather(iter, key, val, -Iteration)
  p <- ggplot2::ggplot(data = iter,
                       ggplot2::aes(x = Iteration,
                                    y = val,
                                    color = key)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()
  print(p)
  return(list(iter = iter, p = p, solution = as.vector(t(x))))
}

x <- Affine(A, b, c, x0)$solution

#xa1 xa2 xb1 xb2 xc1 xc2
A <- matrix(c(1,1,0,0,0,0,
              0,0,1,1,0,0,
              0,0,0,0,1,1,
              1,0,1,0,1,0,
              0,1,0,1,0,1),
            byrow = T, ncol = 6)
b <- matrix(c(12,12,12,8,8),
            byrow = T, ncol = 1)
c <- matrix(c(1,2,3,4,5,6),
            byrow = T, ncol = 6)
x0 <- matrix(c(5,4,3,4,1,1),
            byrow = T, ncol = 1)
Affine(A, b, c, x0)


p = 1,
m = 1, 
s = 10^-3, 
r = 1, 
a = 0, 

courseList <- c("Berlin52", "eil51", "gr48", "pma343")
dat <- LeapFrog::GetData(course = courseList[2])
lf <- LeapFrog::LF(dat$distanceMatrix, p = 1, m = 5, s = .1, r = 50, a = 0, monitor = T)
paste0(round(100 * (lf$distance-dat$optimalValue)/dat$optimalValue, 2), "%")
print(lf$iterPlot)
LeapFrog::PlotTour(dat$coordinateMatrix, lf$solution)







