
#### MULTIVARIATE ####-----------------------------------------------------------------------------------

setwd("D:/Poli/Corsi/NPS/Block IV - Nonparametric forecasting/Conformal Prediction UPDATED")

# x: matrix data
x <- read.table('parziali.txt')

# plot
plot(x, asp=1)

# initialization
n <- dim(x)[1]
alpha <- 0.1


# grid
#x1.new.grid <- seq(min(x[,1]) - 0.25*diff(range(x[,1])), max(x[,1]) + 0.25*diff(range(x[,1])), length = 30)
#x2.new.grid <- seq(min(x[,2]) - 0.25*diff(range(x[,2])), max(x[,2]) + 0.25*diff(range(x[,2])), length = 30)
x1.new.grid <- seq(min(x.calib[,1]) - 0.5*diff(range(x.calib[,1])), max(x.calib[,1]) + 0.5*diff(range(x.calib[,1])), length = 50)
x2.new.grid <- seq(min(x.calib[,2]) - 0.5*diff(range(x.calib[,2])), max(x.calib[,2]) + 0.5*diff(range(x.calib[,2])), length = 50)
p.value <- matrix(nrow = length(x1.new.grid), ncol = length(x2.new.grid))

# NC computes the non-conformative score between between z.aug and z.aug[i]
NC <- function(z.aug, i){
  
  # Euclidian (L2) distance (squared) between z.aug[i,] and the mean of z.aug[-1,]
  #sum( (z.aug[i,] - colMeans(z.aug[-i,]))^2 )
  
  # L1 (manhattan) distance between z.aug[i,] and the mean of z.aug[-1,]
  #sum( abs(z.aug[i,] - colMeans(z.aug[-i,])) )
  
  # L-infinity (Chebyshev) distance
  max( abs(z.aug[i,] - colMeans(z.aug[-i,])) )
  
  # Mahlanobis Distance
  #mahalanobis(z.aug[i,], center=colMeans(z.aug[-i,]), cov = cov(z.aug[-i,])) 
  
  # kNN
  #distances2 <- ( as.matrix(dist(z.aug))[i,-i] )^2
  #mean(sort(distances2)[1:K]) # average linkage
  #min(sort(distances2)[1:K])  # single linkage
  #max(sort(distances2)[1:K])  # complete linkage
  
  
  # Mahalanobis Distance (a mano)
  #as.numeric( as.matrix(z.aug[i,] - colMeans(z.aug[-i,])) %*% 
  #              solve(cov(z.aug[-i,])) %*% 
  #              as.matrix(t(z.aug[i,] - colMeans(z.aug[-i,]))) ) 
  
  # Regression
  #abs( z.aug[i,2] - sum(coefficients(lm(z.aug[-i,2]  ~ z.aug[-i,1]))*c(1, z.aug[i,1]))) # Regression
  
}

# cycle over the grid
for(k in 1:length(x1.new.grid)) {
  for(h in 1:length(x2.new.grid)) {
    # augmented sample
    x.aug <- rbind(x, c(x1.new.grid[k],x2.new.grid[h]))
    # empty vector of scores
    scores <- numeric(dim(x.aug)[1])
    # cycle over the augmented sample
    for (i in 1:dim(x.aug)[1]) {
      # score of the i-th sample in augmented sample
      scores[i] <- NC(x.aug, i)
    }
    # p-value of the (k,h)-th new observation in the grid
    p.value[k,h] <- sum(scores >= scores[dim(x.aug)[1]])/(dim(x.aug)[1])
    print(c(k,h))
  }
}

# Plot the p-values and the prediction region
image(x1.new.grid, x2.new.grid, p.value, zlim=c(0,1), asp=1)
points(X, pch=16)
contour(x1.new.grid, x2.new.grid, p.value, levels = alpha, add=T)


# usando la dorma chiusa:

# point prediction:
g <- colMeans(x)
g

# nc-scores
# li calcolo in un modo non bellissimo, come se avessi split conformal, ma solo col training,
# traininando tranne che con l'i-esimo dato
n <- dim(x)[1]
scorez <- c()
for(i in 1:n){
    score <- NC(x, i)
    scorez <- c(scorez, score)
}
scorez

alpha <- 0.1
ceiling((1-alpha)*(n+1))
k <- sort(scorez)[ceiling((1-alpha)*(n+1))]

PI.1 <- c(g[1]-k, g[1]+k)
PI.2 <- c(g[2]-k, g[2]+k)
PI.1
PI.2

####___________________________________________________________________________________________
#### SPLIT ####

# x: matrix data
x <- read.table('parziali.txt')

# plot
plot(x, asp=1)

# initialization
n <- dim(x)[1]
alpha <- 0.1

# Split Conformal
training.prop <- 0.75
m <- ceiling(n*training.prop)
set.seed(100)
training.id   <- sample(1:n, size=m, replace = F) 
x.train  <- x[training.id,]   # Proper Training Set
x.calib  <- x[-training.id,]  # Calibration set

# grid
#x1.new.grid <- seq(min(x.calib[,1]) - 0.25*diff(range(x.calib[,1])), max(x.calib[,1]) + 0.25*diff(range(x.calib[,1])), length = 30)
#x2.new.grid <- seq(min(x.calib[,2]) - 0.25*diff(range(x.calib[,2])), max(x.calib[,2]) + 0.25*diff(range(x.calib[,2])), length = 30)
x1.new.grid <- seq(min(x.calib[,1]) - 0.5*diff(range(x.calib[,1])), max(x.calib[,1]) + 0.5*diff(range(x.calib[,1])), length = 100)
x2.new.grid <- seq(min(x.calib[,2]) - 0.5*diff(range(x.calib[,2])), max(x.calib[,2]) + 0.5*diff(range(x.calib[,2])), length = 100)

p.value <- matrix(nrow = length(x1.new.grid), ncol = length(x2.new.grid))

#prediction rule
prediction.train <- colMeans(x.train)

NC <- function(z.aug, i){
  
  # Euclidian distance between z.aug[i,] and prediction.train
  #sum((z.aug[i,] - prediction.train)^2) # euclidian distance
  
  # L1 (manhattan) distance between z.aug[i,] and prediction.train
  #sum( abs(z.aug[i,] - prediction.train) )
  
  # L-infinity (Chebyshev) distance
  max( abs(z.aug[i,] - prediction.train) )
  
  # Mahlanobis Distance
  #mahalanobis(z.aug[i,], center=prediction.train, cov = cov(x.train)) 
  
}

for(k in 1:length(x1.new.grid)) {
  for(h in 1:length(x2.new.grid)) {
    x.calib.aug <- rbind(x.calib, c(x1.new.grid[k],x2.new.grid[h]))
    scores <- numeric(dim(x.calib.aug)[1])
    for (i in 1:dim(x.calib.aug)[1]) {
      scores[i] <- NC(x.calib.aug, i)
    }
    p.value[k,h] <- sum(scores >= scores[dim(x.calib.aug)[1]])/(dim(x.calib.aug)[1])
    print(c(k,h))
  }
}

# Plot the p-values and the prediction region
#image(x1.new.grid, x2.new.grid, p.value, zlim=c(0,1), asp=1)
#points(x.calib, pch=16)
#contour(x1.new.grid, x2.new.grid, p.value, levels = alpha, add=T)
#points(prediction.train[1], prediction.train[2], pch=3, cex=4)


### CLOSED FORM #########

# point prediction:
g.train <- colMeans(x.train)
g.train

# nc-scores
n2 <- dim(x.calib)[1]
# loop over the calibration set
scorez.calib <- c()
for(i in 1:n2){
  score <- max( abs(x.calib[i,] - g.train) )
  scorez.calib <- c(scorez.calib, score)
}
scorez.calib

alpha <- 0.1
ceiling((1-alpha)*(n2+1))
k <- sort(scorez.calib)[ceiling((1-alpha)*(n2+1))]
k

PI.1 <- c(g.train[1]-k, g.train[1]+k)
PI.2 <- c(g.train[2]-k, g.train[2]+k)
PI.1
PI.2


# Plot the p-values and the prediction region
image(x1.new.grid, x2.new.grid, p.value, zlim=c(0,1), asp=1)
points(x.calib, pch=16)
contour(x1.new.grid, x2.new.grid, p.value, levels = alpha, add=T)
points(prediction.train[1], prediction.train[2], pch=3, cex=4)

abline(v=PI.1)
abline(h=PI.2)





