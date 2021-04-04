
rm(list=ls())

# simulating an AR(1)
# T=100

set.seed(0)
data <- arima.sim(list(order=c(1,0,0), ar=.5), n=101)
plot(data)

yt <- data[-101]
yT_true <- data[101]

# Prediction with OLS estimation (manually)
yt_target <- yt[2:100]
yt_regres <- yt[1:99]
m <- lm(yt_target ~ yt_regres -1)

yT <- yt[100]
y_hat <- predict(m, newdata=list(yt_regres=yT))
y_hat

sum(coefficients(lm(yt_target ~ yt_regres -1))*yT)

#______________________________________________________________
##### CP

y_grid <- seq(-3,3,by=0.1)
  
# b: dimension of the block
b <- 5
# K: number of blocks, K=T/b
K <- 100/b

# I use NOB, so I have |PI|=K transformations
# definisco una funzione pi che trasforma l'ordine
pi_j_NOB <- function(j,t)
{
  if(1<=t && t<=100-(j-1)*b)
  {
    return(t + (j-1)*b)
  }
  if(100-(j-1)*b+1<=t && t<=100)
  {
    return(t + (j-1)*b - 100)
  }
}

# così non va perchè controlla la condizione nella parentesi solo per il primo
# pi_j_NOB(2,1:100)

# così funziona invece,
# non so è esattamente la stessa cosa di CHernozukovv, ma penso non cambi molto comunque
sapply(1:100,FUN=pi_j_NOB,j=2)

#View(cbind(yt,yt[sapply(1:100,FUN=pi_j_NOB,j=2)]))

# pvalues for the grid
pv_grid <- numeric(length(y_grid))

cnt <- 1

# for each candidate y
for(y in y_grid)
{
  # augmented sample
  y_aug <- c(yt, y)
  
  # train a model
  yt_target <- y_aug[2:101]
  yt_regres <- y_aug[1:100]
  m <- lm(yt_target ~ yt_regres -1)
  
  # save residual
  score_y <- abs(tail(m$residuals, n=1))
  
  scores_pi <- numeric(K)
    
  # for each transformation pi in PI
  for(j in 1:K)
  {
    # transformated sample
    y_aug_pi <- yt[sapply(1:100,FUN=pi_j_NOB,j=j)]
    
    # train a model
    yt_target <- y_aug_pi[2:101]
    yt_regres <- y_aug_pi[1:100]
    m <- lm(yt_target ~ yt_regres -1)
    
    scores_pi[j] <- abs(tail(m$residuals, n=1))
  }
  
  pv_grid[cnt] <- 1/K * (sum(scores_pi >= score_y))
  cnt <- cnt+1

}

alpha <- 0.05

# Plot the p-values
plot(y_grid, pv_grid, type='l', ylim=c(0,1))
abline(h=c(0,1))
abline(h=alpha, col='red', lty=2)
points(yT_true, numeric(length(yT_true)), pch=12)


# Compute the Prediction Interval
PI.grid <- y_grid[which(pv_grid >= alpha)]
PI <- c(min(PI.grid), max(PI.grid))
PI
abline(v = PI, col='red')
#points(PI.grid, numeric(length(PI.grid)), pch=16, col='red')

# plot as a time series


