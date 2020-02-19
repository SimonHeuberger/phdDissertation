

X <- scale(matrix(rnorm(4800), ncol = 3))

YX <- cbind(scale(Y), X)

chol1 <- solve(chol(var(YX)))

new.YX <- YX %*% chol1

zapsmall(cor(new.YX))
all.equal(YX[,1], new.YX[,1])

cor.mat <- matrix(c(
  1  , 0.4, 0.5, 0.6,
  0.4,   1,   0,   0,
  0.5,   0,   1,   0,
  0.6,   0,   0,   1),
  ncol = 4)

chol2 <- chol(cor.mat)
final.YX <- new.YX %*% chol2 * sd(Y) + mean (Y)
colnames(final.YX) <- c("Y", "X1", "X2", "X3")

mean(Y)
colMeans(final.YX)

sd(Y)
apply(final.YX, 2, sd)

zapsmall(cor(final.YX))
all.equal(Y, final.YX[,1])





anes <- readRDS(here("data", "anes_cleaned.rds"))
anes <- anes[,1:6] # without education (i.e. Y)
anes.num <- data.frame(cbind(anes$age, as.numeric(anes$gender), as.numeric(anes$race), as.numeric(anes$income),  # numeric versions of variables
                             as.numeric(anes$occupation), as.numeric(anes$pid)))
colnames(anes.num) <- colnames(anes)

mu <- list() # for covariate means
std.dev <- list() # for covariate standard deviations
for(i in 1:ncol(anes.num)){
  mu[i] <- mean(anes.num[,i])
  std.dev[i] <- sd(anes.num[,i])
}
mu <- unlist(mu)
std.dev <- unlist(std.dev)

cor.mat <- cor(anes.num) # correlation matrix 
cov.mat <- std.dev %*% t(std.dev) * cor.mat # covariance matrix

set.seed(1)
library(MASS)
X <- mvrnorm(n = obs, mu = mu, Sigma = cov.mat, empirical = TRUE)

# Combine Y and X into dataframe
mc.df <- data.frame(cbind(Y, X))
