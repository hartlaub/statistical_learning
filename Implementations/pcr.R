#  Principal Component Regression

#  Homemade implementation ---------------------------------------

PCR <- function(Y, X, M) {
  
  for (i in 1:ncol(X)) {
    
    X[, i] <- (X[, i] - mean(X[, i])) / sd(X[, i])
    
  }
  
  ybar <- rep(mean(Y), length(Y))
  
  V <- svd(X)$v
  Z <- X %*% V
  
  model <- lm(I(Y - ybar) ~ 0 + Z[, 1:M])
  
  return(model)
  
}

#  pcr {pls} -----------------------------------------------------
  
library(pls)

?pcr()

#  Comparison ----------------------------------------------------

x1 <- rnorm(500, 2, 50)
x2 <- rt(500, 2)
x3 <- runif(500, 6, 700)
x4 <- rnorm(500)
x5 <- rexp(500, 20)

y <- 45 * (x1 ^ 2) + 2 * x2 + 5.5 * x3 + 0.00001 * x4 + 0.95 * log(x5)

X <- data.frame(cbind(y, x1, x2, x3, x4, x5))

pcr.fit <- pcr(y ~ ., data = X, scale = TRUE, ncomp = 3)
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

homemodel <- PCR(y, cbind(x1, x2, x3, x4, x5), 3)
  
  
  
  
  
  
  
  