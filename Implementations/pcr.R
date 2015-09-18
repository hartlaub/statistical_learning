#  Principal Component Regression

#  Homemade implementation ---------------------------------------

PCR <- function(Y, X, M) {
  
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

X <- data.frame(matrix(c(rnorm(1000, 4, 5), 
                         rnorm(1000),
                         rnorm(1000, 4, 500),
                         runif(1000, -100, 4),
                         rnorm(1000, 1000, 10)),
                         nrow = 1000, 
                         ncol = 5))

pcr.fit <- pcr(X1 ~ ., data = X, scale = TRUE, validation = "CV")
summary(pcr.fit)
  
  
  
  
  
  
  
  