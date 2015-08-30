mydata <- data.frame(X = rnorm(200), Y = rnorm(200))

mymodel <- lm(Y ~ X, data = mydata)

summary(mymodel)
