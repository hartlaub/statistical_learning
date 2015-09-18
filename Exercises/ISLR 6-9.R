### This code satisfies the requirements Problem 2.9 from "Introduction to 
### Statistical Learning with Applications in R."
### Code for ridge regression, lasso, PCR, and PLS based off of code from
### Section 6.5 from ISLR.
### 9/16/15
## TEST

require(ISLR) # contains College dataset
require(glmnet) # for ridge regression and lasso
require(pls) # for PCR and PLS

CollegeCopy <- College

### Coerce Private == "Yes" to Private = 1
### and Private == "No" to Private = 0.

CollegeCopy$Private <- as.numeric(CollegeCopy$Private) - 1

### a) Randomly split the data set into a training set and a test set.

set.seed(100)
random <- sample(nrow(CollegeCopy), nrow(CollegeCopy))

training <- CollegeCopy[random[1:389],]
test <- CollegeCopy[random[390:nrow(CollegeCopy)],]

### b) Fit a linear model using least squares on the training set,
###    and report the test error obtained.

# Fit the linear model.

linearModel <- lm(Apps ~ ., data = training)
summary(linearModel)

# Calculate test error.

testPredictions <- as.numeric(predict(linearModel, test))
testResids <- test$Apps - testPredictions
mean(testResids^2) # MSE = 973540.9

### c) Fit a ridge regression model on the training set, with lambda chosen
###    by cross-validation. Report the test error obtained.

# Create x and y matrices.

x <- model.matrix(Apps ~ ., training)
y <- training$Apps

# Perform ten-fold cross validation. 
# bestlam stores the choice of lambda that minimizes MSE.

set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 0, nfolds = 10)
plot(cv.out) # Cross-validation curve with upper and lower standard dev curves.
             # One line is at log(Lambda) at which MSE is minimized (cv.out$lambda.min)
             # One line is at log(Lambda) corresponding to largest value of lambda
             # within 1 standard error of mininum. (cv.out$lambda1se)

bestlam <- cv.out$lambda.min
bestlam

# Fit ridge regression model and view coefficients.

ridge.mod <- glmnet(x, y, alpha = 0, lambda = bestlam)
predict(ridge.mod, type = "coefficients")

# Calculate test error. 

testx <- model.matrix(Apps ~ ., data = test)
ridge.pred <- predict(ridge.mod, testx)
mean((ridge.pred - test$Apps)^2) # MSE = 860927.1

### d) Fit a lasso model on the training set, with lambda chosen by
###    cross-validation. Report the test error obtained, along with the number
###    of non-zero coefficient estimates.

# Create x and y matrices.

x <- model.matrix(Apps ~ ., training)
y <- training$Apps

# Perform ten-fold cross validation. 
# bestlam stores the choice of lambda that minimizes MSE.

set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(cv.out) # Cross-validation curve with upper and lower standard dev curves.
             # One line is at log(Lambda) at which MSE is minimized (cv.out$lambda.min)
             # One line is at log(Lambda) corresponding to largest value of lambda
             # within 1 standard error of mininum. (cv.out$lambda1se)

bestlam <- cv.out$lambda.min
bestlam

# Fit lasso model and view coefficients.

lasso.mod <- glmnet(x, y, alpha = 1, lambda = bestlam)
predict(lasso.mod, type = "coefficients") # All 18 coefs are non-zero

# Calculate test error. 

testx <- model.matrix(Apps ~ ., data = test)
lasso.pred <- predict(lasso.mod, testx)
mean((lasso.pred - test$Apps)^2) # MSE = 1368719

### e) Fit a PCR model on the training set, with M chosen by cross-validation.
###    Report the test error obtained, along with the value of M selected by
###    cross-validation.

# Fit PCR model and look for cross-validated value of M that minimizes
# root MSE (from summary) or MSE (from plot).
# This sugguests M = 17.

set.seed(1)
pcr.fit <- pcr(Apps ~ ., data = training, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

# Calculate test error.

pcr.pred <- predict(pcr.fit, test, ncomp = 17)
mean((pcr.pred - test$Apps)^2) # MSE = 1359059

### f) Fit a PLS model on the training set, with M chosen by cross-validation.
###    Report the test error obtained, along with the value of M selected by
###    cross-validation.

# Fit PLS model and look for cross-validated value of M that minimizes
# root MSE (from summary) or MSE (from plot).
# This sugguests M = 8.

set.seed(1)
pls.fit <- plsr(Apps ~ ., data = training, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# Calculate test error.

pls.pred <- predict(pls.fit, test, ncomp = 8)
mean((pls.pred - test$Apps)^2) # MSE = 1372594
