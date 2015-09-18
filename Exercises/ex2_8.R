### Elements of Statistical Learning: Exercise 2.8
### Implementation of K-nearest neighbors binary classification
### and OLS regression binary classification.

#############################################################
#################### FUNCTIONS ##############################
#############################################################

### Function: dist (K nearest neighbors)
### 
### Inputs:
###    y and x: Two numerical vectors of same length
###
### Output:
###    Euclidean distance between y and x.

dist <- function(y, x){
  return(sqrt(sum((y - x )^2)))
}

### Function: classify (K nearest neighbors)
### 
### Inputs: 
###   te_row: A row of the test data. 
###   tr: Complete training dataset. The 0/1 classifications should be 
###       stored in the first column of tr.
###   k: Number of nearest neighbors that user wishes to use in order to
###      classify te_row.
###   Uses dist() function.
###
### Output:
###   Outputs 0 if majority of k rows of tr that have closest Euclidean distance 
###   to te_row are classified as 0.
###   Outputs 1 if majority of k rows of tr that have closest Euclidean distance 
###   to te_row are classified as 1.

classify <- function(te_row, tr, k){
  
  ### tr$new1 computes Euclidean distance between te_row (a row of the test data) 
  ### and each of the rows of tr (training data).
  tr$new1 <- apply(tr[,2:ncol(tr)], 1, dist, x = te_row[2:length(te_row)])
  
  ### Order tr (training data) from smallest to largest Euclidean distance 
  ### from te_row.
  tr <- tr[order(tr$new1), ]
  
  ### Extract k rows of tr that are closest to te_row.
  tr <- tr[1:k, ]
  
  ### Take mean of 0/1 classification of k closest neighbors in training data.
  z <- mean(tr[,1])
  
  ### If z > 0.5, more than half of k nearest neighbors are classified as 1,
  ### so return 1.
  ### If z <= 0.5, more than half of k nearest neighbors are classified as 0,
  ### so return 0.
  return(as.numeric(z > 0.5))
}

### Function: Knn (K nearest neighbors)
###
### Inputs:
###   tr: Complete training dataset. The 0/1 classifications should be 
###       stored in the first column of tr.
###   te: Complete test dataset. Should not contains classifications of
###       test data.
###   k: Number of nearest neighbors that user wishes to use in order to
###      classify te_row.
###   Uses classify() function.
###
### Outputs:
###   Returns test dataset te with additional column 'classify' that contains 
###   0/1 classification of all rows of te.

Knn <- function(tr, te, k){
  te$classify <- apply(te, 1, classify, tr = tr, k = k)
  return(te)
}

### Function: linearClassification (OLS regression)
###
### Inputs:
###   tr: Training data. First row ('V1') should contain classification of
###       training data.
###   te: Test data. First row should not contain classification of test data.
###
### Output: 
###   Outputs test data with a 'classify' column, which contains the 0/1
###   classification of each test observation.

linearClassification <- function(tr, te){
  
  ### linearModel stores linear regression model created from training data.
  colnames(tr)[1] <- "trainingClass"
  linearModel <- lm(trainingClass ~ .- trainingClass, data = tr)
  
  ### Use linearModel to predict probability that test observations are
  ### truly classified as 1.
  test_predictions <- predict(linearModel, te)
  test_classification <- test_predictions
  
  ### If Pr(test_classification == 1) > 0.5, classify that test observation as 1.
  ### If Pr(test_classification == 1) <= 0.5, classify that test observation as 0.
  test_classification[test_classification > 0.5] <- 1
  test_classification[test_classification <= 0.5] <- 0
  
  ### 'classify' column of te will contain classification of all observations
  ### in test dataset.
  te$classify <- test_classification
  
  return(te)
}

#############################################################
######################## MAIN ###############################
#############################################################

setwd("C:/Users/Robin Dunn/Documents/Robin 7th Semester/Statistical Learning/statistical_learning/Exercises/Data")

### training stores training data. 
training <- read.table("zip.train.csv", sep = " ")
training$V258 <- NULL

### test stores test data.
test <- read.table("zip.test.csv", sep = " ")

### Only use training and test data rows with true classification equal to 
### 2 or 3.
training <- training[training$V1==2 | training$V1==3, ]
test <- test[test$V1==2 | test$V1==3, ]

### In training and test data, if classification is 2, set classification to 0.
### If classification is 3, set classification to 1.
training$V1 <- training$V1 - 2
test$V1 <- test$V1 - 2

###########################
### k nearest neighbors ###
###########################

#############
### k = 1 ### 97.53 % accuracy
#############

### test_new stores test data with 0/1 classification of test data
### in 'classify' column, using my Knn function with k = 1.
test_new <- Knn(training, test, 1)

### Outputs percentage of correctly classified test data.
sum(test_new$classify == test[,1]) / length(test_new$classify)

### x stores vector with 0/1 classification of test data
### using R's knn function with k = 1.
require(class)
x <- knn(training[,2:ncol(training)], test[,2:ncol(test)], training[,1], k=1)
sum(x == test[,1]) / length(x)

### Compares classifcations in x (from R's knn function) to
### classifications in test_new$classify (from my Knn function)
table(x == test_new$classify)

#############
### k = 3 ### 96.98 % accuracy
#############

### test_new stores test data with 0/1 classification of test data
### in 'classify' column, using my Knn function with k = 3.
test_new <- Knn(training, test, 3)

### Outputs percentage of correctly classified test data.
sum(test_new$classify == test[,1]) / length(test_new$classify)

### x stores vector with 0/1 classification of test data
### using R's knn function with k = 3.
require(class)
x <- knn(training[,2:ncol(training)], test[,2:ncol(test)], training[,1], k=3)
sum(x == test[,1]) / length(x)

### Compares classifcations in x (from R's knn function) to
### classifications in test_new$classify (from my Knn function)
table(x == test_new$classify)

#############
### k = 5 ### 96.98 % accuracy
#############

### test_new stores test data with 0/1 classification of test data
### in 'classify' column, using my Knn function with k = 5.
test_new <- Knn(training, test, 5)

### Outputs percentage of correctly classified test data.
sum(test_new$classify == test[,1]) / length(test_new$classify)

### x stores vector with 0/1 classification of test data
### using R's knn function with k = 5.
require(class)
x <- knn(training[,2:ncol(training)], test[,2:ncol(test)], training[,1], k=5)
sum(x == test[,1]) / length(x)

### Compares classifcations in x (from R's knn function) to
### classifications in test_new$classify (from my Knn function)
table(x == test_new$classify)

#############
### k = 7 ### 96.70 % accuracy
#############

### test_new stores test data with 0/1 classification of test data
### in 'classify' column, using my Knn function with k = 7.
test_new <- Knn(training, test, 7)

### Outputs percentage of correctly classified test data.
sum(test_new$classify == test[,1]) / length(test_new$classify)

### x stores vector with 0/1 classification of test data
### using R's knn function with k = 7.
require(class)
x <- knn(training[,2:ncol(training)], test[,2:ncol(test)], training[,1], k=7)
sum(x == test[,1]) / length(x)

### Compares classifcations in x (from R's knn function) to
### classifications in test_new$classify (from my Knn function)
table(x == test_new$classify)

#############
### k = 15 ## 96.15 % accuracy
#############

### test_new stores test data with 0/1 classification of test data
### in 'classify' column, using my Knn function with k = 15.
test_new <- Knn(training, test, 15)

### Outputs percentage of correctly classified test data.
sum(test_new$classify == test[,1]) / length(test_new$classify)

### x stores vector with 0/1 classification of test data
### using R's knn function with k = 15.
require(class)
x <- knn(training[,2:ncol(training)], test[,2:ncol(test)], training[,1], k=15)
sum(x == test[,1]) / length(x)

### Compares classifcations in x (from R's knn function) to
### classifications in test_new$classify (from my Knn function)
table(x == test_new$classify)


###########################
###### OLS regression ##### 95.88 % accuracy
###########################

test_new <- linearClassification(training, test[,2:length(test)])
sum(test_new$classify == test[,1]) / length(test_new$classify)
