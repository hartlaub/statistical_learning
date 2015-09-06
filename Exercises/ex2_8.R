#  Get KNN() working and construct df comparing k = 1, 3, 5, 7, 15 and linear fit classifications

classify <- function(x, df, k) {
  # k-nearest neighbors binary classification of
  # the input vector x according to training data
  # in df. 
  #
  # Args:
  #   x: a numeric vector of inputs
  #   df: a data frame of training data with a
  #       binary classification in the first column
  #       and input data in the remaining columns
  #   k: the number of neighbors averaged over
  #
  # Returns:
  #   1 if knn average > 0.5
  #   0 if knn average <= 0.5
  
  df$new <- apply(df[, 2:ncol(df)], 1, distance, b = x)
  df <- df[order(df$new), ]
  df <- df[1:k, ]
  
  z <- mean(df[, 1])
  
  return(as.numeric(z > 0.5))
  
}

distance <- function(a, b) {
  # Computes the euclidean distance
  # between numeric vectors a and b
  # of equal length
  #
  # Args:
  #   a: numeric vector
  #   b: numeric vector
  #
  # Returns:
  #   ||a - b||
  
  return(sqrt(sum((a - b)^2)))
  
}

KNN <- function(tr, te, k) {
  # Binary classification of a test
  # data set by k-nearest-neighbors averaging
  #
  # Args:
  #   tr: a data frame of training data with a 
  #       binary classifier in the first column
  #       and numeric input data in the rest.
  #   te: a data frame of numeric test data.
  #       Assumes ncol(te) = ncol(tr) - 1
  #    k: the number of neighbors averaged over
  #
  # Returns:
  #   te with a column of binary classifiers
  #   added to it
  
  te$class <- apply(te, 1, classify, df = tr, k = k)
  
  return(te)
  
}

setwd("~/Desktop/statistical_learning/Exercises/Data")

training <- read.table("zip.train.csv", sep = " ")
training$V258 <- NULL
training <- training[training$V1 == 2 | training$V1 == 3, ]
training$V1 <- training$V1 == 3
training$V1 <- as.numeric(training$V1)

test <- read.table("zip.test.csv", sep = " ")
test <- test[test$V1 == 2 | test$V1 == 3, ]

class1 <- KNN(training, test[, 2:ncol(test)], 15)
x <- knn(training[, 2:ncol(training)], test[, 2:ncol(test)], training$V1, 15)
class1$class == x

linclass <- lm(V1 ~ ., data = training)
summary(linclass)

fits <- cbind(rep(1, nrow(test)), as.matrix(test[, 2:ncol(test)])) %*% linclass$coefficients
fits <- fits > 0.5
