setwd("~/Desktop/statistical_learning/Exercises/Data")

training <- read.table("zip.train.csv", sep = " ")
training$V258 <- NULL

test <- read.table("zip.test.csv", sep = " ")
