library(caret);library(randomForest)
library(rpart);library(rattle);library(data.table)
training <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",data.table = F)
testing <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",data.table = F)
