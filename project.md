Practical Machine Learning Project
----
## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Data
The training data for this project are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]

The test data are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har]. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## What you should submit
The goal of your project is to predict the manner in which they did the exercise. This is the ¡°classe¡± variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-). You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details.

## Reproducibility
Due to security concerns with the exchange of R code, your code will not be run during the evaluation by your classmates. Please be sure that if they download the repo, they will be able to view the compiled HTML version of your analysis.

##1. Loading data and package

```r
library(caret);library(randomForest);library(data.table)
training <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",data.table = F)
testing <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",data.table = F)
```

## 2. Cleaning data
Actually, when we get the data, we find that there are too many variable in the data set and some of them are not very good for modeling. So we need to clean the data for model selecting.  First, we remove those variables that have more than 95% NA. Then we remove some irrelevant random variable with the name of user_name and timestamp and so on. 

```r
dim(training);dim(testing)
```

```
## [1] 19622   160
```

```
## [1]  20 160
```

```r
index <- sapply(1:ncol(training), function(i) sum(is.na(training[,i]))/nrow(training) < 0.95)
trainset <- training[,index]
rm_index <- grep("^V|timestamp|user_name|window", names(trainset))
trainset <- trainset[,-rm_index]
```

## 3. split the training data for cross validation 

```r
set.seed(123)
inTrainset <- createDataPartition(y = trainset$classe, p =0.75, list = FALSE)
train_trainset <- trainset[inTrainset,]
valid_trainset <- trainset[-inTrainset,]
```

## 4. preprocess with PCA and build the model with randomForest

```r
prepro <- preProcess(train_trainset[,-53], method = "pca")
trainPC <- predict(prepro,train_trainset[,-53])
trainPC <- data.frame(trainPC, train_trainset$classe)
modFit <- randomForest(train_trainset.classe~., data = trainPC)
```

## 5. test the accuracy with the validation data
From the result we can see that the accuracy is 0.9755302 which is pretty high, so the model is fairly good for the prediction of the testing data. Also we can calculate the out of sample error is 0.024 which is also good.

```r
validPC <- predict(prepro,valid_trainset[,-53])
confusionMatrix(factor(valid_trainset$classe), predict(modFit,validPC))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1388    1    1    5    0
##          B    9  923   14    1    2
##          C    2   13  831    7    2
##          D    0    0   48  755    1
##          E    2    4    3    5  887
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9755          
##                  95% CI : (0.9708, 0.9797)
##     No Information Rate : 0.2857          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.969           
##  Mcnemar's Test P-Value : 4.308e-07       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9907   0.9809   0.9264   0.9767   0.9944
## Specificity            0.9980   0.9934   0.9940   0.9881   0.9965
## Pos Pred Value         0.9950   0.9726   0.9719   0.9391   0.9845
## Neg Pred Value         0.9963   0.9954   0.9837   0.9956   0.9988
## Prevalence             0.2857   0.1919   0.1829   0.1576   0.1819
## Detection Rate         0.2830   0.1882   0.1695   0.1540   0.1809
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9944   0.9872   0.9602   0.9824   0.9955
```

```r
accuracy <- confusionMatrix(factor(valid_trainset$classe), predict(modFit,validPC))$overall[1]
Ose <- 1 - confusionMatrix(factor(valid_trainset$classe), predict(modFit,validPC))$overall[1]
accuracy
```

```
##  Accuracy 
## 0.9755302
```

```r
Ose
```

```
##   Accuracy 
## 0.02446982
```

## 6. Predict the result of the testing data

```r
testPC <- predict(prepro, testing)
predict(modFit,testPC)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  C  A  A  E  D  B  A  A  A  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
