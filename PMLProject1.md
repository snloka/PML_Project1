Practical Machine Learning Course Project Writeup
========================================================

Backgrouond
===========

  This is an R Markdown document using knitr to generate the HTML report. 
  
  Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: (http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset). 
  
  
Data
====
  The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
  
  The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
  
  The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 

Required Libraries
==================
## Please make sure you have these libraries installed.

```r
install.packages("ggplot")
install.packages("lattice")
install.packages("RCurl")
install.packages("dplyr")
install.packages("plyr")
install.packages("caret")
install.packages("gbm")
```
## load the libraries

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
library(lattice)
library(RCurl)
```

```
## Loading required package: bitops
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(grid)
library(plyr)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.2.1
```

```r
library(gbm)
```

```
## Warning: package 'gbm' was built under R version 3.2.1
```

```
## Loading required package: survival
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: splines
## Loading required package: parallel
## Loaded gbm 2.1.1
```


## download the data file from the website.

```r
trainfileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(trainfileUrl, destfile = "./pml-training.csv", method="libcurl")
testfileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(testfileUrl, destfile = "./pml-testing.csv", method="libcurl")
```

## Loading and preprocessing the data
load the data file into a data.frame

```r
colList <- c(3,4,6:11,37:49,60:68,84:86,102,113:124,140,151:160)
colList <- c(5, 8:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160)
colList <- c(5:13,15:16,18:160)
colList <- c(5, 8:11, 37:49, 60:68, 84:86, 113:124, 151:160)
colList <- c(5, 8:10, 37:48, 60:68, 84:86, 113:124, 151:160)
colList <- c(8:10, 37:48, 60:68, 84:86, 113:124, 151:160)
exColList <- c(1,2,3,4,14,17,26,89,92, 101,127,130,139)
set.seed(1234)
traindatafilename <- "pml-training.csv"
trainrawdata <- read.csv(traindatafilename, sep=",", header=TRUE,skipNul=TRUE, na.strings=c("NA"))
trainIndex1 <- createDataPartition(y=trainrawdata[,160], p=0.80, list=FALSE)
traindata1 <- trainrawdata[trainIndex1,colList]
traindata2 <- trainrawdata[-trainIndex1,colList]
#traindata1 <- trainrawdata[,colList]


testdatafilename <- "pml-testing.csv"
testrawdata <- read.csv(testdatafilename, sep=",", header=TRUE)
testdata1 <- testrawdata[,colList]
#ggplot(traindata1, aes(x=roll_forearm, y=pitch_forearm)) + geom_point(color="steelblue", size=4, alpha=1/2) + facet_grid( . ~ classe)
#ggplot(traindata1, aes(x=roll_forearm, y=yaw_forearm)) + geom_point(color="steelblue", size=4, alpha=1/2) + facet_grid( . ~ classe)
```


```r
str(traindata1)
summary(traindata1)
#head(testdata1)
```


```r
str(testdata1)
summary(testdata1)
head(testdata1)
```

## Train for the model with the train data set.



```r
fit1
```

```
## Stochastic Gradient Boosting 
## 
## 15699 samples
##    48 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Adaptively Cross-Validated (10 fold, repeated 5 times) 
## 
## Summary of sample sizes: 14129, 14128, 14130, 14132, 14129, 14127, ... 
## 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa      Accuracy SD
##   1                   50      0.5512941  0.4215730  0.01367104 
##   1                  100      0.6170550  0.5112362  0.01331014 
##   1                  150      0.6456016  0.5490581  0.01624890 
##   2                   50      0.6607659  0.5679753  0.01729908 
##   2                  100      0.7266489  0.6533531  0.01838485 
##   2                  150      0.7662813  0.7040207  0.01685281 
##   3                   50      0.7121212  0.6349903  0.01410474 
##   3                  100      0.7790263  0.7201325  0.01459230 
##   3                  150      0.7704065  0.7088387  0.04463839 
##   Kappa SD    .B 
##   0.01759215    5
##   0.01770277    5
##   0.02118739    5
##   0.02199835    5
##   0.02378107    5
##   0.02151259    5
##   0.01782971    5
##   0.01853475    5
##   0.05696962  140
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were n.trees = 150,
##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.
```

```r
print(fit1$finalModel)
```

```
## A gradient boosted model with multinomial loss function.
## 150 iterations were performed.
## There were 23 predictors of which 23 had non-zero influence.
```

```r
#plot(fit1$finalModel, uniform=TRUE, main="Classification Tree")
#text(fit1$finalModel, use.n=TRUE, all=TRUE, cex=0.8)
```

## Now predict with the test data set.
First predict the train data to check for the accuracy

```r
prediction2 <- predict(fit1, newdata=traindata2)
summary(prediction2)
```

```
##    A    B    C    D    E 
## 1188  729  740  627  639
```

```r
str(prediction2)
```

```
##  Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Check how good is the prediction of the train data

```r
confusionMatrix(prediction2, traindata2$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1004  113   22   22   27
##          B   23  565   74   11   56
##          C   32   56  537   77   38
##          D   44    3   30  517   33
##          E   13   22   21   16  567
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8132          
##                  95% CI : (0.8006, 0.8252)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7631          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8996   0.7444   0.7851   0.8040   0.7864
## Specificity            0.9344   0.9482   0.9373   0.9665   0.9775
## Pos Pred Value         0.8451   0.7750   0.7257   0.8246   0.8873
## Neg Pred Value         0.9590   0.9393   0.9538   0.9618   0.9531
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2559   0.1440   0.1369   0.1318   0.1445
## Detection Prevalence   0.3028   0.1858   0.1886   0.1598   0.1629
## Balanced Accuracy      0.9170   0.8463   0.8612   0.8853   0.8820
```

```r
table(prediction2)
```

```
## prediction2
##    A    B    C    D    E 
## 1188  729  740  627  639
```

Finally, predict the test data.

```r
prediction1 <- predict(fit1, newdata=testdata1)
summary(prediction1)
```

```
##  A  B  C  D  E 
## 10  5  1  1  3
```

```r
table(prediction1)
```

```
## prediction1
##  A  B  C  D  E 
## 10  5  1  1  3
```

```r
prediction1
```

```
##  [1] A A A A A E D B A A A C B A E E A B B B
## Levels: A B C D E
```


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(prediction1)
```







