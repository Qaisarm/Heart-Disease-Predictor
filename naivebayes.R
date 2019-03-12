##################################################################
###########################/Naive Bayes Classifer Code /##########
##################################################################

# Uncomment this life if you run first time
#install.packages(c('tree','ISLR','randomForest', 'e1071','naivebayes'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(naivebayes)

cleveland <- read.csv("processed.cleveland.csv",header=T)
hungarian <- read.csv("processed.hungarian.csv",header=T)
va <- read.csv("processed.va.csv",header=T)
switzerland<- read.csv("processed.switzerland.csv",header=T)
#data <- bind_rows(cleveland, hungarian, switzerland, va)
data<-rbind(cleveland,hungarian,va,switzerland)
data$slope<-NULL
data$ca<-NULL
data$thal<-NULL

# convert class 1-4 to 1
data$class[data$class>"1"]<-"1"
#find ? elements
idx<-data =="?"
#replace elements with NA
is.na(data) <- idx

#Change values to correct variables
data$age <- as.character(data$age)
data$age <- as.integer(data$age)
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$class <- as.factor(data$class)

data$trestbps <- as.character(data$trestbps)
data$trestbps <- as.integer(data$trestbps)
data$chol <- as.character(data$chol)
data$chol <- as.integer(data$chol)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$thalach <- as.character(data$thalach)
data$thalach <- as.integer(data$thalach)
data$exang <- as.factor(data$exang)
data$oldpeak <- as.character(data$oldpeak)
data$oldpeak <- as.numeric(data$oldpeak)
str(data)

# Calculate Mode

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data$trestbps[which(is.na(data$trestbps))] <- mean(data$trestbps, na.rm = TRUE)
data$chol[which(is.na(data$chol))] <- mean(data$chol, na.rm = TRUE)
getmode(data$fbs)
data$fbs[which(is.na(data$fbs))] <- 0
getmode(data$restecg)
data$restecg[which(is.na(data$restecg))] <- 0
data$thalach[which(is.na(data$thalach))] <- mean(data$thalach, na.rm = TRUE)
getmode(data$exang)
data$exang[which(is.na(data$exang))] <- 0
getmode(data$oldpeak)
data$oldpeak[which(is.na(data$oldpeak))] <- 0


# initialize some data structures to store metrics for each model
accuracy<-vector()
precision<-vector()
recall<-vector()


# create 10 folds (partitions of the data after shuffering it)

datarandom<-data[sample(nrow(data)),]
folds<-cut(seq(1,nrow(data)), breaks=10, labels=FALSE)
set.seed(1)
for(i in 1:10){
  
  # split dataset
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE) 
  data_all.test <- datarandom[testIndexes, ] 
  data_all.train <- datarandom[trainIndexes, ]
  
  #Train model with Naive Bayes 
  
  naive_model<- naive_bayes(data_all.train$class ~., data = data_all.train)
  naive_model
  
  
  # generate  Matrix table
  prediction <- predict(naive_model, data_all.test)
  
  m<- table(prediction, actual= data_all.test$class)
  
  #calculate accuracy, predict and recall
  print(m)
  n=sum(m)
  nc=nrow(m)
  
  diag=diag(m)
  rowsums=apply(m,1,sum)
  colsums=apply(m,2,sum)
  p=rowsums
  q=colsums
  accuracy=sum(diag)/n
  accuracy
  
  precision=diag/colsums
  
  recall=diag/rowsums
  
}
accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

f_measure<-2*((precision*recall)/(precision+recall))
f_measure
f_measureaverage=mean(f_measure)
f_measureaverage

#  generate plot e.g. column 4 and 5

data(datarandom) # load datarandom dataset
pairs(datarandom[4:5], main="Heart Disease Prediciton (important factors)", 
      pch=21, bg=c("red","green3","blue")[unclass(datarandom$class)])


#Create model 
naive_model<- naive_bayes(data$class ~., data = data)
naive_model

