library(class)
path<-'/Users/megan/Documents/STAT315A_stanford/assignment&exam/hw1'
setwd(path)
temp<-readLines("train.txt")
train<-list()
m=length(temp)
rm(temp)
options(digits=6)
for (i in 1:m){
  train[[i]]=as.numeric(unlist(strsplit(temp[i], " ")))
}

p=ncol(train)
train<-matrix(unlist(train), ncol = p, byrow = TRUE)
y=train[,1]
x=train[,-1]

set.seed(123)
cv<-function(x,y,fs,k){
  m=nrow(x)
  #temp=x
  fdsize=floor(m/fs)
  trainsize=fdsize*(fs-1)
  testsize=m-fdsize*(fs-1)
  ind=sample(1:m,trainsize,replace=FALSE)
  
  train_x=x[,ind]
  train_y=x[,ind]
  test_x=x[,-ind]
  test_y=x[,-ind]
  
  test_yhat=knn(train_x,test_x,cl=factor(train_y),k = k)
  train_yhat=knn(train_x,train_x,cl=factor(train_y),k = k)
  
  train_err= mean(train_yhat != test_yhat)
  test_err=mean(test_yhat != test_yhat)
}


