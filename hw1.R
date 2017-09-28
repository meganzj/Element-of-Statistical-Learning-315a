library(class)
library(dplyr)
library(ggplot2)
library(tidyr)
####################################################################################################
###############     DATA INPUT     #################
path <-
  '/Users/megan/Documents/STAT315A_stanford/assignment&exam/hw1'
setwd(path)
temp <- readLines("train.txt")
train <- list()
m = length(temp)
options(digits = 6)
for (i in 1:m) {
  train[[i]] = as.numeric(unlist(strsplit(temp[i], " ")))
}
rm(temp)
#p = ncol(train)
train <- matrix(unlist(train), ncol = 257, byrow = TRUE)
train <- train[which(train[, 1] %in% c(2, 3)), ]
#y = train[, 1]
#x = train[,-1]

###############     FUNCTION     #################
knncv <- function(X, fs, k) {
  m = nrow(X)
  fdsize = floor(m / fs) # #of elements in first (fs-1) fold
  lastsize = m - fdsize * (fs - 1) # elements in last fold
  firstfold = rep(1:(fs - 1), fdsize)
  lastfold = rep(fs, lastsize)
  set.seed(123)
  ind = sample(c(firstfold, lastfold)) #randomly assign the 1-10fold indicator
  #to dataset
  mse = matrix(0, fs * length(k), 4) #k,fs,train_mse,test_mse
  m_ind = 0
  for (i in 1:length(k)) {
    for (j in 1:fs) {
      j_ind = which(ind == j)
      train_X = X[-j_ind,-1]
      train_y = X[-j_ind, 1]
      test_X = X[j_ind,-1]
      test_y = X[j_ind, 1]
      test_yhat = knn(train_X, test_X, cl = factor(train_y), k = k[i])
      train_yhat = knn(train_X, train_X, cl = factor(train_y), k = k[i])
      #
      train_err = mean(train_yhat != train_y)
      test_err = mean(test_yhat != test_y)
      print(c(train_err, test_err))
      m_ind = m_ind + 1
      print(c(i, j, m_ind))
      mse[m_ind, 1] = k[i]
      mse[m_ind, 2] = j
      mse[m_ind, 3] = train_err
      mse[m_ind, 4] = test_err
    }
  }
  return(mse)
}


lscv <- function(X, fs) {
  m = nrow(X)
  fdsize = floor(m / fs) # #of elements in first (fs-1) fold
  lastsize = m - fdsize * (fs - 1) # elements in last fold
  firstfold = rep(1:(fs - 1), fdsize)
  lastfold = rep(fs, lastsize)
  set.seed(123)
  ind = sample(c(firstfold, lastfold)) #randomly assign the 1-10fold indicator to dataset
  mse = matrix(0, fs, 3) #k,fs,train_mse,test_mse
  m_ind = 0
  
  for (j in 1:fs) {
    j_ind = which(ind == j)
    train_X = X[-j_ind,-1]
    train_y = X[-j_ind, 1]
    test_X = X[j_ind,-1]
    test_y = X[j_ind, 1]
    
    ols = lm(train_y ~ train_X)
    beta = ols$coefficients
    
    test_yhat = beta[1] + test_X %*% beta[-1]
    test_yhat = ifelse(test_yhat > 2.5, 3, 2)
    train_yhat = ols$fitted
    train_yhat = ifelse(train_yhat > 2.5, 3, 2)
    
    train_err = mean(train_yhat != train_y)
    test_err = mean(test_yhat != test_y)
    
    m_ind = m_ind + 1
    mse[m_ind, 1] = j
    mse[m_ind, 2] = train_err
    mse[m_ind, 3] = test_err
  }
  return(mse)
}


cobo<-function(X,k,r) {
  m=nrow(X)
  m1=floor(m * r)
  m2= m - m1
  ind = c(rep(0,m1),rep(1,m2))
  set.seed(123)
  ind = sample(ind)
  train_X = X[which(ind == 0),-1]
  train_y = X[which(ind == 0), 1]
  test_X = X[which(ind == 1), -1]
  test_y = X[which(ind == 1), 1]
  
  test_yhat1<-knn(train_X, test_X, cl = factor(train_y), k = k)
  lm1<-lm(train_y ~ train_X)
  beta<-lm1$coefficients
  test_yhat2<-beta[1] + test_X %*% beta[-1]
  test_yhat2<- ifelse(test_yhat2>2.5,3,2)
  ind<-which(test_yhat1 != test_y)
  test_yhat1[ind] = test_yhat2[ind]
  cobo_error = mean(test_yhat1 != test_y)
  return(cobo_error)
}


knn_<-function(X,k,r) {
  m=nrow(X)
  m1=floor(m * r)
  m2= m - m1
  set.seed(123)
  ind = c(rep(0,m1),rep(1,m2))
  ind = sample(ind)
  train_X = X[which(ind == 0),-1]
  train_y = X[which(ind == 0), 1]
  test_X = X[which(ind == 1), -1]
  test_y = X[which(ind == 1), 1]
  
  test_yhat<-knn(train_X, test_X, cl = factor(train_y), k = k)
  knn_error=mean(test_yhat != test_y)
  return(knn_error)
}


ls_<-function(X,r){
  m=nrow(X)
  m1=floor(m * r)
  m2= m - m1
  ind = c(rep(0,m1),rep(1,m2))
  set.seed(123)
  ind = sample(ind)
  train_X = X[which(ind == 0),-1]
  train_y = X[which(ind == 0), 1]
  test_X = X[which(ind == 1), -1]
  test_y = X[which(ind == 1), 1]
  
  lm1<-lm(train_y ~ train_X)
  beta<-lm1$coefficients
  test_yhat2<-beta[1] + test_X %*% beta[-1]
  test_yhat2<- ifelse(test_yhat2>2.5,3,2)
  ls_error=mean(test_yhat2 != test_y)
  return(ls_error)
}
###############     CROSS-VALIDATION    #################
k = c(1, 3, 5, 7, 15) #define knn's k's candidate
knnmse <- knncv(train, 10, k)
lsmse <- lscv(train, 10)

knnmse <- data.frame(knnmse)
lsmse <- data.frame(lsmse)

knnmse_summary <- knnmse %>%
  group_by(X1) %>%
  summarise(train_err = mean(X3, na.rm = TRUE),
            test_err = mean(X4, na.rm = TRUE))
lsmse_summary <- lsmse %>%
  summarise(train_err = mean(X2, na.rm = TRUE),
            test_err = mean(X3, na.rm = TRUE))
#FOR KNN ALGO, K = 5 HAS THE MIN TEST_ERROR

###############   MODEL      #################
knn_(train,5,0.7)
ls_(train,0.7)
cobo(train,5,0.7)

###############   PLOT      #################
l=length(k)
mat1<-matrix(k,l,1)
v=unlist(rep(lsmse_summary,l))
mat2<-matrix(v,l,2,byrow=TRUE)
lsmse_summary2<-tbl_df(cbind(mat1,mat2))
colnames(lsmse_summary2)<-c("X1","train_err","test_err")

knnmse_summary2<-gather(knnmse_summary,split,error,train_err:test_err,factor_key = TRUE)  
lsmse_summary2<- gather(lsmse_summary2,split,error,train_err:test_err,factor_key = TRUE)
plotdata<-bind_rows("knn" = knnmse_summary2, "ls" = lsmse_summary2, .id = "Algo") %>% 
          mutate(group = paste(Algo, split, sep = ","))
g1<-ggplot(data = plotdata, aes(y = error, x = X1 )) + 
    geom_line(aes(group = group, colour = group)) +
    xlab("K")