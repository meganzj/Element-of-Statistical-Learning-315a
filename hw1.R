library(class)

####################################################################################################
###############     DATA INPUT     #################
path <- '/Users/megan/Documents/STAT315A_stanford/assignment&exam/hw1'
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
y = train[, 1]
x = train[, -1]

###############     FUNCTION     #################
k = c(1,3,5,7,15) #define knn's k's candidate

knncv <- function(x, y, fs, k) {
  m = nrow(x)
  fdsize = floor(m / fs) # #of elements in first (fs-1) fold
  lastsize = m - fdsize * (fs - 1) # elements in last fold
  firstfold = rep(1:(fs - 1), fdsize)
  lastfold = rep(fs, lastsize)
  set.seed(123)
  ind = sample(c(firstfold, lastfold)) #randomly assign the 1-10fold indicator
                                       #to dataset
  mse = matrix(0,fs*length(k),4) #k,fs,train_mse,test_mse
  m_ind=0
   for (i in 1:length(k)) {
     for (j in 1:fs) {
      # for (i in 1:1) {
      #   for (j in 1:10) {    
       j_ind=which(ind == j)
       #print(j_ind)
       train_x = x[-j_ind,-1]
       train_y = x[-j_ind,1]
       test_x = x[j_ind,-1]
       test_y = x[j_ind,1]
      # print(c(nrow(train_y),nrow(train_x),nrow(test_x)))
       test_yhat = knn(train_x, test_x, cl = factor(train_y), k = k[i])
       train_yhat = knn(train_x, train_x, cl = factor(train_y), k = k[i])
      # 
       train_err = mean(train_yhat != train_y)
       test_err = mean(test_yhat != test_y)
       print(c(train_err,test_err))
      m_ind=m_ind + 1
      print(c(i,j,m_ind))
      mse[m_ind,1]=k[i]
      mse[m_ind,2]=j
      mse[m_ind,3]=train_err
      mse[m_ind,4]=test_err
    }
  }
  return(mse)
}
msetest<-knncv(x,y,10,k)
