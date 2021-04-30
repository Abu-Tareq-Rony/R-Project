#Import library for dataset
library(ISLR)
library(class)
auto_new<-Auto[,-9]
newOrigin<- c("USA", "European", "Japanese")
auto_new$origin<- factor(newOrigin[auto_new$origin], newOrigin)
head(auto_new)#to see the dataset

set.seed(1234)
train <- sample(1:dim(Auto)[1], 392*.7, rep=FALSE)
test <- -train
cl_train = train
cl_test = test

library(fields)
myknn <- function(train, test, cl_test,cl_train, k)#we can check it by using default k=3
{
  # Loading library
  require(fields)
  # create an empty placeholder for predicted values
  pred = c()
  
  # calculate distance
  # The output of the "dist" dataframe is such that the rows are the 
  #     training data points, while the columns are the testing observations.
  #     The cells for each row-column pair are the Euclidean distance from
  #     training data to the corresponsing testing data
  dist = rdist(train, test)
  
  # Create a loop for each testing observation
  for (i in 1:nrow(test))
  {
    nb = data.frame(dist = dist[,i], class = cl_test)
    
    # Ranking the rows in the dataframe by the distance from the testing
    #   observation. nb stands Neighbourhood
    nb = nb[order(nb$dist),]
    
    # Choose the K closest Neighbour
    topnb = nb[1:k,]
    
    #Deciding the Class by picking the highest occurence name.
    ans = names(sort(summary(topnb$class), decreasing=T)[1])
    
    # concatenate the latest prediction to the previous one
    pred= c(pred, ans)
  }
  return(pred)
}


#A categorical vector for the predicted categories for the testing:- 
pred


#The accuracy and confusion matrix of the classification
err
#error rate,1-.o9=.91



#The value of k will use
set.seed(101); n = 500
x = matrix(rnorm(n, sd = 5),n/2,2)
class = rep(c(1,2),each = n/4)
x[class == 1,] = x[class == 1,] + 7
set.seed(101)
train = sample(1:n/2, n/4); test = -train
x.train = x[train,]; x.test = x[test,]
class.train = class[train]; class.test = class[test]
kchoice = c(3,5)
# To store the error rate
err = matrix(NA, 2,2)
colnames(err) = c("train", "test")
rownames(err) = c("k = 3", "k = 5")

# initializa column value
j = 1

for(i in kchoice)
{
  # For training error
  pred = myknn(x.train, x.train, as.factor(class.train),k = i)
  err[j,1] = mean(pred!=as.factor(class.train))
  # For testing error
  pred = myknn(x.train, x.test, as.factor(class.train),k = i)
  err[j,2] = mean(pred!=as.factor(class.test))
  #Update
  j = j + 1
}
err
#The testing error rate is always larger than that or training error rate. 
#In addition, using a small k, such as , will have lower training error.  
#in fact using k= 1 will give 0 training error. We also observe that using 
#k= 3 perform better than k=5 in the test data.



#Make a plot of the accuracy versus k to determine the best number of neighbors to use.
make.grid=function(x,n=200){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}


#look at KNN = 3.
xgrid = make.grid(x, n = 150)
ygrid = myknn(x,xgrid, as.factor(class), k = 3)
plot(xgrid,col=c("orange","dodgerblue")[as.numeric(ygrid)],pch = 20,cex = .2, main = "KNN = 3")
points(x,col = c("orangered", "navyblue")[class],pch = 20)
#Those data points that fall outside of their boundaries (defined by colors in the plot) are classfied as errors.

#The following uses KNN = 5
ygrid = myknn(x,xgrid, as.factor(class), k = 5)
plot(xgrid,col=c("orange","dodgerblue")[as.numeric(ygrid)],pch = 20,cex = .2, main = "KNN = 5")

#How about using KNN = 10?
points(x,col = c("orangered", "navyblue")[class],pch = 20)
ygrid = myknn(x,xgrid, as.factor(class), k = 10)
plot(xgrid,col=c("orange","dodgerblue")[as.numeric(ygrid)],pch = 20,cex = .2, main = "KNN = 10")
points(x,col = c("orangered", "navyblue")[class],pch = 20)

#table
table(test, train)
