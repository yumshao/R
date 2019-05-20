library(ISLR)

head(Auto)
n <- dim(Auto)[1]
plot(Auto$horsepower,Auto$mpg)

# Validation Set Approach

set.seed(1)

train <- sample(x=n,size=n/2,replace=FALSE)
train.data <- Auto[train,]
test.data <- Auto[-train,]

lm.fit1 <- lm(mpg~horsepower, data=train.data)
lm.pred1 <- predict(lm.fit1,test.data)
mse1 <- mean((test.data$mpg-lm.pred1)^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=train.data)
lm.pred2 <- predict(lm.fit2,test.data)
mse2 <- mean((test.data$mpg-lm.pred2)^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=train.data)
lm.pred3 <- predict(lm.fit3,test.data)
mse3 <- mean((test.data$mpg-lm.pred3)^2)

lm.fit10 <- lm(mpg~poly(horsepower,10), data=train.data)
lm.pred10 <- predict(lm.fit10,test.data)
mse10 <- mean((test.data$mpg-lm.pred10)^2)

plot(c(1,2,3,10),c(mse1,mse2,mse3,mse10),pch=5, type="b",ylim=c(15,30))

lm.fit1.1 <- lm(mpg~horsepower, data=Auto)
lm.pred1.1 <- predict(lm.fit1.1,Auto)
mse1.1 <- mean((Auto$mpg-lm.pred1.1)^2)

lm.fit2.1 <- lm(mpg~poly(horsepower,2), data=Auto)
lm.pred2.1 <- predict(lm.fit2.1,Auto)
mse2.1 <- mean((Auto$mpg-lm.pred2.1)^2)

lm.fit3.1 <- lm(mpg~poly(horsepower,3), data=Auto)
lm.pred3.1 <- predict(lm.fit3.1,Auto)
mse3.1 <- mean((Auto$mpg-lm.pred3.1)^2)

lm.fit10.1 <- lm(mpg~poly(horsepower,10), data=Auto)
lm.pred10.1 <- predict(lm.fit10.1,Auto)
mse10.1 <- mean((Auto$mpg-lm.pred10.1)^2)

points(c(1,2,3,10),c(mse1.1,mse2.1,mse3.1,mse10.1),pch=19, type="b",col="red")

# Leave-One-Out Cross-Validation (LOOCV)

#The LOOCV can be automatrically computed with glm() and cv.glm()

glm.fit <- glm(mpg~horsepower, data=Auto)  # The result is equivalent to the one with lm(). 
                                           # glm() can be used together with cv.glm(), and this is part of the boot library.

library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto,glm.fit)
names(cv.err)                  # K: The value of K used for the K fold cross validation
                               # delta: The first one is the raw cross-validation estimate of prediction error. The second one
                               #        is the adjusted cross-validation estimate.

cv.error <- rep(NA,5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}
cv.error 
                    
# k-Fold Cross Validation

set.seed(17)

cv.error.10 <- rep(NA,10)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

# Exercise 1.
# Cross-Validation can also be used to estimate the test error for a classification problem.
# Run a logit model with the Smarket data. The dependent variable is Direction and  

library(ISLR)

glm.fit <- glm(Direction~Lag1+Lag2, family=binomial, data=Smarket)
summary(glm.fit)

# Compare this model with the following models using K fold cross-validation with K=10.
# Direction~Lag1+Lag2+Lag3, Direction~Lag1+Lag2+Lag3+Lag4, Direction~Lag1+Lag2+Lag3+Lag4+Lag5
head(Smarket)

set.seed(1)
aa <- rep(NA,40)
cv.error <- matrix(aa,10,4)

n <- nrow(Smarket)
ni <- n/10
a <- sample(1:n,n,replace=FALSE) # randomly select 1250 number
Sdata1 <- Smarket[a,] # rearrange the order of the observation randomly 

###cv.error <- rep(NA,4)
for (j in 3:6) { # j=3:6 means Lag2 to Lag5 (third column to sixth column)
  for (i in 1:10) {
    test.index <- (ni * (i-1) + 1) :  (ni * i) # select kth test data
    test.data <- Sdata1[test.index,c(2:j,9)]
    train.data <- Sdata1[-test.index,c(2:j,9)]
    glm.fit <- glm(Direction~.,family=binomial,data=train.data)
    glm.probs <- predict(glm.fit,test.data, type="response")
    glm.pred <- rep("Down",ni)    
    glm.pred[glm.probs>0.5] <- "Up"    
    cv.error[i,j-2] <- mean(glm.pred!=test.data[,j]) # ??? incorrect number of subscripts on matrix
  }
}

colMeans(cv.error)

# Exercise 2.
# Consider KNN estimation to predict direction using Lag1 and Lag2. To choose the optimal number of neigbhors,
# use the 10 fold cross validation.

library(class)

set.seed(1)
aa <- rep(NA,100)
cv.error <- matrix(aa,10,10)

Sdata1.temp <- Smarket[,c(2,3,9)]
n <- nrow(Sdata1.temp)
ni <- n/10
a <- sample(1:n,n,replace=FALSE)
Sdata1 <- Sdata1.temp[a,]
Sdata1$Lag1 <- scale(Sdata1$Lag1)
Sdata1$Lag2 <- scale(Sdata1$Lag2)

for (j in 1:10) {
  for (i in 1:10) {
    test.index <- (ni * (i-1) + 1) :  (ni * i)
    test.data <- Sdata1[test.index,]
    train.data <- Sdata1[-test.index,]
    yhat <- knn(train.data[,1:2],test.data[,1:2],train.data[,3],k=j+30)
    cv.error[i,j] <- mean(yhat!=test.data[,3])
  }
}

colMeans(cv.error)

# Louis's work

install.packages("caret")
library(caret)

df <- Smarket
trControl = trainControl(method="cv", number=10)
fit <- train(Direction~Lag1+Lag2, method="knn", tuneGrid=expand.grid(k=1:50), trControl=trControl, metric="Accuracy", data=Smarket)
fit

