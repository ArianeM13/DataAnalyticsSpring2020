#CROSS VALIDATION
library(ISLR)
set.seed(1)
help("sample")
train = sample(392,196)
train
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
#want to predict the response for all 392 observations
#mean function used to calculate the MSE of the 196 observations in 
#the validation set. Note the train selects only the observations
#that are not in the training set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#linear regression fit is 23.26001

#we can use the poly() function to estimate the test error
#for quadratic and cubic regression
#Quadratic regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset =train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#fit is 18.71646
#Cubic regressin line
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset =train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#18.79401

#Different Training set will give slightly different errors but
#show same trend that quadartic fits best

#RANDOM FOREST EXAMPLE
install.packages('randomForest')
library(randomForest)
data1<- read.csv(file.choose(),header = T)
head(data1)
colnames(data1) <- c('BuyingPrice', 'Maintenance', 'NumDoors', 'NumPersons','BootSpace','Safety', 'Condition')
head(data1)
str(data1)
levels(data1$Condition)
summary(data1)

set.seed(100)
train <- sample(nrow(data1), replace = F)
TrainSet <- data1[train,]
ValidSet <- data1[-train,] #'-train' means not in train
summary(TrainSet)
summary(ValidSet)

help(randomForest)
#Random Forest Model with default parameters
model1 <- randomForest(Condition ~ ., data=TrainSet, importance = T)
model1
#By default, number of trees is 500 and the number of variables
#at each split is 2 in this case

#fine tuning the parametes of the RandomForest model
#we have increased the metry from 2 to 6
#mtry = Number of Variables randomly sampled as candidate at each split
model2<- randomForest(Condition ~ ., data=TrainSet, ntree = 500, mtry = 6, importance = T)
model2

#We can also use the importance fincation to check important variables
#the below functions show the drop in mean accuracy of the variables
importance(model2)
varImpPlot(model2)

#For lopp and check for different values of mtry
#Using a For-Loop to identifiy the right 'mtry' for the model

a=c()
i = 5
for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data=TrainSet, ntree = 500, mtry = i, importance = T)
  predValid <- predict(model3, ValidSet, type = 'class')
  a[i-2] = mean(predValid == ValidSet$Condition)
  }
a








