#Class Assignment 3

#Regression Tree Example
library(rpart)
library(rpart.plot)
library(ggplot2)
data('msleep')
str(msleep)
help('msleep')
head(msleep)

#creating new data frame
mSleepDF1 <- msleep[,c(3,6,10,11)] # 3 = vore e.g herbivore ,6=sleep_total, 10=brainwt, 11=bodywt
str(mSleepDF1)
head(mSleepDF1)

#Building Regression Decsion Tree that predicts the total sleeping hours of the mamals based on the other variables available on the dataset
help('rpart') #Recursive Partitioning and Regression Trees
sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = "anova")
# method we are using here is anova becuase our target here is sleep_total is a numerical one.
sleepModel_1

#visualising the model
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE) #leaf nodes all collected at bottom
rpart.plot(sleepModel_1, type = 3, fallen.leaves = F)
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) # with 3 digits
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)


#Classification Tree
require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)

#setting seed to get randomly generated numbers when running the model over and over
set.seed(9850)
# generate random numbers
grn <-runif(nrow(iris))
head(grn)
grn
#creating a randomised iris dataset 
irisrand <-iris[order(grn),]
head(irisrand)
head(iris)
str(irisrand)
str(iris)
classificationmodel1 <-C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <-predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)
table(irisrand[101:150,5],Predicted = prediction1)
plot(classificationmodel1)

#NaiveBayes Classifier
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red",main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 

#EXTRAS
#ctree1
require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
#find "prettier" ways to plot the tree

#ctree2
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

#ctree3
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosisâ€")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")



