#Lab 7

#Rpart2
require(rpart)
# build the  tree
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) # display the results
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) # visualize cross-validation results
# plot tree
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.5)
# prune the tree
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
# plot the pruned tree
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.6)
post(pfitM, file = "ptree2.ps", title = "Pruned Regression Tree for Mileage")

#Rpart3
library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE,cex=0.5)
plot(rpart.pred)

#Rpart4
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.6)

post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis") 
pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
#error - fit is not a tree, just a root
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.6)
post(pfitK, file = "ptree.ps", title = "Pruned Classification Tree for Kyphosis")


