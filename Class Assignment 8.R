#Rest of Labs
#Random Forest
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	
importance(fitKF) 

fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) 
importance(fitSwiss) 
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest)

#Support vector classificaton
library(e1071)
set.seed (1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
plot(x, col=(3-y)) #checking if lineraly separable
dat <- data.frame(x = x, y  = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
plot(svmfit , dat)
summary(svmfit)

#cost = 0.1
svmfit <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

#svm1
n <- 150 
p <- 2 
sigma <- 1 
meanpos <- 0 
meanneg <- 3 
npos <- round(n/2) 
nneg <- n-npos 
# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

ntrain <- round(n*0.8) 
tindex <- sample(n,ntrain) 
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1
# Visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))

#Example in pdf

library("kernlab")
data("iris")
irismodel <- ksvm(Species ~ ., data = iris,
                  + type = "C-bsvc", kernel = "rbfdot",
                  + kpar = list(sigma = 0.1), C = 10,
                  + prob.model = TRUE)
irismodel

predict(irismodel, iris[c(3, 10, 56, 68,
                          + 107, 120), -5], type = "probabilities")
predict(irismodel, iris[c(3, 10, 56, 68,
                          + 107, 120), -5], type = "decision")

k <- function(x, y) {
  + (sum(x * y) + 1) * exp(0.001 * sum((x -
                                          + y)^2))
  + }

class(k) <- "kernel"
data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene,
               + kernel = k, C = 10, cross = 5)
gene

#Svm2 -13
library(rpart)
data(Ozone, package="mlbench")
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

data(iris)
model <- svm(Species ~ ., data = iris)
print(model)
summary(model)
pred <- fitted(model)
table(pred, y)

pred <- fitted(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

#prediction
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

#density-estimation
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)
m <- svm(~., data = X, gamma = 0.1)
# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)
# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)
# weights
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)

data(promotergene)
ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]
gene <-  ksvm(Class~.,data=genetrain,kernel="rbfdot",
              kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)
genetype <- predict(gene,genetest,type="probabilities")

library(e1071) 
m1 <- matrix( c( 
  0,    0,    0,    1,    1,    2,     1, 2,    3,    2,    3, 3, 0, 1,2,3, 
  0, 1, 2, 3, 
  1,    2,    3,    2,    3,    3,     0, 0,    0,    1, 1, 2, 4, 4,4,4,    0, 
  1, 2, 3, 
  1,    1,    1,    1,    1,    1,    -1,-1,  -1,-1,-1,-1, 1 ,1,1,1,     1, 
  1,-1,-1 
), ncol = 3 ) 

Y = m1[,3] 
X = m1[,1:2] 

df = data.frame( X , Y ) 

par(mfcol=c(4,2)) 
for( cost in c( 1e-3 ,1e-2 ,1e-1, 1e0,  1e+1, 1e+2 ,1e+3)) { 
  #cost <- 1 
  model.svm <- svm( Y ~ . , data = df ,  type = "C-classification" , kernel = 
                      "linear", cost = cost, 
                    scale =FALSE ) 
print(model.svm$SV) 
  
plot(x=0,ylim=c(0,5), xlim=c(0,3),main= paste( "cost: ",cost, "#SV: ", 
                                                 nrow(model.svm$SV) )) 
points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}

data(spam)
index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter
mailtype <- predict(filter,spamtest[,-58])
table(mailtype,spamtest[,58])


data(iris)
library('kernlab')
rbf <- rbfdot(sigma=0.1)
rbf
irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)

irismodel
fitted(irismodel)
predict(irismodel, iris[,-5], type="probabilities")

## Demo of the plot function - something wrong with x
x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60),rep(-1,60)))

svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)

K <- as.kernelMatrix(crossprod(t(x)))

svp2 <- ksvm(K, y, type="C-svc")

svp2


xtest <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))

Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))

predict(svp2, Ktest)

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene

data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv

x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)


regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene


data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv



svp <- ksvm(xtrain,ytrain,type="C-svc",kernel='vanilladot',C=100,scaled=c())
svp
attributes(svp)
alpha(svp)
alphaindex(svp)
b(svp)
plot(svp,data=xtrain) #does not work


cv.folds <- function(n,folds=3)

{
  split(sample(n),rep(1:folds,length=length(y)))
}
cv.ksvm <- function(x,y,folds=3,...) 
  
svp <- ksvm(x,y,type="C-svc",kernel='vanilladot',C=1,scaled=c(),cross=5)
print(cross(svp))

#svm rpart1
library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
table(pred = svm.pred, true = testset[,10])

#Fa
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
prcomp(m1) 
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

install.packages("Hmisc")
library(Hmisc)
#doent get the data
AthleticsData <- spss.get("AthleticsData.sav")
names(AthleticsData)
cor(AthleticsData)
prcomp(AthleticsData)

fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)

fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)

install.packages("GPArotation")
library(GPArotation)

fit <- principal(AthleticsData, nfactors=3, rotate='varimax')
fit

data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary) #show the items and keying information

#Loess
data(cars)
cars.lo <- loess(dist ~ speed, cars)
predict(cars.lo, data.frame(speed = seq(5, 30, 1)), se = TRUE)
cars.lo2 <- loess(dist ~ speed, cars, control = loess.control(surface = "direct"))
predict(cars.lo2, data.frame(speed = seq(5, 30, 1)), se = TRUE)

require(graphics)
plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)

with(cars, {
  plot(speed, dist)
  lines(supsmu(speed, dist))
  lines(supsmu(speed, dist, bass = 7), lty = 2)
})


#Splines
data(cars)

attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)

lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df = 10), lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')
detach()


plot(residuals(cars.spl) ~ fitted(cars.spl))
abline(h = 0, col = "gray")

stopifnot(all.equal(cars$dist,
                    fitted(cars.spl) + residuals(cars.spl)))

nKnots <- Vectorize(.nknots.smspl) ; c.. <- adjustcolor("gray20",.5)
curve(nKnots, 1, 250, n=250)
abline(0,1, lty=2, col=c..); text(90,90,"y = x", col=c.., adj=-.25)
abline(h=100,lty=2); abline(v=200, lty=2)

n <- c(1:799, seq(800, 3490, by=10), seq(3500, 10000, by = 50))
plot(n, nKnots(n), type="l", main = "Vectorize(.nknots.smspl) (n)")
abline(0,1, lty=2, col=c..); text(180,180,"y = x", col=c..)
n0 <- c(50, 200, 800, 3200); c0 <- adjustcolor("blue3", .5)
lines(n0, nKnots(n0), type="h", col=c0)
axis(1, at=n0, line=-2, col.ticks=c0, col=NA, col.axis=c0)
axis(4, at=.nknots.smspl(10000), line=-.5, col=c..,col.axis=c.., las=1)

y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))
xx  <- seq(1, length(y18), len = 201)
(s2  <- smooth.spline(y18)) # GCV
(s02  <- smooth.spline(y18, spar = 0.2))
(s02. <- smooth.spline(y18, spar = 0.2, cv = NA))
plot(y18, main = deparse(s2$call), col.main = 2)
lines(s2, col = "gray"); lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); mtext(deparse(s02$call), col = 3)

(s2  <- smooth.spline(y18, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
(s2m <- smooth.spline(y18, cv = TRUE, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))

require(graphics)

op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = 0.1+c(3,3,3,1))
n <- 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through", n, "points"))
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)

y <- (x-6)^2
plot(x, y, main = "spline(.) -- 3 methods")
lines(spline(x, y, n = 201), col = 2)
lines(spline(x, y, n = 201, method = "natural"), col = 3)
lines(spline(x, y, n = 201, method = "periodic"), col = 4)
legend(6, 25, c("fmm","natural","periodic"), col = 2:4, lty = 1)

y <- sin((x-0.5)*pi)
f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- get("z", envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
curve(f(x, deriv = 1), 1, 10, col = 2, lwd = 1.5)
curve(f(x, deriv = 2), 1, 10, col = 2, lwd = 1.5, n = 401)
curve(f(x, deriv = 3), 1, 10, col = 2, lwd = 1.5, n = 401)
par(op)

.x <- splinecoef$x
u <- seq(3, 6, by = 0.25)
(ii <- findInterval(u, .x))
dx <- u - .x[ii]
f.u <- with(splinecoef,
            y[ii] + dx*(b[ii] + dx*(c[ii] + dx* d[ii])))
stopifnot(all.equal(f(u), f.u))

set.seed(1); x <- round(rnorm(30), 1); y <- sin(pi * x) + rnorm(30)/10
plot(x, y, main = "spline(x,y)  when x has ties")
lines(spline(x, y, n = 201), col = 2)
tx <- table(x); mx <- as.numeric(names(tx[tx > 1]))
ry <- matrix(unlist(tapply(y, match(x, mx), range, simplify = FALSE)),
             ncol = 2, byrow = TRUE)
segments(mx, ry[, 1], mx, ry[, 2], col = "blue", lwd = 2)

n <- 20
set.seed(11)
x. <- sort(runif(n)) ; y. <- cumsum(abs(rnorm(n)))
plot(x., y.)
curve(splinefun(x., y.)(x), add = TRUE, col = 2, n = 1001)
curve(splinefun(x., y., method = "monoH.FC")(x), add = TRUE, col = 3, n = 1001)
curve(splinefun(x., y., method = "hyman")   (x), add = TRUE, col = 4, n = 1001)
legend("topleft",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")

x. <- c(7.09, 8.09, 8.19, 8.7, 9.2, 10, 12, 15, 20)
f <- c(0, 2.76429e-5, 4.37498e-2, 0.169183, 0.469428, 0.943740,
       0.998636, 0.999919, 0.999994)
s0 <- splinefun(x., f)
s1 <- splinefun(x., f, method = "monoH.FC")
s2 <- splinefun(x., f, method = "hyman")
plot(x., f, ylim = c(-0.2, 1.2))
curve(s0(x), add = TRUE, col = 2, n = 1001) -> m0
curve(s1(x), add = TRUE, col = 3, n = 1001)
curve(s2(x), add = TRUE, col = 4, n = 1001)
legend("right",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")

xx <- m0$x
plot(xx, s1(xx) - s2(xx), type = "l",  col = 2, lwd = 2,
     main = "Difference   monoH.FC - hyman"); abline(h = 0, lty = 3)

x <- xx[xx < 10.2] ## full range: x <- xx .. does not show enough
ccol <- adjustcolor(2:4, 0.8)
matplot(x, cbind(s0(x, deriv = 2), s1(x, deriv = 2), s2(x, deriv = 2))^2,
        lwd = 2, col = ccol, type = "l", ylab = quote({{f*second}(x)}^2),
        main = expression({{f*second}(x)}^2 ~" for the three 'splines'"))
legend("topright",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       lwd = 2, col  =  ccol, lty = 1:3, bty = "n")

#IDA
library(kknn)
data(ionosphere)
i2<-ionosphere[,-1]
i2<-i2[,-1]
library(MASS)
ifit <- lda(class ~ ., data=i2, na.action="na.omit", CV=TRUE)
summary(ifit)

ctable <- table(i2$class, ifit$class)
diag(prop.table(ctable, 1))
sum(diag(prop.table(ctable)))













