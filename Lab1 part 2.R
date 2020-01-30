multivariate <-read.csv(file.choose(), header = T)
help("attach")  #so dont need to keep telling R look into multivariate ataset
attach(multivariate)
help(lm)
mm <- lm(Homeowners~Immigrant) #gives fromula in format y = mx+c
mm
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
abline(mm,col=3,lwd=10) #col - colour, lwd - line width
attributes(mm)
mm$coefficients

#to prefdict homeowners for 0 immigrants and 20 immigrants
newImmigrantData <- data.frame(Immigrant = c(0,20))
predict(mm,newImmigrantData)

plot(mtcars$wt,mtcars$mpg)
library(ggplot2) #same as attaching dataset so wouldnt have to keep saying ggplot2
help(library)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg, data = mtcars) #same as above
ggplot(mtcars,aes(x=wt,y=mpg))+ geom_point() #same as above
plot(pressure$temperature,pressure$pressure, type ='l') #normal R plot and without type l it just plots points
points(pressure$temperature,pressure$pressure) #plots points on previous plot

lines(pressure$temperature,pressure$pressure/2, col = 'red') #plots line on previous plot for 0.5 pressure
points(pressure$temperature,pressure$pressure/2, col='blue')

library(ggplot2) 
qplot(pressure$temperature,pressure$pressure, geom ='line')
qplot(temperature,pressure,data = pressure, geom = 'line') #same plot as before
ggplot(pressure, aes(x=temperature,y=pressure)) + geom_line() + geom_point()


#Creating Bar Graphs
barplot(BOD$demand, name.arg = BOD$Time)
table(mtcars$cyl) 
barplot(table(mtcars$cyl))
qplot(mtcars$cyl) #cyl is continuous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete
#Bar graph of counts
qplot(factor(cyl),data =mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

#Creating Histogram
#Viewing the distribution of one-dimensional data with a histogram

hist(mtcars$mpg)
hist(mtcars$mpg, breaks =10)
hist(mtcars$mpg, breaks =5)
hist(mtcars$mpg, breaks =12)
qplot(mpg, data =mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=5)

#Creating Box-plot
ToothGrowth
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = 'boxplot')
ggplot(ToothGrowth, aes(x=supp,y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = 'boxplot')
qplot(interaction(supp,dose), len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()
