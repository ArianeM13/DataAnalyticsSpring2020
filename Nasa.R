#BINARY (LOGISTIC) REGRESSION

Precip_Binary <- read.csv(file.choose(), header = T)
summary(Precip_Binary)
head(Precip_Binary)
sapply(Precip_Binary,sd)


#Exploratory Data Analysis
#Boxplot

#Histogram
par(mfrow = c(1,2))
h1 <- hist(Precip_Binary$Precip_avg.mm.,main = 'Precipitation', xlab = 'Precipitation Average (mm)',col = 'light gray')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))
h2 <- hist(Precip_Binary$Date_bin, main = 'Dust',xlab = 'Presence of dust', xlim=c(0,1), breaks = 2, xaxt = 'n', col = 'light green')
axis(side=1, at=seq(0,1,1), labels=seq(0,1,1))
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))

boxplot(Precip_Binary$Precip_avg.mm., ylab = 'Precipitaion avg (mm)', col="light blue")

#Scatterplot
plot(Precip_Binary$Precip_avg.mm.,Precip_Binary$Date_bin, main = "Presence of Dust vs Precipitation Average",
     xlab = "Precipitaion Average (mm)", ylab = "Presence of Dust",
     ylim = c(0,1), pch = 19)

#Model
BRmodel <- glm(Date_bin ~ Precip_avg.mm., data = Precip_Binary, family = "binomial")
summary(BRmodel)
with(BRmodel, null.deviance - deviance)
with(BRmodel, df.null - df.residual)


range(Precip_Binary$Precip_avg.mm.)
xPrecip_avg <-seq (0, 35, 0.1)
yPrecip_avg <- predict(BRmodel, list(Precip_avg.mm.=xPrecip_avg),type="response")
plot(Date_bin ~ Precip_avg.mm., data = Precip_Binary, pch = 16, 
     xlab = 'Precipitaion Average (mm)', ylab = 'Presence of Dust' )
lines(xPrecip_avg, yPrecip_avg, col = "green", lwd = 2)


#PRECIPITAION CLASSIFICATION
#EDA
Precip_days <- read.csv(file.choose(), header = T)
Precip_num <- read.csv(file.choose(), header = T)
Day_before <- read.csv(file.choose(), header = T)
Day_of <- read.csv(file.choose(), header = T)
Day_after1 <- read.csv(file.choose(), header = T)
Day_after2 <- read.csv(file.choose(), header = T)

str(Precip_days) #inital levels for days are not what I want
head(Precip_days)

#Removing rows with no "day" data that is day of, day before ...
Precip_days1 <- Precip_days[!grepl("-", Precip_days$Days),] 
Precip_num1 <- Precip_num[!grepl("-", Precip_num$Days),] 


#Histograms
par(mfrow = c(2,2))
h1 <- hist(Day_before$Precip_avg.mm.,main = 'Precipitation Day Before', xlab = 'Precipitation Average (mm)',col = 'light pink')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))

h1 <- hist(Day_of$Precip_avg.mm.,main = 'Precipitation Day of', xlab = 'Precipitation Average (mm)',col = 'red')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))

h1 <- hist(Day_after1$Precip_avg.mm.,main = 'Precipitation 1 Day After', xlab = 'Precipitation Average (mm)',col = 'light pink')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))

h1 <- hist(Day_after2$Precip_avg.mm.,main = 'Precipitation 2 Days After', xlab = 'Precipitation Average (mm)',col = 'light pink')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))
summary(h1)

#Boxplot
boxplot(Day_before$Precip_avg.mm., ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation Day Before')

boxplot(Day_of$Precip_avg.mm., ylab = 'Precipitaion avg (mm)', col="light green")
title('Precipitation Day of')

boxplot(Day_after1$Precip_avg.mm., ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation 1 Day After')

boxplot(Day_after2$Precip_avg.mm., ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation 2 Days After')

boxplot(Precip_avg.mm.~Days, data =Precip_days1,ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation Averages for all days')

#changing structure of factor levels
str(Precip_days1$Days)
Precip_days2 <- Precip_days1
#Precip_days2$Days <- relevel(Precip_days2$Days, ref = c('day_before','day_of','1day_after','2days_after'))
Precip_days2$Days <- droplevels(Precip_days2$Days)
str(Precip_days2$Days)
levels(Precip_days2$Days) <- c('day_before','day_of','1day_after','2days_after')

boxplot(Precip_avg.mm.~Days, data =Precip_days2,ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation Averages for all days restructured')

#Scatter plot
plot(Precip_num1$Precip_avg.mm.,Precip_num1$Day_num, main = "Days vs Precipitation Average",
     xlab = "Precipitaion Average (mm)", ylab = "Days",
     pch = 19)

plot(Precip_num1$Day_num,Precip_num1$Precip_avg.mm., main = "Days vs Precipitation Average",
     xlab = "Precipitaion Average (mm)", ylab = "Days",
     pch = 19)

plot(Precip_num1$Day_num,Precip_num1$Precip_avg.mm.,  main = "Days vs Precipitation Average",
     xlab = "Precipitaion Average (mm)", ylab = "Days",
     pch = 19)

#Classification of Precipitaion
str(Precip_days2$Precip_avg.mm.)
Precip_days2$Precip_class <- cut(Precip_days2$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class <- as.factor(Precip_days2$Precip_class)
summary(Precip_days2$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Precip_days2)

class.freq <- table(Precip_days2$Precip_class)
h1 <- barplot(class.freq, ylim=c(0, max(class.freq) + 15), main = 'Precipitation Classification', col = 'orange')
text(h1,class.freq+1, class.freq, adj=c(0.5, -0.5))

#Decision Tree
library(rpart)
library(rpart.plot)
 
str(Precip_days2)
Precip_days2$Precip_class <- as.factor(Precip_days2$Precip_class)
Precip_days2$Days <- as.factor(Precip_days2$Days)
Precip_days2$Date_bin <- as.factor(Precip_days2$Date_bin)

PrecipTree1 <- rpart(Precip_class~Days, data = Precip_days2 , method = 'class')
summary(PrecipTree1)
printcp(PrecipTree1)
rpart.plot(PrecipTree1, extra= 106)
rpart.plot(PrecipTree1)

PrecipTree1.1 <- rpart(Precip_class~Days, data = Precip_days2, cp=0.1)
summary(PrecipTree1.1)
printcp(PrecipTree1.1)
rpart.plot(PrecipTree1.1, extra= 106)
rpart.plot(PrecipTree1.1)


PrecipTree2 <- rpart(Days ~ Precip_class, data = Precip_days2 , method = 'class', minsplit = 1)
summary(PrecipTree2)
printcp(PrecipTree2)
rpart.plot(PrecipTree2, extra= 106)
rpart.plot(PrecipTree2)


PrecipTree3 <- rpart(Precip_class ~ Date_bin, data = Precip_days2 , method = 'class')
summary(PrecipTree3)
printcp(PrecipTree2)
rpart.plot(PrecipTree3, extra= 106)
rpart.plot(PrecipTree2)

PrecipTree4 <- rpart(Date_bin~Precip_class, data = Precip_days2 , method = 'class', minsplit=2, minbucket=1)
summary(PrecipTree4)
printcp(PrecipTree4)
rpart.plot(PrecipTree4, extra= 106)
rpart.plot(PrecipTree4)

#Trying with ctree
library(party)

tree <- ctree(Precip_class~Days, data = Precip_days2)
plot(tree) #kind of works but splits into two

tree <- ctree(Days ~ Precip_class, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class ~ Date_bin, data = Precip_days2)
plot(tree) #This works

tree <- ctree(Date_bin~Precip_class, data = Precip_days2)
plot(tree)

#Trying with days as numbers
str(Precip_num1)
Precip_num1$Day_num <- droplevels(Precip_num1$Day_num)
str(Precip_num1$Day_num)
levels(Precip_num1$Day_num) <- c('-1','0','1','2')
str(Precip_num1)

str(Precip_num1$Precip_avg.mm.)
Precip_num1$Precip_class <- cut(Precip_num1$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_num1$Precip_class <- as.factor(Precip_num1$Precip_class)
summary(Precip_num1$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Precip_num1)

tree <- ctree(Precip_class~Day_num, data = Precip_num1)
plot(tree) #same as above

tree <- ctree(Precip_avg.mm.~Day_num, data = Precip_num1)
plot(tree) 

tree <- ctree(Precip_class~Day_num +Precip_avg.mm., data = Precip_num1)
plot(tree) #only split based on avg

#Classification 2
str(Precip_days2$Precip_avg.mm.)
Precip_days2$Precip_class2 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,5,10,20,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class2 <- as.factor(Precip_days2$Precip_class2)
summary(Precip_days2$Precip_class2) # Light-315, Mod-9,Heavy-10,Very H-4
head(Precip_days2) 

class.freq2 <- table(Precip_days2$Precip_class)
h1 <- barplot(class.freq, ylim=c(0, max(class.freq2) + 15), main = 'Precipitation Classification', col = 'orange')
text(h1,class.freq2+1, class.freq2, adj=c(0.5, -0.5))


