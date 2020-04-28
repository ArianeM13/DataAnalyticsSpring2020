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

#changing structure of factor levels - Messed up data
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

#binary
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

class.freq2 <- table(Precip_days2$Precip_class2)
h1 <- barplot(class.freq2, ylim=c(0, max(class.freq2) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq2+1, class.freq2, adj=c(0.5, -0.5))

tree <- ctree(Precip_class2~Days, data = Precip_days2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class2 ~ Date_bin, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class2~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 3
Precip_days2$Precip_class3 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,5,10,100), labels= c('Light','Moderate','Heavy'))
Precip_days2$Precip_class3 <- as.factor(Precip_days2$Precip_class3)
summary(Precip_days2$Precip_class3) # Light-315, Mod-9,Heavy-14
head(Precip_days2) 

class.freq3 <- table(Precip_days2$Precip_class3)
h1 <- barplot(class.freq3, ylim=c(0, max(class.freq3) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq3+1, class.freq3, adj=c(0.5, -0.5))

tree <- ctree(Precip_class3~Days, data = Precip_days2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class3 ~ Date_bin, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class3~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 4
Precip_days2$Precip_class4 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,1,5,100), labels= c('Light','Moderate','Heavy'))
Precip_days2$Precip_class4 <- as.factor(Precip_days2$Precip_class4)
summary(Precip_days2$Precip_class4) # Light-257, Mod-58,Heavy-23

class.freq4 <- table(Precip_days2$Precip_class4)
h1 <- barplot(class.freq4, ylim=c(0, max(class.freq4) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq4+1, class.freq4, adj=c(0.5, -0.5))

tree <- ctree(Precip_class4~Days, data = Precip_days2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class4 ~ Date_bin, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class4~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 5
Precip_days2$Precip_class5 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,10,20,100), labels= c('Light','Moderate','Heavy'))
Precip_days2$Precip_class5 <- as.factor(Precip_days2$Precip_class5)
summary(Precip_days2$Precip_class5) # Light-324, Mod-10,Heavy-4
head(Precip_days2) 

class.freq5 <- table(Precip_days2$Precip_class5)
h1 <- barplot(class.freq5, ylim=c(0, max(class.freq5) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq5+1, class.freq5, adj=c(0.5, -0.5))

tree <- ctree(Precip_class5~Days, data = Precip_days2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class5 ~ Date_bin, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class5~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 6
str(Precip_days2$Precip_avg.mm.)
Precip_days2$Precip_class6 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,0.5,1,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class6 <- as.factor(Precip_days2$Precip_class6)
summary(Precip_days2$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Precip_days2) 

class.freq6 <- table(Precip_days2$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Precip_days2)
plot(tree)


#binary
tree <- ctree(Precip_class6 ~ Date_bin, data = Precip_days2)
plot(tree)


tree <- ctree(Precip_class6~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 7
str(Precip_days2$Precip_avg.mm.)
Precip_days2$Precip_class7 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,0.5,1,5,10,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class7 <- as.factor(Precip_days2$Precip_class7)
summary(Precip_days2$Precip_class7) # Trace-224   Light-33, Mod-58,Heavy-9,Very H-14
head(Precip_days7) 

class.freq7 <- table(Precip_days2$Precip_class7)
h1 <- barplot(class.freq7, ylim=c(0, max(class.freq7) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq7+1, class.freq7, adj=c(0.5, -0.5))

tree <- ctree(Precip_class7~Days, data = Precip_days2)
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class7 ~ Date_bin, data = Precip_days2)
plot(tree)

tree <- ctree(Precip_class7~Precip_avg.mm., data = Precip_days2)
plot(tree)

#Classification 6 is best so far - doing variation - Classification 8
str(Precip_days2$Precip_avg.mm.)
Precip_days2$Precip_class8 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,0.5,5,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class8 <- as.factor(Precip_days2$Precip_class8)
summary(Precip_days2$Precip_class8) #Trace-224   Light-91, Mod-9,Heavy-10,Very H-4
head(Precip_days2) 

class.freq8 <- table(Precip_days2$Precip_class8)
h1 <- barplot(class.freq8, ylim=c(0, max(class.freq8) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq8+1, class.freq8, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Precip_days2)
plot(tree)


#binary
tree <- ctree(Precip_class8 ~ Date_bin, data = Precip_days2)
plot(tree)


tree <- ctree(Precip_class8~Precip_avg.mm., data = Precip_days2)
plot(tree)

#other variation - Classification 9
Precip_days2$Precip_class9 <- cut(Precip_days2$Precip_avg.mm., br = c(-1,0.5,1,5,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days2$Precip_class9 <- as.factor(Precip_days2$Precip_class9)
summary(Precip_days2$Precip_class9) #Trace-224   Light-33, Mod-58,Heavy-19,Very H-4
head(Precip_days2) 

class.freq9 <- table(Precip_days2$Precip_class9)
h1 <- barplot(class.freq9, ylim=c(0, max(class.freq9) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq9+1, class.freq9, adj=c(0.5, -0.5))

tree <- ctree(Precip_class9~Days, data = Precip_days2)
plot(tree)


#binary
tree <- ctree(Precip_class9 ~ Date_bin, data = Precip_days2)
plot(tree)


tree <- ctree(Precip_class9~Precip_avg.mm., data = Precip_days2)
plot(tree)

plot(Precip_avg.mm.~Days, data = Precip_days2 )

head(Precip_days)
head(Precip_days2)
head(Precip_days1)

#releveling
Precip_days3 <- Precip_days1
#Precip_days2$Days <- relevel(Precip_days2$Days, ref = c('day_before','day_of','1day_after','2days_after'))
Precip_days3$Days <- droplevels(Precip_days3$Days)
str(Precip_days2$Days)
#levels(Precip_days2$Days) <- c('day_before','day_of','1day_after','2days_after')

boxplot(Precip_avg.mm.~Days, data =Precip_days3,ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation Averages for all days')

#Class 1
Precip_days3$Precip_class <- cut(Precip_days3$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class <- as.factor(Precip_days3$Precip_class)
summary(Precip_days3$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Precip_days3)

class.freq <- table(Precip_days3$Precip_class)
h1 <- barplot(class.freq, ylim=c(0, max(class.freq) + 15), main = 'Precipitation Classification', col = 'orange')
text(h1,class.freq+1, class.freq, adj=c(0.5, -0.5))

tree <- ctree(Precip_class~Days, data = Precip_days3)
plot(tree)

#binary
tree <- ctree(Precip_class ~ Date_bin, data = Precip_days3)
plot(tree)

#Classification 2
str(Precip_days3$Precip_avg.mm.)
Precip_days3$Precip_class2 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,5,10,20,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class2 <- as.factor(Precip_days3$Precip_class2)
summary(Precip_days3$Precip_class2) # Light-315, Mod-9,Heavy-10,Very H-4
head(Precip_days3) 

class.freq2 <- table(Precip_days3$Precip_class2)
h1 <- barplot(class.freq2, ylim=c(0, max(class.freq2) + 15), main = 'Precipitation Classification 2', col = 'red')
text(h1,class.freq2+1, class.freq2, adj=c(0.5, -0.5))

tree <- ctree(Precip_class2~Days, data = Precip_days3)
plot(tree)

#binary
tree <- ctree(Precip_class2 ~ Date_bin, data = Precip_days3)
plot(tree)

#Classification 6
Precip_days3$Precip_class6 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.5,1,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class6 <- as.factor(Precip_days3$Precip_class6)
summary(Precip_days3$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Precip_days3) 

class.freq6 <- table(Precip_days3$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Precipitation Classification', col = 'purple')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Precip_days3)
plot(tree)


#binary
tree <- ctree(Precip_class6 ~ Date_bin, data = Precip_days3)
plot(tree)

#Classification 10
Precip_days3$Precip_class10 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.1,0.5,1,10,20,100), labels= c('Trace','Very Light' ,'Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class10 <- as.factor(Precip_days3$Precip_class10)
summary(Precip_days3$Precip_class10) #Trace-132, VL-92   Light-33, Mod-67,Heavy-10,Very H-4
head(Precip_days3) 

class.freq10 <- table(Precip_days3$Precip_class10)
h1 <- barplot(class.freq10, ylim=c(0, max(class.freq10) + 15), main = 'Precipitation Classification 10', col = 'blue')
text(h1,class.freq10+1, class.freq10, adj=c(0.5, -0.5))

tree <- ctree(Precip_class10~Days, data = Precip_days3)
plot(tree)


#binary
tree <- ctree(Precip_class10 ~ Date_bin, data = Precip_days3)
plot(tree)

#Classification11
Precip_days3$Precip_class6 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.5,1,5,10,20,100), labels= c('Trace','Very Light', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class6 <- as.factor(Precip_days3$Precip_class6)
summary(Precip_days3$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Precip_days3) 

class.freq6 <- table(Precip_days3$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Precipitation Classification 6', col = 'purple')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Precip_days3)
plot(tree)


#binary
tree <- ctree(Precip_class6 ~ Date_bin, data = Precip_days3)
plot(tree)

Head(precip)