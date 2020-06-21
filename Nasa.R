#BINARY (LOGISTIC) REGRESSION

Precip_Binary <- read.csv(file.choose(), header = T)
summary(Precip_Binary)
head(Precip_Binary)
sapply(Precip_Binary,sd)


#Exploratory Data Analysis
#Boxplot

#Histogram
par(mfrow = c(1,2))
h1 <- hist(Precip_Binary$Precip_avg.mm.,main = 'Precipitation', breaks = 20, xlab = 'Precipitation Average (mm)',col = 'light gray')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))
h2 <- hist(Precip_Binary$Date_bin, main = 'Dust',xlab = 'Presence of dust', xlim=c(0,1), breaks = 2, xaxt = 'n', col = 'light green')
axis(side=1, at=seq(0,1,1), labels=seq(0,1,1))
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))

boxplot(Precip_Binary$Precip_avg.mm.,ylab = 'Precipitaion avg (mm)', col="light blue")
par(mfrow = c(3,1))

bar <- ggplot(Precip_Binary, aes(x=Date_bin)) + geom_bar(fill = 'steelblue3') +
        theme_minimal()+
        geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
        labs(x = "Presence of Dust")
bar

hist <- ggplot(Precip_Binary, aes(x=Precip_avg.mm.)) + geom_histogram(col= 'darkorchid4' ,fill = 'orchid3') +
        theme_minimal()+
        stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
        labs(x="Precipitaion Average (mm)")
hist

bp <- ggplot(Precip_Binary, aes(x ='', y=Precip_avg.mm.)) + 
        geom_boxplot (fill="#69b3a2")+
        theme_minimal()+
        labs(y="Precipitaion Average (mm)")
bpr <- bp + coord_flip()
bpr

grid.arrange(bar, hist, bpr, 
          ncol = 1, nrow = 3,
          heights =c (1.1,1.1,.5))
        

#Scatterplot
plot(Precip_Binary$Precip_avg.mm.,Precip_Binary$Date_bin, main = "Presence of Dust vs Precipitation Average",
     xlab = "Precipitaion Average (mm)", ylab = "Presence of Dust",
     ylim = c(0,1), pch = 19)

ggplot(Precip_Binary, aes(x=Precip_avg.mm., y=Date_bin)) +
        geom_point(col= 'indianred4')+
        theme_minimal()+
        labs(x="Precipitaion Average (mm)", y = "Presence of Dust")
        

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

head(Precip_Binary)
#Model using regression splitting data
set.seed(1000)
#Creating training set of 70% of data
ran <- sample(1:nrow(Precip_Binary), 0.7 * nrow(Precip_Binary))
APrecip_train <- Precip_Binary[ran,] 
head(APrecip_train)

#Creating test set of 30% of data
APrecip_test <- Precip_Binary[-ran,]
head(APrecip_test)

BRmodel2 <- glm(Date_bin ~ Precip_avg.mm., data = APrecip_train , family = "binomial")
summary(BRmodel2)
head(BRmodel2)

#res plot
BR.res <- resid(BRmodel2)
plot(APrecip_train$Date_bin, BR.res, main ='Residual Plot with resid')
plot(BRmodel2$residuals, main = 'Residual Plot')

#Prediction
RegPred <- predict(BRmodel2, APrecip_test,type="response")
actuals_preds <- data.frame(cbind(actuals=APrecip_test$Date_bin, predicteds=RegPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy #8.765833e-05
head(actuals_preds)
plot(actuals_preds, ,main = 'Plot of Actuals vs Precdicted')

#Min_max Accuracy - Higher the better
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy #0.09331581

#MAPE (Mean absolute percentage error) - lower the better
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
mape #wqinf

#model plot
plot(Date_bin ~ Precip_avg.mm., data = APrecip_test, pch = 16, 
     xlab = 'Precipitaion Average (mm)', ylab = 'Presence of Dust' )
#lines(BRmodel2$model, col = "green", lwd = 2)
Precip_avg.mm. <- data.frame(APrecip_test$Precip_avg.mm.)
head(Precip_avg.mm.)
curve(predict(BRmodel2, data.frame(Precip_avg.mm.= x),type="response"),add=TRUE)

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

#ggplot
box1  <- ggplot(Precip_days3, aes(x=Days, y=Precip_avg.mm., color=Days)) +
        geom_boxplot()+
        theme_minimal()+
        labs(y="Precipitaion Average (mm)")
boxf<-box1+ coord_flip()

hist1 <- ggplot(Precip_days3, aes(x=Precip_avg.mm., fill = Days)) + 
        geom_histogram() +
        theme_minimal()+
        labs(x="Precipitaion Average (mm)")
histf<-hist1+ coord_flip()


grid.arrange(hist1,boxf, 
             ncol = 1, nrow = 2)
             #heights =c (1.1,1.1,.5))
             
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
h1 <- barplot(class.freq, ylim=c(0, max(class.freq) + 15), main = 'Precipitation Classification', col = 'light green')
text(h1,class.freq+1, class.freq, adj=c(0.5, -0.5))

h1 <- barplot(class.freq, horiz=TRUE, main = 'Precipitation Classification', col = 'orange')
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

#releveling - CORRECTED
Precip_days3 <- Precip_days1
#Precip_days2$Days <- relevel(Precip_days2$Days, ref = c('day_before','day_of','1day_after','2days_after'))
Precip_days3$Days <- droplevels(Precip_days3$Days)
str(Precip_days2$Days)
str(Precip_days3$Days)

#levels(Precip_days2$Days) <- c('day_before','day_of','1day_after','2days_after')

boxplot(Precip_avg.mm.~Days, data =Precip_days3, horizontal = T, ylab = 'Precipitaion avg (mm)', col="light blue")
title('Precipitation Averages for all days')

#Class 1 - THIS
Precip_days3$Precip_class <- cut(Precip_days3$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class <- as.factor(Precip_days3$Precip_class)
summary(Precip_days3$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Precip_days3)

class.freq <- table(Precip_days3$Precip_class)
h1 <- barplot(class.freq, ylim=c(0, max(class.freq) + 15), main = 'Precipitation Classification', col = 'light blue')
text(h1,class.freq+1, class.freq, adj=c(0.5, -0.5))

ggplot(Precip_days3, aes(x=Precip_class)) + 
        geom_bar(stat = "count" , fill = "skyblue3") +
        theme_minimal()+
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        labs(x="Precipitaion Class")

#Correlation Matrix
PrecipCorr1 <- data.frame(PrecipClass = as.numeric(Precip_days3$Precip_class),
                          PrecipBin = as.numeric(Precip_days3$Date_bin),
                          PrecipDays = as.numeric(Precip_days3$Days))

head(PrecipCorr1)
# library(lsr)
# PrecipClass <-data.frame(Precip_days3$Precip_class)
# PrecipBin <- data.frame(Precip_days3$Date_bin)
# correlate(PrecipClass, PrecipBin)
# 
# PrecipCorr1$PrecipClassNum <- cut(Precip_days3$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('1','2','2','7'))
# PrecipCorr1$PrecipClassNum <- as.numeric(PrecipCorr1$PrecipClassNum)
# head(PrecipCorr1)
# 
# cor(PrecipCorr1$PrecipDays, PrecipCorr1$PrecipClassNum)
# cor(PrecipCorr1$PrecipClassNum, PrecipCorr1$PrecipClass)
datacor <- cor(PrecipCorr1)
datacor
library(corrplot)
corrplot(datacor)
plot(PrecipCorr1$PrecipClass,PrecipCorr1$PrecipDays)

library(ggplot2)
library(gridExtra)

# ggplot(Precip_days3) +
#         aes(x = Precip_class, fill = Days) +
#         geom_bar() +
#         scale_fill_hue() +
#         theme_minimal()

# ggplot(Precip_days3) +
#         aes(x = Date_bin, fill = Precip_class) +
#         geom_bar() +
#         scale_fill_hue() +
#         theme_minimal()+
#         annotate(geom = "Corr1")


Corr1 <- table(Precip_days3$Precip_class,Precip_days3$Date_bin)
Corr1
plot(Corr1, main= 'Days(Binary) vs Preciptation Class')
preciplot <- ggplot(Precip_days3) +
        aes(x = Date_bin, fill = Precip_class) +
        geom_bar() +
        scale_fill_brewer(palette="Set2")+
        theme_minimal()+
        labs(fill = "Precip Class")
        #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot
tt <- ttheme_default(core = list(padding=unit(c(20,4), "mm")))
Corrtab <- tableGrob(Corr1, theme=tt)
grid.arrange(preciplot, Corrtab,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

chisq.test(Precip_days3$Precip_class,Precip_days3$Date_bin)
chisq.test(Corr1)
#cramersV(Precip_days3$Precip_class,Precip_days3$Date_bin)
fisher.test(Precip_days3$Precip_class,Precip_days3$Date_bin)
fisher.test(Corr1)

Corr2 <- table(Precip_days3$Precip_class,Precip_days3$Days)
Corr2
plot(Corr2, col('light blue'))
preciplot2 <- ggplot(Precip_days3) +
        aes(x = Days, fill = Precip_class) +
        geom_bar() +
        scale_fill_brewer(palette="Set2")+
        theme_minimal()+
        labs(fill = "Precip Class")
        #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
    
preciplot2
tt <- ttheme_default()
Corrtab2 <- tableGrob(Corr2, theme=tt)
grid.arrange(preciplot2, Corrtab2,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

chisq.test(Precip_days3$Precip_class,Precip_days3$Days)
chisq.test(Corr2)
#cramersV(Precip_days3$Precip_class,Precip_days3$Days)
fisher.test(Precip_days3$Precip_class,Precip_days3$Days,workspace=2e8)
fisher.test(Corr2,workspace=2e8)
fisher.test(Corr2,simulate.p.value=TRUE,B=1e7)

#Decision Tree
tree <- ctree(Precip_class~Days, data = Precip_days3)
plot(tree)
par(mfrow = c(1,2))
#binary
tree <- ctree(Precip_class ~ Date_bin, data = Precip_days3)
plot(tree)

# Classification 2
# str(Precip_days3$Precip_avg.mm.)
# Precip_days3$Precip_class2 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,5,10,20,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
# Precip_days3$Precip_class2 <- as.factor(Precip_days3$Precip_class2)
# summary(Precip_days3$Precip_class2) # Light-315, Mod-9,Heavy-10,Very H-4
# head(Precip_days3) 
# 
# class.freq2 <- table(Precip_days3$Precip_class2)
# h1 <- barplot(class.freq2, ylim=c(0, max(class.freq2) + 15), main = 'Precipitation Classification 2', col = 'red')
# text(h1,class.freq2+1, class.freq2, adj=c(0.5, -0.5))
# 
# tree <- ctree(Precip_class2~Days, data = Precip_days3)
# plot(tree)
# 
# #binary
# tree <- ctree(Precip_class2 ~ Date_bin, data = Precip_days3)
# plot(tree)

#Classification 6 - THIS
Precip_days3$Precip_class6 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.5,1,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Precip_days3$Precip_class6 <- as.factor(Precip_days3$Precip_class6)
summary(Precip_days3$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Precip_days3) 

class.freq6 <- table(Precip_days3$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Precipitation Classification', col = 'purple')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

ggplot(Precip_days3, aes(x=Precip_class6)) + 
        geom_bar(stat = "count" , fill = "aquamarine3") +
        theme_minimal() +
        geom_text(stat='count', aes(label=..count..), vjust=-1)+
        labs(x="Expanded Precipitaion Class")

#Correlation Matrix
# Corr3 <- table(Precip_days3$Precip_class6,Precip_days3$Date_bin)
# Corr3
# plot(Corr3, main= 'Days(Binary) vs Expanded Precip Class')
# chisq.test(Precip_days3$Precip_class6,Precip_days3$Date_bin)
# cramersV(Precip_days3$Precip_class6,Precip_days3$Date_bin)
# 
# 
# Corr4 <- table(Precip_days3$Precip_class6,Precip_days3$Days)
# Corr4
# plot(Corr4, col('light blue'))
# chisq.test(Precip_days3$Precip_class6,Precip_days3$Days)
# cramersV(Precip_days3$Precip_class6,Precip_days3$Days)

# ggplot(Precip_days3) +
#         aes(x = Date_bin, fill = Precip_class6) +
#         geom_bar() +
#         scale_fill_hue() +
#         theme_minimal()+
#         annotate(geom = "Corr3")


Corr3 <- table(Precip_days3$Precip_class6,Precip_days3$Date_bin)
Corr3
#plot(Corr3, main= 'Days(Binary) vs Expanded Preciptation Class')
preciplot3 <- ggplot(Precip_days3) +
        aes(x = Date_bin, fill = Precip_class6) +
        geom_bar() +
        scale_fill_brewer(palette="Dark2")+
        theme_minimal()+
        labs(fill = "Precip Class")
        #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))

preciplot3
tt <- ttheme_default(core = list(padding=unit(c(20,4), "mm")))
Corrtab3 <- tableGrob(Corr3, theme=tt)
grid.arrange(preciplot3, Corrtab3,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

chisq.test(Precip_days3$Precip_class6,Precip_days3$Date_bin)
chisq.test(Corr3)
#cramersV(Precip_days3$Precip_class,Precip_days3$Date_bin)
fisher.test(Precip_days3$Precip_class6,Precip_days3$Date_bin)
fisher.test(Corr3)

Corr4 <- table(Precip_days3$Precip_class6,Precip_days3$Days)
Corr4
#plot(Corr4, col('light blue'))
preciplot4 <- ggplot(Precip_days3) +
        aes(x = Days, fill = Precip_class6) +
        geom_bar() +
        scale_fill_brewer(palette="Dark2")+
        theme_minimal()+
        labs(fill = "Precip Class")
        #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot4
tt <- ttheme_default()
Corrtab4 <- tableGrob(Corr4, theme=tt)
grid.arrange(preciplot4, Corrtab4,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

chisq.test(Precip_days3$Precip_class6,Precip_days3$Days)
chisq.test(Corr4)
#cramersV(Precip_days3$Precip_class,Precip_days3$Days)
fisher.test(Precip_days3$Precip_class6,Precip_days3$Days, simulate.p.value=TRUE,B=1e7)
fisher.test(Corr4, simulate.p.value=TRUE,B=1e7)


#Decision Tree
tree6 <- ctree(Precip_class6~Days, data = Precip_days3)
plot(tree6)

#binary
tree <- ctree(Precip_class6~Date_bin, data = Precip_days3)
plot(tree)


#Classification 10
# Precip_days3$Precip_class10 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.1,0.5,1,10,20,100), labels= c('Trace','Very Light' ,'Light','Moderate','Heavy','Very Heavy'))
# Precip_days3$Precip_class10 <- as.factor(Precip_days3$Precip_class10)
# summary(Precip_days3$Precip_class10) #Trace-132, VL-92   Light-33, Mod-67,Heavy-10,Very H-4
# head(Precip_days3) 
# 
# class.freq10 <- table(Precip_days3$Precip_class10)
# h1 <- barplot(class.freq10, ylim=c(0, max(class.freq10) + 15), main = 'Precipitation Classification 10', col = 'blue')
# text(h1,class.freq10+1, class.freq10, adj=c(0.5, -0.5))
# 
# tree <- ctree(Precip_class10~Days, data = Precip_days3)
# plot(tree) #Still splits into two
# 
# #binary
# tree <- ctree(Precip_class10 ~ Date_bin, data = Precip_days3)
# plot(tree)
# 
# #Classification11
# Precip_days3$Precip_class6 <- cut(Precip_days3$Precip_avg.mm., br = c(-1,0.5,1,5,10,20,100), labels= c('Trace','Very Light', 'Light','Moderate','Heavy','Very Heavy'))
# Precip_days3$Precip_class6 <- as.factor(Precip_days3$Precip_class6)
# summary(Precip_days3$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
# head(Precip_days3) 
# 
# class.freq6 <- table(Precip_days3$Precip_class6)
# h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Expanded Precipitation Classification', col = 'light blue')
# text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))
# 
# tree <- ctree(Precip_class6~Days, data = Precip_days3)
# plot(tree)
# 
# tree <- ctree(Days~Precip_class, data = Precip_days3)
# plot(tree)
# 
# #binary
# tree <- ctree(Precip_class6 ~ Date_bin, data = Precip_days3)
# plot(tree)
# 
# tree <- ctree(Date_bin ~ Precip_class6, data = Precip_days3)
# plot(tree)
# 
# tree <- ctree(Date_bin ~ Precip_class, data = Precip_days3)
# plot(tree)

#Barplot with classification 1 and 6
#Light-257, Mod-58,Heavy-9,Very H-14 -1
#Trace-224   Light-33, Mod-67,Heavy-10,Very H-4 - 6

Class <- matrix(c(0,257,58,9,14,224,33,67,10,4),ncol=5,byrow=TRUE)
colnames(Class) <- c("Trace","Light","Moderate", "Heavy", "Very Heavy")
rownames(Class) <- c("Class 1", "Class 6")
Class <- as.table(Class)
Class


h1 <- barplot(Class, ylim=c(0, max(Class) + 15), main="Precipitation Classification",
        col=c("light blue","light green"), beside = T)
text(h1,Class+1, Class, adj=c(0.5, -0.5))

legend("topright",c("Initial","Other"),
       fill = c("light blue","light green"))
#Prediction with  classification 1 and 6
head(Precip_days3)

set.seed(1000)
#Creating training set of 70% of data
rand <- sample(1:nrow(Precip_days3), 0.7 * nrow(Precip_days3))
Precip_train <- Precip_days3[rand,] 
head(Precip_train)

#Creating test set of 30% of data
Precip_test <- Precip_train[-rand,]
head(Precip_test)

tree_train1 <- ctree(Precip_class~Days, data = Precip_train)
plot(tree_train1)

tree_train6 <- ctree(Precip_class6~Days, data = Precip_train)
plot(tree_train6)

#making predictions with the trees

Precip_pred1 <- predict(tree_train1,Precip_test)
Precip_pred6 <- predict(tree_train6,Precip_test)

tab1 <- table(Precip_pred1,Precip_test$Precip_class)
tab1

tab6 <- table(Precip_pred6,Precip_test$Precip_class6)
tab6

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(tab1) #74.28571
accuracy(tab6)#64.28571

#model predictions of days based on precip
tree6 <- ctree(Days~Precip_class6, data = Precip_train)
plot(tree6)

tree1 <- ctree(Days~Precip_class, data = Precip_train)
plot(tree1)

Precip_pred1.1 <- predict(tree1,Precip_test)
Precip_pred6.1 <- predict(tree6,Precip_test)

tab1 <- table(Precip_pred1.1,Precip_test$Days)
tab1

tab6 <- table(Precip_pred6.1,Precip_test$Days)
tab6

accuracy(tab1) #32.85714
accuracy(tab6) #34.28571


#maing predictions with the binary trees

treeb1 <- ctree(Precip_class ~ Date_bin, data = Precip_train)
plot(treeb1)

treeb6 <- ctree(Precip_class6 ~ Date_bin, data = Precip_train)
plot(treeb6)

Precip_predb1 <- predict(treeb1,Precip_test)
Precip_predb6 <- predict(treeb6,Precip_test)

tabb1 <- table(Precip_predb1,Precip_test$Precip_class)
tabb1

tabb6 <- table(Precip_predb6,Precip_test$Precip_class6)
tabb6

accuracy(tabb1) #74.28571
accuracy(tabb6) #64.28571

#other way

treeb1.1 <- ctree(Date_bin~Precip_class, data = Precip_train)
plot(treeb1.1)

treeb6.1 <- ctree(Date_bin~Precip_class6, data = Precip_train)
plot(treeb6.1)

Precip_predb1.1 <- predict(treeb1.1,Precip_test)
Precip_predb6.1 <- predict(treeb6.1,Precip_test)

tabb1.1 <- table(Precip_predb1.1,Precip_test$Date_bin)
tabb1.1

tabb6.1 <- table(Precip_predb6.1,Precip_test$Date_bin)
tabb6.1

accuracy(tabb1.1) #45.71429
accuracy(tabb6.1) #45.71429


