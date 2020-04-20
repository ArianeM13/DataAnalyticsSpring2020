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


str(Precip_Binary)
set.seed(500)


