#Creating Data Frame - A data frame is a table or a two-dimensional array-like structure in which each column contains values of one variable and each row contains one set of values from each column. 
#RPI Weather dataframe

days <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun') #c() is used for combine(concatenate) to make vectors in R ie put multiple values into a singke variable
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) #Temp in F during winter
snowed <- c('T','T','F','F','T','T','F') #Snowed on that day: T=True, F=False
help('data.frame')

RPI_Weather_Week <- data.frame(days,temp,snowed) #creating the data frame
RPI_Weather_Week

#to transpose use t dataframe
RPI_Weather_Week_Flipped<- data.frame(t(RPI_Weather_Week[-1]))
colnames(RPI_Weather_Week_Flipped) <- RPI_Weather_Week[, 1]
RPI_Weather_Week_Flipped

head(RPI_Weather_Week) #shows first 6 rows of data frame
str(RPI_Weather_Week) #structure of dataframe
summary(RPI_Weather_Week)
RPI_Weather_Week[-1] #takes out first column
RPI_Weather_Week[-1,] #takes out first row
RPI_Weather_Week[1] #shows 1st column and all the rows in list
RPI_Weather_Week[,1] #shows the contents of 1st column in a row
RPI_Weather_Week[1,] #shows first row
RPI_Weather_Week[,'snowed'] 
RPI_Weather_Week['days'] #shows column as list
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')] #rows 1 to 5 with days and temp columns
RPI_Weather_Week$temp #another way to show what is in temp column
subset(RPI_Weather_Week,subset=snowed==TRUE) #forlater

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
help(order)
RPI_Weather_Week[sorted.snowed,]

#RPI_Weather_Week[descending_snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
RPI_Weather_Week[dec.snow,]

#Creating an empty dataframe - why?
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2) #col.name.1 is just an arbitrary name you choose based on what you want
df

#importing data and exporting data
#writing to a CSV file:
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

#EPI Data is already downloaded 
#two ways EPI_data <- read.csv(”<path>/2010EPI_data.csv") or EPIdata <- read.csv(file.choose(), header = T)
#code last time used to download last time: EPIdata2010 <- read.csv(file.choose(), skip=1, header = T)
View(EPIdata2010)
attach(EPIdata2010)
fix(EPIdata2010)
EPIdata2010$EPI
tf <- is.na(EPI) #records True values if the value is NA
tf
E <- EPI[!tf] #filters out NA values, new array
E
summary(EPI)
fivenum(EPI,na.rm=TRUE) #na.rm removes all the NA
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0), prob=TRUE) #probablity density = true, seq- interval?, bw= bin width
lines(density(EPI,na.rm=TRUE,bw=1.))
lines(density(EPI,na.rm=TRUE,bw='SJ'))
rug(EPI)

plot(ecdf(EPI),do.points=T, verticals=F)
plot(ecdf(EPI),do.points=F, verticals=F)
plot(ecdf(EPI),do.points=F, verticals=T)

help(ecdf)

par(pty='s')
help(par)

qqnorm(EPI)
qqline(EPI)

x<- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for t dsn')
qqline(x)

plot(ecdf(DALY),do.points=F, verticals=T)
par(pty='s')
qqnorm(DALY)
qqline(DALY)

boxplot(EPI,DALY, names=c('EPI','DALY'))
qqplot(EPI,DALY)

boxplot(EPI,DALY,ENVHEALTH,ECOSYSTEM,AIR_H,WATER_H,AIR_E,WATER_E, names=c('EPI','DALY','ENVHEALTH','ECOSYSTEM','AIR_H','WATER_H','AIR_E','WATER_E'))

EPILand<-EPI[!Landlock]
ELand<- EPILand[!is.na(EPILand)]
ELand 

hist(ELand)
hist(ELand,seq(30.,95.,1.0), prob=T)
lines(density(ELand,na.rm=TRUE,bw=1.))
lines(density(ELand,na.rm=TRUE,bw='SJ'))
rug(ELand)

plot(ecdf(ELand),do.points=F, verticals=T)
par(pty='s')
qqnorm(ELand)
qqline(ELand)
