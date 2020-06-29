#INVESTIGATING THE RELATIONSHIP BETWEEN PRECIPITATION AND HLD

Precip_Binary <- read.csv(file.choose(), header = T)
summary(Precip_Binary)
head(Precip_Binary)
sapply(Precip_Binary,sd)
str(Precip_Binary)
Precip_Binary$Date_bin <- as.factor(Precip_Binary$Date_bin)
str(Precip_Binary)

Precip_days <- read.csv(file.choose(), header = T)
Precip_num <- read.csv(file.choose(), header = T)
Day_before <- read.csv(file.choose(), header = T)
Day_of <- read.csv(file.choose(), header = T)
Day_after1 <- read.csv(file.choose(), header = T)
Day_after2 <- read.csv(file.choose(), header = T)

head(Precip_days)
summary(Precip_days)
str(Precip_days) #inital levels for days are not what I want

#EDA
#Binary
library(ggplot2)
library(gridExtra)
bar <- ggplot(Precip_Binary, aes(x=Date_bin)) + geom_bar(fill = 'steelblue3') +
  theme_minimal()+
  #geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  labs(x = "Presence of Dust")
bar

hist <- ggplot(Precip_Binary, aes(x=Precip_avg.mm.)) + geom_histogram(col= 'darkorchid4' ,fill = 'orchid3') +
  theme_minimal()+
  #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
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

ggplot(Precip_Binary, aes(x=Precip_avg.mm., y=Date_bin)) +
  geom_point(col= 'indianred4')+
  theme_minimal()+
  labs(x="Precipitaion Average (mm)", y = "Presence of Dust")

#using jitter plot because data is packed together in split plot
#see distribution better
ggplot(Precip_Binary, aes(x=Precip_avg.mm., y=Date_bin, color =Date_bin)) +
  geom_jitter(alpha = 0.7,
              size = 1.5)+ #small  random number added to each point
  theme_minimal() +
  scale_color_manual(values=c("royalblue2", "tomato2"))+
  labs(x="Precipitaion Average (mm)", y = "Presence of Dust")+
  theme(legend.position = "none") 

#https://rkabacoff.github.io/datavis/Bivariate.html

#4-Day Data
#Removing rows with no "day" data that is day of, day before ..
Precip_days1 <- Precip_days[!grepl("-", Precip_days$Days),] 
head(Precip_days1)
str(Precip_days1) #still not the levels I want

#releveling
Preciplevel <- Precip_days1
Preciplevel$Days <- droplevels(Preciplevel$Days)
Preciplevel$Days <- factor(Preciplevel$Days, 
                           levels =c('day_before','day_of','1day_after',
                                     '2days_after'))
str(Precip_days1$Days)
summary(Precip_days1$Days)
str(Precip_days3$Days)
summary(Precip_days3$Days)
str(Preciplevel$Days)
summary(Preciplevel$Days)

Preciplevel$Date_bin <- as.factor(Preciplevel$Date_bin)
str(Preciplevel)

#Plots of Data
dp <- ggplot(Preciplevel, aes(x =Days, y=Precip_avg.mm., color =Days)) + 
  geom_boxplot ()+
  theme_minimal()+
  labs(y="Precipitaion Average (mm)")+
  labs(y="Precipitaion Average (mm)", x = "Days")+
  theme(legend.position = "none") 
dp
dpr <- dp + coord_flip()
dpr

ggplot(Preciplevel, aes(x=Precip_avg.mm.)) + geom_histogram(col= 'darkorchid4' ,fill = 'orchid3') +
  theme_minimal()+
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  labs(x="Precipitaion Average (mm)")

data <- data.frame(Precipavg = Preciplevel$Precip_avg.mm.,
                   Days = Preciplevel$Days)
head(data)

dhist <- ggplot( data,aes(x=Precipavg, fill=Days)) +
  geom_histogram(colour="white",alpha=0.5, position = 'identity') +
  #scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal() +
  labs(fill="", x='Precipitaion Average (mm)')+
  theme(legend.position = "top")
dhist
dhistr <- dhist + coord_flip()
dhistr

grid.arrange(dhistr,dpr,
             ncol = 1, nrow = 2,
             heights =c (1,.5))

ghist1 <- ggplot(Day_before,aes(x=Precip_avg.mm.)) +
  geom_histogram(color="red",fill = '#F8766D') +
  #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  theme_minimal() +
  labs(x='Precipitaion Average (mm)')+
  ggtitle("Day Before")
ghist1

ghist2 <- ggplot(Day_of,aes(x=Precip_avg.mm.)) +
  geom_histogram(color="springgreen4",fill = '#7CAE00') +
  #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  theme_minimal() +
  labs(x='Precipitaion Average (mm)')+
  ggtitle("Day of")
ghist2

ghist3 <- ggplot(Day_after1,aes(x=Precip_avg.mm.)) +
  geom_histogram(color="cyan4",fill = '#00BFC4') +
  #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  theme_minimal() +
  labs(x='Precipitaion Average (mm)')+
  ggtitle("1 Day After")
ghist3

ghist4 <- ggplot(Day_after2,aes(x=Precip_avg.mm.)) +
  geom_histogram(color='darkorchid3',fill = '#C77CFF') +
  #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
  theme_minimal() +
  labs(x='Precipitaion Average (mm)')+
  ggtitle("2 Days After")
ghist4

grid.arrange(dp,ghist1,ghist2,ghist3,ghist4,
            layout_matrix = rbind(c(1,1,1,1),c(2,3,4,5)))

#CLASSIFICATIONS - preliminary analysis
#Classification  1
Preciplevel2 <- Preciplevel
head(Preciplevel)
head(Preciplevel2)
Preciplevel2$Precip_class <- cut(Preciplevel$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class <- as.factor(Preciplevel$Precip_class)
summary(Preciplevel2$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Preciplevel2)

class.freq <- table(Preciplevel2$Precip_class)
h1 <- barplot(class.freq, ylim=c(0, max(class.freq) + 15), main = 'Precipitation Classification', col = 'light blue')
text(h1,class.freq+1, class.freq, adj=c(0.5, -0.5))

library(party)

tree <- ctree(Precip_class ~ Days, data = Preciplevel2)
plot(tree)

#binary
tree <- ctree(Precip_class~Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class~Precip_avg.mm., data = Preciplevel2)
plot(tree)

#Classification 2
Preciplevel2$Precip_class2 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,5,10,20,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class2 <- as.factor(Preciplevel2$Precip_class2)
summary(Preciplevel2$Precip_class2) # Light-315, Mod-9,Heavy-10,Very H-4
head(Preciplevel2) 

class.freq2 <- table(Preciplevel2$Precip_class2)
h1 <- barplot(class.freq2, ylim=c(0, max(class.freq2) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq2+1, class.freq2, adj=c(0.5, -0.5))

tree <- ctree(Precip_class2~Days, data = Preciplevel2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class2 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class2~Precip_avg.mm., data = Preciplevel2)
plot(tree) #spilt into 3 categories not 4

#Classification 3
Preciplevel2$Precip_class3 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,5,10,100), labels= c('Light','Moderate','Heavy'))
Preciplevel2$Precip_class3 <- as.factor(Preciplevel2$Precip_class3)
summary(Preciplevel2$Precip_class3) # Light-315, Mod-9,Heavy-14
head(Preciplevel2) 

class.freq3 <- table(Preciplevel2$Precip_class3)
h1 <- barplot(class.freq3, ylim=c(0, max(class.freq3) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq3+1, class.freq3, adj=c(0.5, -0.5))

tree <- ctree(Precip_class3~Days, data = Preciplevel2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class3 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class3~Precip_avg.mm., data = Preciplevel2)
plot(tree)

#Classification 4
Preciplevel2$Precip_class4 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,1,5,100), labels= c('Light','Moderate','Heavy'))
Preciplevel2$Precip_class4 <- as.factor(Preciplevel2$Precip_class4)
summary(Preciplevel2$Precip_class4) # Light-257, Mod-58,Heavy-23

class.freq4 <- table(Preciplevel2$Precip_class4)
h1 <- barplot(class.freq4, ylim=c(0, max(class.freq4) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq4+1, class.freq4, adj=c(0.5, -0.5))

tree <- ctree(Precip_class4~Days, data = Preciplevel2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class4 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class4~Precip_avg.mm., data = Preciplevel2)
plot(tree)

#Classification 5
Preciplevel2$Precip_class5 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,10,20,100), labels= c('Light','Moderate','Heavy'))
Preciplevel2$Precip_class5 <- as.factor(Preciplevel2$Precip_class5)
summary(Preciplevel2$Precip_class5) # Light-324, Mod-10,Heavy-4
head(Preciplevel2) 

class.freq5 <- table(Preciplevel2$Precip_class5)
h1 <- barplot(class.freq5, ylim=c(0, max(class.freq5) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq5+1, class.freq5, adj=c(0.5, -0.5))

tree <- ctree(Precip_class5~Days, data = Preciplevel2)
tree
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class5 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class5~Precip_avg.mm., data = Preciplevel2)
plot(tree)#does not split into 3 - splits into 2

#Classification 6
Preciplevel2$Precip_class6 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.5,1,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class6 <- as.factor(Preciplevel2$Precip_class6)
summary(Preciplevel2$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Preciplevel2) 

class.freq6 <- table(Preciplevel2$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Preciplevel2)
plot(tree)

#binary
tree <- ctree(Precip_class6 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class6~Precip_avg.mm., data = Preciplevel2)
plot(tree)#splits into 4

#Classification 7
Preciplevel2$Precip_class7 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.5,1,5,10,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class7 <- as.factor(Preciplevel2$Precip_class7)
summary(Preciplevel2$Precip_class7) # Trace-224   Light-33, Mod-58,Heavy-9,Very H-14
head(Preciplevel2) 

class.freq7 <- table(Preciplevel2$Precip_class7)
h1 <- barplot(class.freq7, ylim=c(0, max(class.freq7) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq7+1, class.freq7, adj=c(0.5, -0.5))

tree <- ctree(Precip_class7~Days, data = Preciplevel2)
plot(tree)
summary(tree)

#binary
tree <- ctree(Precip_class7 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class7~Precip_avg.mm., data = Preciplevel2)
plot(tree)

#Classification 6 is best so far - doing variation - Classification 8
Preciplevel2$Precip_class8 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.5,5,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class8 <- as.factor(Preciplevel2$Precip_class8)
summary(Preciplevel2$Precip_class8) #Trace-224   Light-91, Mod-9,Heavy-10,Very H-4
head(Preciplevel2) 

class.freq8 <- table(Preciplevel2$Precip_class8)
h1 <- barplot(class.freq8, ylim=c(0, max(class.freq8) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq8+1, class.freq8, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Preciplevel2)
plot(tree) #splits into 3 - same as Class 6

#binary
tree <- ctree(Precip_class8 ~ Date_bin, data = Preciplevel2)
plot(tree)


tree <- ctree(Precip_class8~Precip_avg.mm., data = Preciplevel2)
plot(tree)#split in 4 rather than 5

#Classification 9
Preciplevel2$Precip_class9 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.5,1,5,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class9 <- as.factor(Preciplevel2$Precip_class9)
summary(Preciplevel2$Precip_class9) #Trace-224   Light-33, Mod-58,Heavy-19,Very H-4
head(Preciplevel2) 

class.freq9 <- table(Preciplevel2$Precip_class9)
h1 <- barplot(class.freq9, ylim=c(0, max(class.freq9) + 15), main = 'Precipitation Classification', col = 'red')
text(h1,class.freq9+1, class.freq9, adj=c(0.5, -0.5))

tree <- ctree(Precip_class9~Days, data = Preciplevel2)
plot(tree) #one node

#binary
tree <- ctree(Precip_class9 ~ Date_bin, data = Preciplevel2)
plot(tree)

tree <- ctree(Precip_class9~Precip_avg.mm., data = Preciplevel2)
plot(tree)#Splits into 5

#Classification 10
Preciplevel2$Precip_class10 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.1,0.5,1,10,20,100), labels= c('Trace','Very Light' ,'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class10 <- as.factor(Preciplevel2$Precip_class10)
summary(Preciplevel2$Precip_class10) #Trace-132, VL-92   Light-33, Mod-67,Heavy-10,Very H-4
head(Preciplevel2)

class.freq10 <- table(Preciplevel2$Precip_class10)
h1 <- barplot(class.freq10, ylim=c(0, max(class.freq10) + 15), main = 'Precipitation Classification 10', col = 'blue')
text(h1,class.freq10+1, class.freq10, adj=c(0.5, -0.5))

tree <- ctree(Precip_class10~Days, data = Preciplevel2)
plot(tree) #Still splits into two

#binary
tree <- ctree(Precip_class10 ~ Date_bin, data = Preciplevel2)
plot(tree)

#Classification11
Preciplevel2$Precip_class6 <- cut(Preciplevel2$Precip_avg.mm., br = c(-1,0.5,1,5,10,20,100), labels= c('Trace','Very Light', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel2$Precip_class6 <- as.factor(Preciplevel2$Precip_class6)
summary(Preciplevel2$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Preciplevel2)

class.freq6 <- table(Preciplevel2$Precip_class6)
h1 <- barplot(class.freq6, ylim=c(0, max(class.freq6) + 15), main = 'Expanded Precipitation Classification', col = 'light blue')
text(h1,class.freq6+1, class.freq6, adj=c(0.5, -0.5))

tree <- ctree(Precip_class6~Days, data = Preciplevel2)
plot(tree) #one node

#binary
tree <- ctree(Precip_class6 ~ Date_bin, data = Preciplevel2)
plot(tree)

#CHOSEN CLASSIFICATIONS
#Classification 1 - Initial Classification
Preciplevel$Precip_class <- cut(Preciplevel$Precip_avg.mm., br = c(-1,1,5,10,100), labels= c('Light','Moderate','Heavy','Very Heavy'))
Preciplevel$Precip_class <- as.factor(Preciplevel$Precip_class)
summary(Preciplevel$Precip_class) # Light-257, Mod-58,Heavy-9,Very H-14
head(Preciplevel)
#Plot of Data
ggplot(Preciplevel, aes(x=Precip_class)) + 
  geom_bar(stat = "count" , fill = "skyblue3") +
  theme_minimal()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Precipitaion Class")

#Classification 6 -  Expanded Classification
Preciplevel$Precip_class6 <- cut(Preciplevel$Precip_avg.mm., br = c(-1,0.5,1,10,20,100), labels= c('Trace', 'Light','Moderate','Heavy','Very Heavy'))
Preciplevel$Precip_class6 <- as.factor(Preciplevel$Precip_class6)
summary(Preciplevel$Precip_class6) #Trace-224   Light-33, Mod-67,Heavy-10,Very H-4
head(Preciplevel) 
#Plot of Data
ggplot(Preciplevel, aes(x=Precip_class6)) + 
  geom_bar(stat = "count" , fill = "aquamarine3") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  labs(x="Expanded Precipitaion Class")

#Correlations -
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#https://rpubs.com/hoanganhngo610/558925
#https://towardsdatascience.com/fishers-exact-test-in-r-independence-test-for-a-small-sample-56965db48e87
#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

#Contingency Table
#Initial Classification - Binary
Corr1 <- table(Preciplevel$Precip_class,Preciplevel$Date_bin)
Corr1

preciplot <- ggplot(Preciplevel) +
  aes(x = Date_bin, fill = Precip_class) +
  geom_bar() +
  scale_fill_brewer(palette="Set2")+
  theme_minimal() +
  labs(y = 'Count', x= 'DustDay (Binary)', fill = "Precip Class")
  #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot
tt <- ttheme_default(core = list(padding=unit(c(20,4), "mm")))
Corrtab <- tableGrob(Corr1, theme=tt)
grid.arrange(preciplot, Corrtab,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))
#4-Day
Corr2 <- table(Preciplevel$Precip_class,Preciplevel$Days)
Corr2

preciplot2 <- ggplot(Preciplevel) +
  aes(x = Days, fill = Precip_class) +
  geom_bar() +
  scale_fill_brewer(palette="Set2")+
  theme_minimal()+
  labs(y = 'Count', x= 'DustDay (4-Day)', fill = "Precip Class")
  #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot2
tt <- ttheme_default()
Corrtab2 <- tableGrob(Corr2, theme=tt)
grid.arrange(preciplot2, Corrtab2,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

#Expanded Classification - Binary
Corr3 <- table(Preciplevel$Precip_class6,Preciplevel$Date_bin)
Corr3

preciplot3 <- ggplot(Preciplevel) +
  aes(x = Date_bin, fill = Precip_class6) +
  geom_bar() +
  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+
  labs(y = 'Count', x= 'DustDay (Binary)', fill = "Precip Class")
  #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot3
tt <- ttheme_default(core = list(padding=unit(c(20,4), "mm")))
Corrtab3 <- tableGrob(Corr3, theme=tt)
grid.arrange(preciplot3, Corrtab3,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

#4-Day
Corr4 <- table(Preciplevel$Precip_class6,Preciplevel$Days)
Corr4
#plot(Corr4, col('light blue'))
preciplot4 <- ggplot(Preciplevel) +
  aes(x = Days, fill = Precip_class6) +
  geom_bar() +
  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+
  labs(y = 'Count', x= 'DustDay (4-Day)', fill = "Precip Class")
  #geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
preciplot4
tt <- ttheme_default()
Corrtab4 <- tableGrob(Corr4, theme=tt)
grid.arrange(preciplot4, Corrtab4,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))
#Plots
grid.arrange(preciplot, preciplot2, Corrtab,Corrtab2,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

grid.arrange(preciplot3, preciplot4, Corrtab3,Corrtab4,
             nrow=2,
             as.table=TRUE,
             heights = c(3, 1))

#Chi-Square Test
library(lsr) #Cramer's
#Initial Classification -Binary
chisq.test(Preciplevel$Precip_class,Preciplevel$Date_bin)
chisq.test(Corr1)#same
# Warning produced
chisq.test(Corr1)$observed
chisq.test(Corr1)$expected
chisq.test(Corr1)$residuals
#cramersV(Preciplevel$Precip_class,Preciplevel$Date_bin)
# Some expected less than 5
fisher.test(Preciplevel$Precip_class,Preciplevel$Date_bin)
fisher.test(Corr1)

#4-Day
chisq.test(Preciplevel$Precip_class,Preciplevel$Days)
chisq.test(Corr2)
#cramersV(Preciplevel$Precip_class,Preciplevel$Days)
fisher.test(Preciplevel$Precip_class,Preciplevel$Days,workspace=2e8)
fisher.test(Corr2,workspace=2e8)
fisher.test(Corr2,simulate.p.value=TRUE,B=1e7)

#Expanded Classification-Binary
chisq.test(Preciplevel$Precip_class6,Preciplevel$Date_bin)
chisq.test(Corr3)
#cramersV(Preciplevel$Precip_class,Preciplevel$Date_bin)
fisher.test(Preciplevel$Precip_class6,Preciplevel$Date_bin)
fisher.test(Corr3)

#4-Day
chisq.test(Preciplevel$Precip_class6,Preciplevel$Days)
chisq.test(Corr4)
#cramersV(Preciplevel$Precip_class,Preciplevel$Days)
fisher.test(Preciplevel$Precip_class6,Preciplevel$Days, simulate.p.value=TRUE,B=1e7)
fisher.test(Corr4, simulate.p.value=TRUE,B=1e7)

#plotting residuals from chi-squared
chisq.test(Corr1)$residuals
chisq.test(Corr2)$residuals
chisq.test(Corr3)$residuals
chisq.test(Corr4)$residuals

par(mfrow = c(2,2))
corrplot(chisq.test(Corr1)$residuals,
         is.cor = FALSE, tl.col = "black", tl.srt = 45)
corrplot(chisq.test(Corr3)$residuals, is.cor = FALSE, 
         tl.col = "black", tl.srt = 45)
corrplot(chisq.test(Corr2)$residuals,
         is.cor = FALSE, tl.col = "black", tl.srt = 45)
corrplot(chisq.test(Corr4)$residuals, is.cor = FALSE, 
         tl.col = "black", tl.srt = 45)

#Spearman's and Kendall's Rank Correlation
#Initial Classification
PrecipCorr1 <- data.frame(PrecipClass = as.numeric(Preciplevel$Precip_class),
                          PrecipBin = as.numeric(Preciplevel$Date_bin),
                          PrecipDays = as.numeric(Preciplevel$Days))
head(PrecipCorr1)
comp<- data.frame (Num = PrecipCorr1$PrecipClass, Class=Preciplevel$Precip_class)
comp

comp2<- data.frame (Num = PrecipCorr1$PrecipDays, Class=Preciplevel$Days)
comp2

comp3<- data.frame (Num = PrecipCorr1$PrecipBin, Class=Preciplevel$Date_bin)
comp3

datacorS <- cor(PrecipCorr1, method = "spearman")
datacorS

datacorK <- cor(PrecipCorr1, method = "kendall")
datacorK

#Expanded Classification
PrecipCorr6 <- data.frame(PrecipClass = as.numeric(Preciplevel$Precip_class6),
                          PrecipBin = as.numeric(Preciplevel$Date_bin),
                          PrecipDays = as.numeric(Preciplevel$Days))
head(PrecipCorr6)
comp4<- data.frame (Num = PrecipCorr6$PrecipClass, Class=Preciplevel$Precip_class6)
comp4

comp5<- data.frame (Num = PrecipCorr6$PrecipDays, Class=Preciplevel$Days)
comp5

comp6<- data.frame (Num = PrecipCorr6$PrecipBin, Class=Preciplevel$Date_bin)
comp6

datacorS2 <- cor(PrecipCorr6, method = "spearman")
datacorS2

datacorK2 <- cor(PrecipCorr6, method = "kendall")
datacorK2

#Using rcorr -spearman - to get both correlation coeff and p-value
library("Hmisc") 
library(corrplot)
#Initial
scorr <- rcorr(as.matrix(PrecipCorr1), type = 'spearman') 
scorr
scorr$r #same as cor()
datacorS
scorr$P
corrplot(scorr$r)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corrs <- flattenCorrMatrix(scorr$r, scorr$P)
colnames(corrs) <- c('Row', 'Column', 'Correlation', 'p-value')
corrs

tt <- ttheme_default()
Corrs <- tableGrob(corrs, theme=tt)

par(mfrow = c(2,1))
corrplot(scorr$r)
grid.arrange(Corrs,
             nrow=2,
             as.table=TRUE,
             heights = c(2, 1))
corrplot(scorr$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Expanded Classification
scorr6 <- rcorr(as.matrix(PrecipCorr6), type = 'spearman') 
scorr6
scorr6$r #same as cor()
datacorS2
scorr6$P
corrplot(scorr6$r)

corrs6 <- flattenCorrMatrix(scorr6$r, scorr6$P)
colnames(corrs6) <- c('Row', 'Column', 'Correlation', 'p-value')
corrs6

tt <- ttheme_default()
Corrs6 <- tableGrob(corrs6, theme=tt)

par(mfrow = c(2,1))
corrplot(scorr6$r)
grid.arrange(Corrs6,
             nrow=2,
             as.table=TRUE,
             heights = c(2, 1))
corrplot(scorr6$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Decision Tree - https://ademos.people.uic.edu/Chapter24.html
head(Preciplevel)

set.seed(1000)
#Creating training set of 70% of data
rand <- sample(1:nrow(Preciplevel), 0.7 * nrow(Preciplevel))
Precip_train <- Preciplevel[rand,] 
head(Precip_train)
summary(Precip_train)

#Creating test set of 30% of data
Precip_test <- Precip_train[-rand,]
head(Precip_test)
summary(Precip_test)

#Initial Classification - 4-Day
tree_train1 <- ctree(Precip_class~Days, data = Precip_train)
plot(tree_train1)

Precip_pred1 <- predict(tree_train1,Precip_test)
Precip_pred1
tab1 <- table(Precip_pred1,Precip_test$Precip_class)
tab1

#https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

accuracy(tab1) #74.28571

#Expanded Classification - 4-Day
tree_train6 <- ctree(Precip_class6~Days, data = Precip_train)
plot(tree_train6)

Precip_pred6 <- predict(tree_train6,Precip_test)
Precip_pred6 

tab6 <- table(Precip_pred6,Precip_test$Precip_class6)
tab6

accuracy(tab6)#64.28571

#Initial Classification - Binary
treeb1 <- ctree(Precip_class ~ Date_bin, data = Precip_train)
plot(treeb1)

Precip_predb1 <- predict(treeb1,Precip_test)
Precip_predb1

tabb1 <- table(Precip_predb1,Precip_test$Precip_class)
tabb1

accuracy(tabb1) #74.28571

#Expanded Classification - Binary
treeb6 <- ctree(Precip_class6 ~ Date_bin, data = Precip_train)
plot(treeb6)

Precip_predb6 <- predict(treeb6,Precip_test)
Precip_predb6

tabb6 <- table(Precip_predb6,Precip_test$Precip_class6)
tabb6

accuracy(tabb6) #64.28571






